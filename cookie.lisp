;;; Copyright (c) 2004-2010, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;; * Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.

;;; * Redistributions in binary form must reproduce the above
;;; copyright notice, this list of conditions and the following
;;; disclaimer in the documentation and/or other materials
;;; provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :silcro)

(defvar +day-names+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defvar +month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defclass cookie ()
  ((name :initarg :name
         :reader cookie-name
         :type string
         :documentation "The name of the cookie - a string.")
   (value :initarg :value
          :accessor cookie-value
          :initform ""
          :documentation "The value of the cookie. Will be URL-encoded
when sent to the browser.")
   (expires :initarg :expires
            :initform nil
            :accessor cookie-expires
            :documentation "The time \(a universal time) when the
cookie expires \(or NIL).")
   (path :initarg :path
         :initform nil
         :accessor cookie-path
         :documentation "The path this cookie is valid for \(or NIL).")
   (domain :initarg :domain
           :initform nil
           :accessor cookie-domain
           :documentation "The domain this cookie is valid for \(or NIL).")
   (secure :initarg :secure
           :initform nil
           :accessor cookie-secure
           :documentation "A generalized boolean denoting whether this
cookie is a secure cookie."))
  (:documentation "Each COOKIE objects describes one outgoing cookie."))

(defun set-cookie* (cookie res)
  "Adds the COOKIE object COOKIE to the outgoing cookies of the
REPLY object REPLY. If a cookie with the same name
\(case-sensitive) already exists, it is replaced."
  (let* ((name (cookie-name cookie))
         (place (assoc name res :test #'string=)))
    (cond
      (place
        (setf (cdr place) cookie))
      (t
       (nconc res (list (cons "Set-Cookie" (stringify-cookie cookie))))
        cookie))))

(defun set-cookie% (name &key (value "") expires path domain secure res)
  "Creates a cookie object from the parameters provided and adds
it to the outgoing cookies of the REPLY object REPLY. If a cookie
with the name NAME \(case-sensitive) already exists, it is
replaced."
  (set-cookie* (make-instance 'cookie
                              :name name
                              :value value
                              :expires expires
                              :path path
                              :domain domain
                              :secure secure)
               res))

(defmacro set-cookie (key value)
  `(set-cookie% ,key :value ,value :path "/" :res res))

(defmacro get-cookie (key)
  `(second (cl-ppcre:split "="
                           (find-if (lambda (kv) (string= ,key (first (cl-ppcre:split "=" kv))))
                                    (cl-ppcre:split "; " (cdr (assoc "Cookie" req :test #'string=)))))))

(defparameter *sessions* (make-hash-table :test #'equal))

(defun init-session-id ()
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 (ironclad:ascii-string-to-byte-array (rfc-1123-date)))))

(defun construct-session-hash (session-id req)
  (concatenate 'string
               (princ-to-string (cdr (assoc :remote-host req)))
               session-id))

(defun init-session (key value res req)
  (let ((session-id (init-session-id)))
    (set-cookie "session" session-id)
    (setf (gethash (construct-session-hash session-id req)
                   *sessions*)
          (list (cons key value)))))

(defmacro set-session (key value)
  `(set-session% ,key ,value res req))

(defmacro get-session (key)
  `(get-session% ,key req))

(defun set-session% (key value res req)
  (let ((session-id (get-cookie "session")))
    (if (not session-id)
        (init-session key value res req)
        (let ((session (gethash (construct-session-hash session-id req) *sessions*)))
          (if session
              (let ((place (assoc key session :test #'equal)))
                (if place
                    (setf (cdr place) value)
                    (nconc session (list (cons key value)))))
              (init-session key value res req))))))

(defun get-session% (key req)
  (let ((session-id (get-cookie "session")))
    (when session-id
      (cdr (assoc key (gethash (construct-session-hash session-id req) *sessions*) :test #'equal)))))

(defun cookie-date (universal-time)
  "Converts UNIVERSAL-TIME to cookie date format."
  (and universal-time
       (rfc-1123-date universal-time)))

(defmethod stringify-cookie ((cookie cookie))
  "Converts the COOKIE object COOKIE to a string suitable for a
'Set-Cookie' header to be sent to the client."
  (format nil
          "~A=~A~:[~;~:*; expires=~A~]~:[~;~:*; path=~A~]~:[~;~:*; domain=~A~]~:[~;; secure~]"
          (cookie-name cookie)
          (cookie-value cookie)
          (cookie-date (cookie-expires cookie))
          (cookie-path cookie)
          (cookie-domain cookie)
          (cookie-secure cookie)))

(defun rfc-1123-date (&optional (time (get-universal-time)))
  "Generates a time string according to RFC 1123.  Default is current time."
  (multiple-value-bind
        (second minute hour date month year day-of-week)
      (decode-universal-time time 0)
    (format nil "~A, ~2,'0d ~A ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
            (svref +day-names+ day-of-week)
            date
            (svref +month-names+ (1- month))
            year
            hour
            minute
            second)))

(defun parse-rfc-1123-date (timestring)
  (encode-universal-time (parse-integer (subseq timestring 23 25))
                         (parse-integer (subseq timestring 20 22))
                         (parse-integer (subseq timestring 17 19))
                         (parse-integer (subseq timestring 5 7))
                         (or (loop for i from 0 below (length +month-names+)
                                  when (string= (subseq timestring 8 11) (svref +month-names+ i))
                                  return (1+ i))
                             1)
                         (parse-integer (subseq timestring 12 16))
                         0))

(defun iso-time (&optional (time (get-universal-time)))
  "Returns the universal time TIME as a string in full ISO format."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))
