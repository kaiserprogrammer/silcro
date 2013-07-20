(in-package :silcro)

(defmacro s-method (method)
  `(defmacro ,(intern (string-upcase (concatenate 'string "s-" method))) ((server regex) &body body)
     `(defroute ,server ,,(string-upcase method) ,(concatenate 'string "^" regex "$")
                (lambda (,(intern (string-upcase "req")) ,(intern (string-upcase "res")))
                  (let ((req ,(intern (string-upcase "req")))
                        (res ,(intern (string-upcase "res"))))
                    (declare (ignorable req res))
                    (let ,(loop for param in (mapcar
                                              (lambda (param) (string-upcase (subseq param 1)))
                                              (cl-ppcre:all-matches-as-strings ":\\w+" regex))
                             collect (list (intern param)
                                           `(param ,(intern param :keyword))))
                      ,@body))))))

(s-method "get")
(s-method "post")
(s-method "put")
(s-method "delete")

(defmacro redirect-to (url)
  `(progn (setf (cdr (assoc :status res)) 302)
          (nconc res (list (cons "Location" ,url)))
          (response-written)))

(defmacro get-id ()
  `(parse-integer (param :id)))

(defmacro param (key)
  `(cdr (assoc ,key (cdr (assoc :params req)) :test #'equal)))

(defmacro s-file (server file &optional url)
  (when (not url)
    (setf url (eval file)))
  `(s-get (,server ,url)
          (setf (cdr (assoc "Content-Type" res :test #'string=)) ,(or (mime-type file)
                                                                     "text/plain"))
          ,(let ((file-length (with-open-file (in (eval file))
                                (file-length in))))
                (if (> 1000000 file-length)
                    `(when-modified ,file
                       (set-content-length ,(with-open-file (in file)
                                                            (file-length in)))
                       (set-last-modification-date ,(rfc1123-write-date file))
                       ,(alexandria:read-file-into-string (eval file) :external-format :latin1))
                    `(progn
                       (content-length ,file)
                       (last-modification-date ,file)
                       (flush-headers)
                       (write-file ,file (alexandria:assoc-value res :stream))
                       (response-written))))))

(defmacro when-modified (file &body body)
  `(let ((timestring (assoc-value req "If-Modified-Since" :test 'equal)))
     (if (and timestring
              (>= (parse-rfc-1123-date timestring)
                  (file-write-date ,file)))
         (progn (setf (cdr (assoc :status res)) 304)
                "")
         (progn ,@body))))

(defun rfc1123-write-date (path)
  (local-time:to-rfc1123-timestring
   (local-time:universal-to-timestamp
    (file-write-date path))))

(defmacro set-content-length (length)
  `(nconc res (list (cons "Content-Length"
                          ,length))))

(defmacro content-length (file)
  `(with-open-file (in ,file)
     (set-content-length (file-length in))))

(defmacro set-last-modification-date (date)
  `(nconc res (list (cons "Last-Modified"
                          ,date))))

(defmacro last-modification-date (path)
  `(set-last-modification-date ,(rfc1123-write-date path)))

(defun write-file (path stream &key (buffer-size 4096) (external-format :latin1))
  (with-open-file (filestream path :external-format external-format :element-type '(unsigned-byte 8))
    (let ((buffer (make-array buffer-size))
          (content-left (file-length filestream)))
      (do ()
          ((not (> content-left 0)))
        (let ((to-read (if (> content-left buffer-size)
                           buffer-size
                           content-left)))
          (read-sequence buffer filestream :end to-read)
          (write-sequence buffer stream :end to-read)
          (decf content-left to-read))))))

(defmacro s-dir (server dir &key (dirs-in-name 1))
  (let ((files)
        (dir (princ-to-string (truename dir))))
    (cl-fad:walk-directory
     dir
     (lambda (file)
       (push (princ-to-string file) files)))
    (cons 'progn
          (loop for file in files
             for url = (subseq file (cl-ppcre:scan (format nil "(/[^/]+){~a,~a}$" (1+ dirs-in-name) (1+ dirs-in-name)) file))
             collect `(s-file ,server ,file ,url)))))

(defmacro write-to-client (text)
  `(write-response text (assoc-value res :stream)))

(defmacro response-written ()
  `(set-response-written res))

(defmacro flush-headers ()
  `(write-headers res))
