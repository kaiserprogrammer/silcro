(defpackage :silcro
  (:use :cl :stampede)
  (:export
   :s-get
   :s-post
   :s-put
   :s-delete
   :redirect-to
   :get-id
   :param
   :s-file))
(in-package :silcro)

(defmacro s-method (method)
  `(defmacro ,(intern (string-upcase (concatenate 'string "s-" method))) (&rest spec)
     `(defroute ,(caar spec) ,,(string-upcase method) ,(concatenate 'string "^" (cadar spec) "$")
                (lambda (,(intern (string-upcase "req")) ,(intern (string-upcase "res")))
                  (let ((req ,(intern (string-upcase "req")))
                        (res ,(intern (string-upcase "res"))))
                    (declare (ignorable req res))
                    ,@(cdr spec))))))

(s-method "get")
(s-method "post")
(s-method "put")
(s-method "delete")

(defmacro redirect-to (url)
  `(progn (setf (cdr (assoc :status res)) 302)
          (nconc res (list (cons "Location" ,url)))
          ""))

(defmacro get-id ()
  `(parse-integer (param :id)))

(defmacro param (key)
  `(cdr (assoc ,key (cdr (assoc :params req)) :test #'equal)))

(defmacro s-file (server file)
  `(s-get (,server ,file)
          (setf (cdr (assoc "Content-Type" res :test #'string=)) "text/css")
          ,(alexandria:read-file-into-string (lisperati:relative-file file))))
