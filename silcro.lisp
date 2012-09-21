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

(defmacro s-file (server file &optional url)
  (when (not url)
    (setf url (eval file)))
  `(s-get (,server ,url)
          (setf (cdr (assoc "Content-Type" res :test #'string=)) (mime-type ,file))
          ,(if (with-open-file (in (eval file))
                 (> 1000000 (file-length in)))
               (alexandria:read-file-into-string (eval file) :external-format :latin1)
               `(alexandria:read-file-into-string ,file :external-format :latin1))))

(defmacro s-dir (server dir)
  (let ((files))
    (cl-fad:walk-directory
     (eval `(lisperati:relative-file ,dir))
     (lambda (file)
       (push (princ-to-string file) files)))
    (cons 'progn
          (loop for file in files
             for url = (concatenate 'string "/" (subseq file (cl-ppcre:scan dir file)))
             collect `(s-file ,server ,file ,url)))))
