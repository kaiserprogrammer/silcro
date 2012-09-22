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
          (response-written)))

(defmacro get-id ()
  `(parse-integer (param :id)))

(defmacro param (key)
  `(cdr (assoc ,key (cdr (assoc :params req)) :test #'equal)))

(defmacro s-file (server file &optional url)
  (when (not url)
    (setf url (eval file)))
  `(s-get (,server ,url)
          (setf (cdr (assoc "Content-Type" res :test #'string=)) (or (mime-type ,file)
                                                                     "text/plain"))
          ,(if (with-open-file (in (eval file))
                 (> 1000000 (file-length in)))
               (alexandria:read-file-into-string (eval file) :external-format :latin1)
               `(progn
                  (flush-headers)
                  (write-file ,file (alexandria:assoc-value res :stream))
                  (response-written)))))

(defun write-file (path stream &key (buffer-size 4096) (external-format :latin1))
  (declare (optimize (debug 3)))
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

(defmacro s-dir (server dir)
  (let ((files))
    (cl-fad:walk-directory
     dir
     (lambda (file)
       (push (princ-to-string file) files)))
    (cons 'progn
          (loop for file in files
             for url = (concatenate 'string "/" (subseq file (cl-ppcre:scan dir file)))
             collect `(s-file ,server ,file ,url)))))

(defmacro write-to-client (text)
  `(let ((stream (assoc-value res :stream)))
     (format stream "~a" ,text)))

(defmacro response-written ()
  `(set-response-written res))

(defmacro flush-headers ()
  `(write-headers res))
