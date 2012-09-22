(defpackage :silcro
  (:use :cl :stampede :alexandria :anaphora)
  (:export
   :s-get
   :s-post
   :s-put
   :s-delete
   :redirect-to
   :get-id
   :param
   :s-file
   :s-dir
   :set-cookie
   :get-cookie
   :set-session
   :get-session
   :write-to-client
   :response-written
   :flush-headers))

