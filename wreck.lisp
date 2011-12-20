

(defvar **prog-lisp-dir** "c:/prog/wreck/")

(defun prog-lisp-dir (dir)
  (concatenate 'string **prog-lisp-dir** dir))

(load (concatenate 'string (prog-lisp-dir "asdf/") "asdf.lisp"))

(defun pl (path &optional sym)
  (push (prog-lisp-dir path) asdf:*central-registry*)
  ; (asdf:operate 'asdf:load-op sym)
  )
(defun ap (package) (asdf:operate 'asdf:load-op package))

(defmacro load-lutils ()
  `(load (prog-lisp-dir "lutils.lisp")))


(pl "hunchentoot/" :hunchentoot)
(pl "bordeaux-threads/" :bordeaux-threads)
(pl "Alexandria/" :alexandria)
(pl "usocket/" :usocket)
(pl "trivial-backtrace/" :trivial-backtrace)
(pl "rfc2388/" :rfc2388)
(pl "md5-1.8.5/" :md5)
(pl "cl-plus-ssl-cl-plus-ssl/" :cl+ssl)
(pl "trivial-garbage_0.19/" :trivial-garbage)
(pl "flexi-streams-1.0.7/" :flexi-streams)
(pl "trivial-gray-streams-2006-09-16/" :trivial-gray-streams)
(pl "cffi_0.10.5/" :cffi)
(pl "babel_0.3.0/" :babel)
(pl "trivial-features_0.6/" :trivial-features)
(pl "cl-ppcre-2.0.3/" :cl-ppcre)
(pl "cl-fad-0.6.4/" :cl-fad)
(pl "cl-base64-3.3.3/" :cl-base64)
(pl "chunga-1.1.1/" :chunga)
(pl "heresy/" :heresy)
(pl "drakma-1.2.3/" '#:drakma)
(pl "puri-1.5.1/" 'puri)

(ap :hunchentoot)
(ap :heresy)
(ap :drakma)


(use-package '(:hunchentoot :heresy))

(defvar server-already-running nil)
(defparameter **port** 82)


(defun start-server ()
  (when (not server-already-running)
    (setf server-already-running (make-instance 'easy-acceptor :port **port**))

    (setf (slot-value server-already-running 'hunchentoot::access-log-destination) nil)
    (setf (slot-value server-already-running 'hunchentoot::message-log-destination) nil)
    
    (setf *dispatch-table*
        (append
         (list
          (lambda (request)
            (let* ((request-uri (request-uri*))
                   (url-split (to-list (map/ #'to-string (filter/ #'non-null/ (split-down-on-test/ (curried #'eql #\/) request-uri))))))
              (lambda (&rest rest)
                (let ((stream (send-headers)))
                 ; (loop for i from 1 to 10 do (progn (format stream "~A" i) (terpri stream)))
                  (loop for i from 1 do
                    (progn
                     (loop for i from 1 to 200 do (write-byte (random 256) stream))
                     (write-byte 13 stream)
                     (write-byte 10 stream)
                     ))
                  
                  )))))

         (list
          ; seems it was removed #'default-dispatcher
          )))

    (start server-already-running)))



(defparameter **die** nil)

(defparameter **errors** 0)
(defparameter **successes** 0)

(defun start-stress-test ()
  (loop for thread from 1 to 100 do
    (let ((thread thread))
      (sleep 0.01)
      (bordeaux-threads:make-thread
       (lambda ()
         (loop for i from 1
               until **die** do
                 (progn
                   (handler-case
                       (unless **die**
                         (close (drakma:http-request (format nil "http://127.0.0.1:~A" **port**) :method :get :want-stream t))
                         (incf **successes**))
                     (error (err)
                       ;(push (format nil "Thread ~A Send ~A failed" thread i) **errors**)
                       (incf **errors**)
                       )))))
       :name (format nil "Stress Test ~A" thread)))))


(defun run ()
  (start-server)
  (start-stress-test))


(defun get-progress ()
  (format nil "Successes: ~A, Errors: ~A" **successes** **errors**))

(defun show-progress ()
  (print (get-progress)))

(run)


