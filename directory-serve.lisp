;;;; directory-serve.lisp

(in-package #:directory-serve)
(declaim (inline remove-cwd clean-path))

(defun generate-page (path)
  (let ((path (if (= (length path) 1)
		  "."
		  (subseq path 1))))
    (if (directory-exists-p path)
	(serve-directory path)
	(if (file-exists-p path)
	    (serve-file path)
	    (serve-error)))))

(defun serve-directory (path)
  (let ((directories (mapcar #'remove-cwd
			     (mapcar #'namestring (subdirectories path))))
	(files (mapcar #'remove-cwd
		       (mapcar #'namestring (directory-files path)))))
    (list 200 '(:content-type "text/html")
	  (list (markup
		 (:div :style "padding-left: 25%; padding-right: 25%"
		   (raw (apply #'concat (loop for dir in directories
					      collect (markup (:a :href dir
								  (clean-path path dir))
							      (:br)))))
		   (raw (apply #'concat (loop for file in files
					      collect (markup (:a :href file
								  (clean-path path file))
							      (:br)))))))))))

(defun remove-cwd (path)
  (replace-all *cwd* "" path))
  
(defun clean-path (dir path)
  (if (string= "." dir)
      path
      (replace-all dir "" path)))


(defun serve-file (file)
  (let ((mime (mimes:mime file))
	(response nil))
    (setf response
	  (if (containsp "text" mime)
	      (str:from-file file)
	      (with-open-file (in file :element-type '(unsigned-byte 8))
		(loop for b in (read-byte in nil nil)
		      while b
		      collect b))))
    (list 200 (list :content-type mime) (list response))))

(defun serve-error ()
  (list 404 '(:content-type "text/html")
	(list (markup (:h1 "404 - Not Found")))))

(defun main ()
  (flet ((find-thread (th)
	   (search "hunchentoot" (bt:thread-name th))))
    (handler-case
	(with-user-abort
	  (when *handler* (clack:stop *handler*))
	  (setf *handler* (clackup *app*))
	  (bt:join-thread (find-if #'find-thread (bt:all-threads))))
      
      (user-abort ())
      (error (e)
	(format t "error: ~a~%" e)))))

(defun stop ()
  "stop serving directories"
  (when *handler*
    (clack:stop *handler*)
    (setf *handler* nil)))

(defun serve (&optional (dir (getcwd)))
  "serve a directory

if DIR is provided, start with that directory as the root
if DIR is not provided, defaults to current directory"

  ;; should offer a way to restart
  (when *handler*
    (error "already serving a directory"))

  ;;
  (if (directory-exists-p dir)
      (setf *cwd* (concat (namestring dir) "/")
	    *handler* (clackup *app*))
      (error "directory does not exist")))
