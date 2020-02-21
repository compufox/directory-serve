;;;; directory-serve.lisp

(in-package #:directory-serve)
(declaim (inline remove-cwd clean-path))

(defun generate-page (path)
  "generates a page for PATH

if PATH is a directory, we list the directory
if PATH is a file, we try and serve the file

otherwise we return a 404 page"
  (let ((path (if (= (length path) 1)
		  *cwd*
		  (namestring (merge-pathnames (subseq path 1)
					       *cwd*)))))
    (if (directory-exists-p path)
	(serve-directory path)
	(if (file-exists-p path)
	    (serve-file path)
	    (serve-error)))))

(defun serve-directory (path)
  "generates a list of directories and files from PATH"
  (let ((directories (mapcar #'namestring (subdirectories path)))
	(files (mapcar #'namestring (directory-files path))))
    (list 200 '(:content-type "text/html")
	  (list (markup
		 (:head
		   (:meta :http-equiv "Content-Type" :content "text/html; charset=utf-8")
		   (:title (remove-cwd path)))
		 (:div :style "padding-left: 25%; padding-right: 25%"
		   (raw (unless (string= path *cwd*)
			  (markup (:a :href ".." (concat (string #\LEFTWARDS_ARROW_WITH_TIP_UPWARDS)
							 " PARENT"))
				  (:br))))
		   (raw (apply #'concat (loop for dir in directories
					      collect (markup (:a :href (clean-path path dir)
								  (clean-path path dir))
							      (:br)))))
		   (raw (apply #'concat (loop for file in files
					      collect (markup (:a :href (clean-path path file)
								  (clean-path path file))
							      (:br)))))))))))

(defun remove-cwd (path)
  "removes cwd from PATH"
  (clean-path *cwd* path))
  
(defun clean-path (dir path)
  "removes DIR from PATH"
  (replace-all dir "" path))

(defun serve-file (file)
  "serves up a file

if FILE is text we return the full file as text
if FILE is not text we try and just serve up a list of bytes"
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
  "serves a 404 page"
  (list 404 '(:content-type "text/html")
	(list (markup (:h1 "404 - Not Found")))))

(defun main ()
  "binary entry point"
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
