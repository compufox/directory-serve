;;;; package.lisp

(defpackage #:directory-serve
  (:use #:cl #:with-user-abort)
  (:import-from :str
		:concat
		:split
		:containsp
		:replace-all)
  (:import-from :uiop
		:subdirectories
		:directory-files
		:directory-exists-p
		:file-exists-p
		:getcwd)
  (:import-from :clack
		:clackup)
  (:import-from :cl-markup
		:raw
		:markup)
  (:import-from :alexandria
		:lastcar))
(in-package :directory-serve)

(defvar *cwd* (namestring (getcwd)))
(defvar *handler* nil)
(defparameter *app*
  (lambda (env)
    (generate-page (getf env :request-uri))))
