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
  (:export :serve
	   :stop))
(in-package :directory-serve)

(defparameter +container-style+
  "
flex-flow:row nowrap;
display:flex;
justify-content:space-around;
height:100%;
margin-left:15%;
margin-right:15%;")
(defparameter +column-style+
  "
padding-left: 2em;
padding-right: 2em;
padding-bottom: 2em;
border: 3px solid #797d7f;
flex-flow:column nowrap;
display:flex;
overflow:auto;
width: 69%;
background-color:#c4a4bc")


(defvar *cwd* (namestring (getcwd)))
(defvar *handler* nil)
(defparameter *app*
  (lambda (env)
    (generate-page (getf env :path-info))))
