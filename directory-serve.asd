;;;; directory-serve.asd

(asdf:defsystem #:directory-serve
  :description "Describe directory-serve here"
  :author "ava fox"
  :license  "NPLv1+"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop #:clack #:str
	       #:cl-markup #:with-user-abort
	       #:trivial-mimes)
  :components ((:file "package")
               (:file "directory-serve"))
  :build-operation "build-op"
  :build-pathname #-Win32 "bin/directory-serve"
                  #+Win32 "bin/directory-serve.exe"
  :entry-point "directory-serve::main")
