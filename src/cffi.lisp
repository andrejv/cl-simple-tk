;;;
;;; Copyright (c) 2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.
;;;

(in-package #:simple-tk)

(defvar *debug* nil
  "Prints wish communication to screen.")

(defvar *current-interp* nil
  "Current wish interpreter.")

(defvar *event-table* nil
  "Hastable mapping window paths to event handlers.")

(defconstant +tcl-ok+ 0
  "Functions return TCL_OK on success.")

(defconstant +tcl-global-only+ 1
  "tcl-set-variable/tcl-get-variable namespace (TCL_GLOBAL_ONLY).")

(define-foreign-library tcl
  (:darwin (:framework "Tcl"))
  (:windows (:or "tcl86.dll" "tcl85.dll"))
  (:unix (:or "libtcl8.6.so" "libtcl8.5.so" "libtcl.so"))
  (t (:default "libtcl")))

(define-foreign-library tk
  (:darwin (:framework "Tk"))
  (:windows (:or "tk86.dll" "tk85.dll"))
  (:unix (:or "libtk8.6.so" "libtk8.5.so" "libtk.so"))
  (t (:default "libtk")))

(use-foreign-library tcl)
(use-foreign-library tk)

(defcfun ("Tcl_CreateInterp" tcl-create-interp)
    :pointer)
(defcfun ("Tcl_DeleteInterp" tcl-delete-interp)
    :void
  (interp :pointer))
(defcfun ("Tcl_Init" tcl-init)
    :int
  (interp :pointer))
(defcfun ("Tk_Init" tk-init)
    :int
  (interp :pointer))
(defcfun ("Tk_MainLoop" tk-main-loop)
    :void)
(defcfun ("Tk_GeometryRequest" tk-geometry-request)
    :void
  (tkwin :pointer) (x :int) (y :int))
(defcfun ("Tk_MainWindow" tk-main-window)
    :pointer
  (interp :pointer))

(defcfun ("Tcl_Eval" tcl-eval)
    :int
  (interp :pointer) (script (:string :encoding :utf-8)))
(defcfun ("Tcl_GetStringResult" tcl-get-string-result)
    (:string :encoding :utf-8)
  (interp :pointer))

(defcfun ("Tcl_GetVar" tcl-get-var)
    (:string :encoding :utf-8)
  (interp :pointer) (var :string :encoding :utf-8) (flags :int))
(defcfun ("Tcl_SetVar" tcl-set-var)
    (:string :encoding :utf-8)
  (interp :pointer) (var :string) (val :string) (flags :int))

(defcfun ("Tcl_CreateCommand" tcl-create-command)
    :int
  (interp :pointer) (name :string :encoding :utf-8) (fun :pointer)
  (data :pointer) (del_proc :pointer))

(defcallback call-lisp
    :int
  ((client_data :pointer)
   (interp :pointer)
   (argc :int)
   (argv :pointer))
  (declare (ignore client_data))
  (handler-case
      (let ((args)
	    (*current-interp* interp))
	(dotimes (i argc)
	  (push (foreign-string-to-lisp
		 (mem-aref argv :pointer i))
		args))
	(setf args (cdr (reverse args)))
	(let ((fun (gethash (car args) *event-table*)))
	  (when fun
	    (if (cdr args)
		(funcall fun (cdr args))
		(funcall fun)))
	  (when *debug*
	    (format t "~{~a~^ ~}~%" (reverse args)))))
    (error (e)
      (format t "TCL callback error: ~a~%" e)))
  0)

(defun init-session ()
  "Setup a tcl/tk session."
  (let ((interp (tcl-create-interp)))

    (when (/= (tcl-init interp) +tcl-ok+)
      (error (format nil "TCL init error: ~a" (tcl-get-string-result interp))))

    (when (/= (tk-init interp) +tcl-ok+)
      (error (format nil "TK init error: ~a" (tcl-get-string-result interp))))

    (let ((main (tk-main-window interp)))
      (when (null-pointer-p main)
        (error (format nil "TK no main window: ~a" (tcl-get-string-result interp))))
      (tk-geometry-request main 200 200))
    
    (tcl-create-command interp
			"call_lisp"
			(callback call-lisp)
			(null-pointer) (null-pointer))
    interp))

(defun get-response (&rest s)
  "Sends the command cmd to wish and reads one line of response"
  (let ((cmd (apply #'format (cons nil s))))
    (when *debug*
      (format t "~a~%" cmd))
    (if (= (tcl-eval *current-interp* cmd) +tcl-ok+)
        (tcl-get-string-result *current-interp*)
        (progn
          (warn (format nil "TCL error: ~a~%" (tcl-get-string-result *current-interp*)))
          ""))))

(defun get-variable (var)
  "Gets the value of the variable VAR."
  (tcl-get-var *current-interp* var +tcl-global-only+))

(defun set-variable (var val)
  "Sets the value of VAR to VAL."
  (tcl-set-var *current-interp* (format nil "~a" var) (format nil "~a" val) +tcl-global-only+))

(defun send-command (&rest s)
  "Runs the command in the current interpreter."
  (let ((cmd (apply #'format (cons nil s))))
    (when *debug*
      (format t "~a~%" cmd))
    (when (/= (tcl-eval *current-interp* cmd) +tcl-ok+)
      (warn (format nil "TCL error: ~a~%" (tcl-get-string-result *current-interp*))))))

