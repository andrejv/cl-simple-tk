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

(in-package :simple-tk)

(defun bind-event (win ev fun)
  "Binds the event EV in window W with function FUN.

Function FUN accepts one argument EVT."
  (let ((id (string-downcase (format nil "~a.~e" (window-path win) ev))))
    (send-command "bind ~a ~a \{call_lisp ~a %x %y %A %W\}"
                  (window-path win) ev id)
    (setf (gethash id *event-table*) fun)))

(defun event-mouse-position (evt)
  "Returns the X and Y coordinate of the EVENT in a list.

EVT is the argument for the FUN argument to the BIND function."
  (let ((x (parse-integer (car evt)))
        (y (parse-integer (cadr evt))))
    (list x y)))

(defun event-key-code (evt)
  "Returns the key code of the event EVT.

EVT is the argument for the FUN argument to the BIND function."
  (caddr evt))

(defun event-window-path (evt)
  "Returns the path of the windows from which the EVT was generated.

EVT is the argument for the FUN argument to the BIND function."
  (cadddr evt))

(defun after (ms fun)
  "Calls FUN after MS miliseconds.

FUN should be afunction with no arguments. Returns the ID of the
callback."
  (let ((id (format nil "after~a" (next-id))))
    (setf (gethash id *event-table*)
          (lambda ()
            (funcall fun)
            (remhash id *event-table*)))
    (get-response "after ~a \{call_lisp ~a\}" ms id)))

(defun after-cancel (id)
  "Cancels the callback ID."
  (send-command "after cancel ~a" id))

(defun after-idle (fun)
  "Calls FUN as an idle callback.

FUN should be a function with no arguments. Returns the ID of the
callback."
  (let ((id (format nil "after~a" (next-id))))
    (setf (gethash id *event-table*)
          (lambda ()
            (funcall fun)
            (remhash id *event-table*)))
    (get-response "after idle \{call_lisp ~a\}" id)))