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

(defvar *window-counter* 0
  "Counts the number of windows created.")

(defvar *root-window* nil
  "The root window.")

(defvar *window-table* nil
  "The hashtable from window paths to windows.")

(defun next-id ()
  "Computes id numbers for unique window names."
  (incf *window-counter*))

(defun key-to-string (k)
  "Converts a keyword K to a downcase string."
  (string-downcase (string k)))

(defun option-to-string (o)
  "Converts an object O to a string representation for tcl interpreter."
  (cond ((stringp o) o)
        ((eq (type-of o) 'tcl-var) (tcl-var-name o))
        ((subtypep (type-of o) 'window) (window-path o))
        ((eq o t) "1")
        ((null o) "0")
        ((listp o) (format nil "~{~s~^ ~}" o))
        (t (format nil "~a" o))))

(defclass window ()
  ((name
    :initform (error "NAME should be prowided")
    :initarg :name
    :reader window-name
    :documentation "The tcl/tk name of the underlying class.")
   (tk-name
    :initform nil
    :initarg :tk-name
    :reader window-tk-name
    :documentation "The short description part of the pathname for current window.")
   (id
    :initform (next-id)
    :initarg :id
    :reader window-id
    :documentation "Unique number across all windows.")
   (string-id
    :initform ""
    :reader widow-string-id
    :documentation "Combination of TK-NAME and ID (can be used as the default textvariable).")
   (path
    :initform "."
    :reader window-path
    :documentation "Tcl/tk path for the window.")
   (parent
    :initarg :parent
    :initform *root-window*
    :reader window-parent
    :documentation "This window's parent.")
   (tk-init
    :initform nil
    :initarg :tk-init
    :documentation "Additional argument for the initialize function")
   (tk-define
    :initform t
    :initarg :tk-define
    :documentation "Should the window be defined.")))

(defmethod initialize-instance :after ((w window) &key)
  (with-slots (name tk-name id string-id path parent tk-init tk-define) w
    (when tk-define
      (setf string-id (format nil "~a~a" (or tk-name "stk") id))
      (if (and parent (string/= (window-path parent) "."))
          (setf path (format nil "~a.~a" (window-path parent) string-id))
          (setf path (format nil ".~a" string-id)))
      (send-command "~a ~a ~a" name path (or tk-init ""))
      (setf (gethash path *window-table*) w))))

(defun window-from-path (path)
  "Returns the window with path PATH."
  (gethash path *window-table*))

(defun bind-command (w fun)
   "Registers the command handler FUN for the window W.

FUN is a function with no arguments."
   (when (functionp fun)
     (send-command "~a configure -command {call_lisp ~a}" (window-path w) (window-path w))
     (setf (gethash (window-path w) *event-table*) fun)))

(defun configure (w option value)
  "Configures the windows W with OPTION set to VALUE."
  (send-command "~a configure -~a {~a}" (window-path w) (key-to-string option) (option-to-string value)))

(defun configure-window (w options)
  "Configures the window W according to OPTIONS.

Called from constructur functions."
  (loop for option on options by #'cddr do
       (let ((o (car option))
             (v (cadr option)))
         (case o
           ((:parent :orient :tk-name :tk-define)) ;; handled elsewhere
           (:command
            (bind-command w v))
           (:selected
            (setf (window-state w) v))
           (otherwise
            (configure w (key-to-string o) (option-to-string v)))))))

(defun window-identify (w x y)
  "Returns the name of the element at X,Y."
  (get-response "~a identify ~a ~a" (window-path w) x y))

(defun clipboard-clear ()
  "Clears the content of the clipboard."
  (send-command "clipboard clear"))

(defun clipboard-get ()
  "Returns the content of the clipboard."
  (get-response "clipboard get"))

(defun clipboard-append (txt)
  "Appends the text TXT to the clipboard."
  (send-command "clipboard append ~s" txt))

(defun tk-version ()
  "Returns the version of tcl/tk.

The return value is a list of three integers."
  (let ((r (get-response "info patchlevel")))
    (mapcar #'parse-integer
            (split-sequence #\. r))))
