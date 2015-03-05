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

(defmacro define-tk-window ((win name tk-name parent &key tk-init) &body body)
  `(progn
     (defclass ,win (,parent) ()
       (:documentation ,(concatenate 'string "This is a wrapper for the " name " class.

It shouldn't be used directly. Use the " (string win) " function instead.")))
     (defun ,win (&rest options)
       ,(concatenate 'string "Creates a new window of type " (string win) ".

The undelying tcl/tk class is " (string name) ".")
       (let ((w (make-instance ',win
                               :name      ,name
                               :tk-name   (getf options :tk-name ,tk-name)
                               :tk-define (getf options :tk-define t)
                               :tk-init   ,tk-init
                               :id        (if (getf options :tk-name) "" (next-id))
                               :parent    (getf options :parent))))
         (configure-window w options)
         ,@body
         w))))

(define-tk-window (button       "ttk::button"      "bt" window))
(define-tk-window (radiobutton  "ttk::radiobutton" "rb" button))
(define-tk-window (label        "ttk::label"       "lb" window))
(define-tk-window (entry        "ttk::entry"       "en" window))
(define-tk-window (spinbox      "ttk::spinbox"     "sp" entry))
(define-tk-window (frame        "ttk::frame"       "fr" window))
(define-tk-window (label-frame  "ttk::labelframe"   "lf" window))
(define-tk-window (scale        "ttk::scale"       "sc" window))
(define-tk-window (progressbar  "ttk::progressbar" "pr" window))
(define-tk-window (separator    "ttk::separator"   "sp" window))
(define-tk-window (toplevel     "toplevel"         "tp" window))
(define-tk-window (message      "message"          "ms" window))
(define-tk-window (combobox     "ttk::combobox"    "cx" entry))
(define-tk-window (menubutton   "ttk::menubutton"  "mb" window))
(define-tk-window (sizegrig     "ttk::sizegrip"    "sg" window))
(define-tk-window (checkbutton  "ttk::checkbutton" "cb" button)
  (setf (window-state w) "!alternate"))

(defun progressbar-start (p &optional interval)
  "Auto-increment progressbar every INTERVAL ms."
  (send-command "~a start ~a" (window-path p) (or interval 20)))

(defun progressbar-stop (p)
  "Stop auto-incrementing progressbar."
  (send-command "~a stop" (window-path p)))

(defun progressbar-step (p &optional step)
  "Increment the progressbar by STEP."
  (send-command "~a step ~a" (window-path p) step))

(defun entry-bbox (ent ind)
  "Returns the bounding box of the char at IND."
  (let ((r (get-response "~a bbox ~a" (window-path ent) ind)))
    (if (> (length r) 0)
        (mapcar #'read-from-string (split-sequence #\Space r))
        ())))

(defun entry-delete (ent first &optional last)
  "Deletes the chars from FIRST to LAST from the entry ENT.

If LAST is not specified it deletes one character."
  (send-command "~a delete ~a ~a" (window-path ent) first (or last "")))

(defun entry-get (ent)
  "Returns the text in the entry ENT."
  (get-response "~a get" (window-path ent)))

(defun entry-icursor (ent ind)
  "Sets the input cursor to index IND."
  (send-command "~a icursor ~a" (window-path ent) ind))

(defun entry-index (ent ind)
  "Returns the index corresponding to IND."
  (get-response "~a index ~a" (window-path ent) ind))

(defun entry-insert (ent ind str)
  "Inserts STR at indes IND in editable text in the window ENT."
  (send-command "~a insert ~a ~s" (window-path ent) ind str))

(defun entry-selection-clear (ent)
  "Clears the selection in editable text in ENT."
  (send-command "~a selection clear" (window-path ent)))

(defun entry-selection-present (ent)
  "Check is some text is selected in ENT."
  (if (string= (get-response "~a selection present" (window-path ent)) "1")
      t
      nil))

(defun entry-selection-range (ent start end)
  "Sets the selection in ENT from START ot END."
  (send-command "~a selection set ~a ~a" (window-path ent) start end))

(defun entry-validate (ent)
  "Validates the entry."
  (if (string= (get-response "~a validate" (window-path ent)) "0")
      nil
      t))

(defun entry-xview (ent)
  "Returns the xview of the window"
  (mapcar #'read-from-string
          (split-sequence #\Space
                          (get-response "~a xview" (window-path ent)))))

(defun (setf entry-xview) (ind ent)
  "Sets the xview of the entry so that the char at IND is visible."
  (send-command "~a xview ~a" (window-path ent) ind))

(defun entry-xview-moveto (ent frac)
  "Sets the view so that the char at fraction FRAC is visible."
  (send-command "~a xview moveto ~a" (window-path ent) frac))

(defun entry-xview-scroll (ent number what)
  "Scrolls the window according to NUMBER and WHAT.

NUMBER is an integer and WHAT can be \"units\" or \"pages\"."
  (send-command "~a xview moveto ~a ~a ~a" (window-path ent) number what))

(defun button-invoke (btn)
  "Invokes the command associated with button BTN.

BTN can also be a checkbutton or a radiobutton."
  (send-command "~a invoke" (window-path btn)))
