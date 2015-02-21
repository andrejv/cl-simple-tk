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

(defun window-state (w)
  "Returns the state SPEC of the window W.

The state can be modified with the setf macro."
  (get-response "~a state" (window-path w)))

(defun (setf window-state) (spec w)
  "Sets the state of the window W."
  (send-command "~a state ~a" (window-path w) spec))

#+nil(defun window-selected (w)
  "Returns t if the window W is selected.

Can be used with the setf macro to select a window."
  (string-equal "0" (window-state w "!selected")))

(defun (setf window-selected) (val w)
  "Sets the selected state of the window W."
  (if val
      (setf (window-state w) "selected")
      (setf (window-state w) "!selected")))

(defun window-resizable (w)
  "Checks if the window W is resizable."
  (let* ((r (get-response "wm resizable ~a" (if w (window-path w) ".")))
         (r-split (split-sequence #\Space r)))
    (list (string= "1" (car r-split))
          (string= "1" (cadr r-split)))))

(defun (setf window-resizable) (r w)
  "Sets the resizable property for window W.

R should be a list with two boolean values, first for vertical and
second for horihontal property."
  (send-command "wm resizable ~a ~s ~s"
                (if w (window-path w) ".")
                (if (car r) "1" "0")
                (if (cadr r) "1" "0")))

(defun window-identify-element (w x y)
  "Identifies the subwindow in W at coordinates X,Y."
  (get-response "~a identify element ~a ~a" (window-path w) x y))

(defun window-geometry (w)
  "Checks if the window W is resizable."
  (let* ((r (get-response "wm geometry ~a" (if w (window-path w) ".")))
         (r-split (split-sequence #\+ r))
         (wh (split-sequence #\x (car r-split))))
    (list (parse-integer (car wh))
          (parse-integer (cadr wh))
          (parse-integer (cadr r-split))
          (parse-integer (caddr r-split)))))

(defun (setf window-geometry) (g w)
  "Sets the geometry of the window W.

G lis a list of ((w h) x y)."
  (send-command "wm geometry ~a ~ax~a+~a+~a"
                (window-path w)
                (car g) (cadr g) (caddr g) (cadddr g)))

(defun window-minsize (w)
  "Returns the minsize of the window."
  (mapcar #'parse-integer
          (get-response "wm minsize ~a" (if w (window-path w) "."))))

(defun (setf window-minsize) (l w)
  "Sets the minsize of the window."
  (send-command "wm minsize ~a ~a ~a" (if w (window-path w) ".") (car l) (cadr l)))

(defun window-withdraw (w)
  "Withdraws the window W."
  (send-command "wm withdraw ~a" (if w (window-path w) ".")))

(defun window-destroy (w)
  "Withdraws the window W.

W can be nil for the root window \".\""
  (send-command "destroy ~a" (if w (window-path w) ".")))

(defun get-tk-themes ()
  "Returns a list of Tk themes."
  (let ((themes (get-response "ttk::style theme names")))
    (split-sequence #\Space themes)))

(defun set-tk-theme (theme)
  "Sets the Tk Theme."
  (send-command "ttk::style theme use ~a" theme))
