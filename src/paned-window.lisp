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

(define-tk-window (panedwindow "ttk::panedwindow" "pw" window
                               :tk-init (format nil "-orient ~a" (getf options :orient "vertical"))))

(defun pw-pane-config (pw pid options)
  (loop for opt on options by #'cddr do
       (send-command "~a pane ~a -~a ~s"
                     (window-path pw) pid
                     (key-to-string (car opt)) (option-to-string (cadr opt)))))

(defun panedwindow-add (pw w &rest options)
  "Adds a new window W to the panedwindow N.

TEXT is the title of the tab."
  (send-command "~a add ~a" (window-path pw) (window-path w))
  (pw-pane-config pw (window-path w) options))

(defun panedwindow-forget (pw pid)
  "Forgets the window with id PID."
  (send-command "~a forget ~a" (window-path pw) (option-to-string pid)))

(defun panedwindow-identify-element (pw x y)
  "Identifies the element at X,Y."
  (get-response "~a identify element ~a ~a" (window-path pw) x y))

(defun panedwindow-identify-sash (pw x y)
  "Identifies the sash at X,Y."
  (parse-integer (get-response "~a identify sash ~a ~a" (window-path pw) x y)))

(defun panedwindow-insert (pw pos subw)
  "Insert SUBW into the panedwindow PW at index POS."
  (send-command "~a insert ~a ~a" (window-path pw) pos (window-path subw)))

(defun panedwindow-sashpos (pw ind)
  "Returns the sash position of subwindow with index IND.

Use SETF to set the position."
  (get-response "~a sashpos ~a" (window-path pw) ind))

(defun (setf panedwindow-sashpos) (newpos pw ind)
  "Sets the sashpos of the subwindow with index IND."
  (send-command "~a sashpos ~a ~a" (window-path pw) ind newpos))
