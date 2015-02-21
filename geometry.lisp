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

(defun pack (wlist &rest options)
  "PACK geometry manager.

WLIST can be a window of a list of windows."
  (unless (listp wlist)
    (setf wlist (list wlist)))
  (setf wlist (mapcar #'window-path wlist))
  (send-command "pack ~{~a~^ ~}" wlist)
  (loop for op on options by #'cddr do
       (send-command "pack config ~{~a~^ ~} -~a ~s"
                     wlist (key-to-string (car op)) (option-to-string (cadr op)))))

(defun grid (wlist &rest options)
  "GRID geometry manager.

WLIST can be a window or a list of windows."
  (unless (listp wlist)
    (setf wlist (list wlist)))
  (setf wlist (mapcar #'window-path wlist))
  (send-command "grid ~{~a~^ ~}" wlist)
  (loop for op on options by #'cddr do
       (send-command "grid config ~{~a~^ ~} -~a ~s"
                     wlist (key-to-string (car op)) (cadr op))))

(defun grid-col-configure (w i &rest options)
  "Configures the columns of the grid geometry manager for windows W."
  (loop for opt on options by #'cddr do
       (send-command "grid columnconfigure ~a ~a -~a ~s"
                     (window-path w) i (key-to-string (car opt)) (option-to-string (cadr opt)))))

(defun grid-row-configure (w i &rest options)
  "Configures the rows of the grid geometry manager for windows W."
  (loop for opt on options by #'cddr do
       (send-command "grid rowconfigure ~a ~a -~a ~s"
                     (window-path w) i (key-to-string (car opt)) (option-to-string (cadr opt)))))

(defun place (wlist &rest options)
  "PLACE geometry manager.

WLIST can be a window or a list of windows."
  (unless (listp wlist)
    (setf wlist (list wlist)))
  (setf wlist (mapcar #'window-path wlist))
  (send-command "palce ~{~a~^ ~}" wlist)
  (loop for op on options by #'cddr do
       (send-command "place config ~{~a~^ ~} -~a ~s"
                     wlist (key-to-string (car op)) (cadr op))))
