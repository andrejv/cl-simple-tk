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

(define-tk-window (notebook "ttk::notebook" "nb" window))

(defun nb-tab-config (n tid options)
  (loop for opt on options by #'cddr do
       (send-command "~a tab ~a -~a ~s"
                     (window-path n) tid
                     (key-to-string (car opt)) (option-to-string (cadr opt)))))

(defun notebook-add (n w &rest options)
  "Adds a new window W to the notebook N.

The title of the tab is specified with the TEXT keyword."
  (send-command "~a add ~a" (window-path n) (window-path w))
  (nb-tab-config n (window-path w) options))

(defun notebook-forget (n tid)
  "Forgets the tab with id TID."
  (send-command "~a forget ~a" (window-path n) (option-to-string tid)))

(defun notebook-hide (n tid)
  "Hides the tab with id TID."
  (send-command "~a hide ~a" (window-path n) (option-to-string tid)))

(defun notebook-identify-element (n x y)
  "Identifies the element at X,Y."
  (get-response "~a identify element ~a ~a" (window-path n) x y))

(defun notebook-identify-tab (n x y)
  "Identifies the tab at X,Y."
  (get-response "~a identify tab ~a ~a" (window-path n) x y))

(defun notebook-index (n tid)
  "Returns the index of tab TID."
  (parse-integer (get-response "~a index ~a" (window-path n) tid)))

(defun notebook-insert (n pos subw)
  "Insert SUBW into the notebook at index POS."
  (send-command "~a insert ~a ~a" (window-path n) pos (window-path subw)))

(defun notebook-select (n)
  "Returns the selected tab in the notebook."
  (get-response "~a select" (window-path n)))

(defun (setf notebook-select) (ind n)
  "Sets the selected notebook."
  (send-command "~a select ~a" (window-path n) ind))

(defun notebook-tab (n tid &rest options)
  "Queries/modifies the options of the tab TID."
  (if (< (length options) 2)
      (get-response "~a tab ~a ~{~a~^ ~}" (window-path n) tid options)
      (nb-tab-config n tid options)))

(defun notebook-tabs (n)
  "Returns a list of tabs in N."
  (split-sequence #\Space
                  (get-response "~a tabs" (window-path n))))
