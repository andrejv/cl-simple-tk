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

(defclass scrollbar (window)
  ((orient
    :initarg :orient
    :reader scrollbar-orient))
  (:documentation
   "A scrollbar class. Use the function SCROLLBAR to create scrollbars."))

(defun scrollbar (&rest options)
  "Returns a scrollbar.

Orientation is defined with ORIENT key which can be \"vertical\" or
\"horizontal\". The scrollbar is connected to windows using the
SCROLLBAR-CONNECT function."
  (let ((scrl (make-instance 'scrollbar :name "ttk::scrollbar"
                             :tk-name "scb"
                             :parent (getf options :parent)
                             :tk-init (format nil "-orient ~a" (getf options :orient "vertical"))
                             :orient (getf options :orient "vertical"))))
    (configure-window scrl options)
    scrl))

(defun scrollbar-connect (w s)
  "Connects the window W with scrollbar S."
  (with-slots (orient) s
    (if (string= orient "horizontal")
        (progn
          (send-command "~a configure -command {~a xview}" (window-path s) (window-path w))
          (send-command "~a configure -xscrollcommand {~a set}" (window-path w) (window-path s)))
        (progn
          (send-command "~a configure -command {~a yview}" (window-path s) (window-path w))
          (send-command "~a configure -yscrollcommand {~a set}" (window-path w) (window-path s))))))

(defun scrollbar-delta (s dx dy)
  "Returns the fractional change corresponding to given change of
thumb position."
  (read-from-string (get-response "~a delta ~a ~a" (window-path s) dx dy)))

(defun scrollbar-fraction (s x y)
  "Returns the fraction corresponding to given point in through area
of scrollbar."
  (read-from-string (get-response "~a fraction ~a ~a" (window-path s) x y)))
