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

(defun parse-options (str)
  "Parses the options returned by GRID-INFO, PACK-INFO and PLACE-INFO functions."
  (flet ((replace-curly (s)
           (if (string= s "\{\}")
               ""
               s)))
    (if (string= str "")
        ()
        (let ((lst (split-sequence #\Space str)))
          (loop
             for (k o) on lst by #'cddr
             collect (intern (subseq (string-upcase k) 1) "KEYWORD") into opt
             collect (replace-curly o) into opt
             finally (return opt))))))

(defun grid (wlist &rest options)
  "GRID geometry manager.

WLIST can be a window or a list of windows."
  (unless (listp wlist)
    (setf wlist (list wlist)))
  (setf wlist (mapcar #'window-path wlist))
  (send-command "grid ~{~a~^ ~}" wlist)
  (loop for op on options by #'cddr do
       (send-command "grid config ~{~a~^ ~} -~a ~s"
                     wlist (key-to-string (car op)) (option-to-string (cadr op)))))

(defun grid-configure (w &rest options)
  "Configure the grid options for the window W"
  (loop for opt on options by #'cddr do
       (send-command "grid config ~a -~a ~s" (window-path w)
                     (key-to-string (car opt))
                     (option-to-string (cadr opt)))))

(defun grid-columnconfigure (w i &rest options)
  "Configures the columns of the grid geometry manager for windows W."
  (loop for opt on options by #'cddr do
       (send-command "grid columnconfigure ~a ~a -~a ~s"
                     (window-path w) i (key-to-string (car opt)) (option-to-string (cadr opt)))))

(defun grid-info (w)
  "Returns the grid options for the window W.

Returns a PLIST of options."
  (parse-options (get-response "grid info ~a" (window-path w))))

(defun grid-rowconfigure (w i &rest options)
  "Configures the rows of the grid geometry manager for windows W."
  (loop for opt on options by #'cddr do
       (send-command "grid rowconfigure ~a ~a -~a ~s"
                     (window-path w) i (key-to-string (car opt)) (option-to-string (cadr opt)))))

(defun grid-forget (w)
  "Stop managing the window W."
  (send-command "grid forget ~a" (window-path w)))

(defun grid-slaves (w &key row column)
  "Returns the list of slaves in master W."
  (let ((r (get-response "grid slaves ~a ~a ~a" (window-path w)
                         (if row (format nil "-row ~a" row) "")
                         (if column (format nil "-column ~a" column) ""))))
    (if (string= r "")
        ()
        (mapcar #'window-from-path
                (split-sequence #\Space r)))))

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

(defun pack-configure (w &rest options)
  "Configure the pack options for the window W"
  (loop for opt on options by #'cddr do
       (send-command "pack config ~a -~a ~s" (window-path w)
                     (key-to-string (car opt))
                     (option-to-string (cadr opt)))))

(defun pack-forget (w)
  "Stop managing the window W."
  (send-command "pack forget ~a" (window-path w)))

(defun pack-info (w)
  "Returns the pack options for the window W.

Returns a PLIST of options."
  (parse-options (get-response "grid info ~a" (window-path w))))

(defun pack-slaves (w)
  "Returns the list of slaves in master W."
  (let ((r (get-response "pack slaves ~a" (window-path w))))
    (if (string= r "")
        ()
        (mapcar #'window-from-path
                (split-sequence #\Space r)))))

(defun place (w &rest options)
  "PLACE geometry manager."
  (send-command "place ~a" (window-path w))
  (loop for op on options by #'cddr do
       (send-command "place config ~a -~a ~s"
                     (window-path w) (key-to-string (car op)) (cadr op))))

(defun place-configure (w &rest options)
  "Configure the place options for the window W"
  (loop for opt on options by #'cddr do
       (send-command "place config ~a -~a ~s" (window-path w)
                     (key-to-string (car opt))
                     (option-to-string (cadr opt)))))

(defun place-forget (w)
  "Stop managing the window W."
  (send-command "place forget ~a" (window-path w)))

(defun place-info (w)
  "Returns the place options for the window W.

Returns a PLIST of options."
  (parse-options (get-response "place info ~a" (window-path w))))

(defun place-slaves (w)
  "Returns the list of slaves in master W."
  (let ((r (get-response "place slaves ~a" (window-path w))))
    (if (string= r "")
        ()
        (mapcar #'window-from-path
                (split-sequence #\Space r)))))
