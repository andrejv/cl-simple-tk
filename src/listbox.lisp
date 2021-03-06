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

(define-tk-window (listbox "listbox" "lx" window))

(defun listbox-bbox (lbox ind)
  "Returns the bounding box of the item IND."
  (mapcar #'parse-integer
          (split-sequence #\Space
                          (get-response "~a bbox ~{~a~^ ~}"
                                        (window-path lbox) ind))))

(defun listbox-activate (lbox ind)
  "Activates the element specified by IND."
  (send-command "~a activate ~a" (window-path lbox) ind))

(defun listbox-curselection (lbox)
  "Returns the list containing the indices of current selection."
  (let ((r (get-response "~a curselection" (window-path lbox))))
    (unless (string= "" r)
      (mapcar #'parse-integer (split-sequence #\Space r)))))

(defun (setf listbox-curselection) (val lbox)
  "Sets the selecion."
  (if (listp val)
      (listbox-selection-set lbox (car val) (cadr val))
      (listbox-selection-set lbox val)))

(defun listbox-delete (lbox first &optional last)
  "Deletes from the listbox"
  (send-command "~a delete ~a ~a" (window-path lbox) first (or last "")))

(defun listbox-get (lbox first &optional last)
  "Returns the contents of the listbox."
  (let ((r (get-response "~a get ~a ~a"
                         (window-path lbox)
                         first
                         (if last last ""))))
    (if last
        (parse-tcl-string r)
        r)))

(defun listbox-index (lbox ind)
  "Returns the numeric index for IND."
  (parse-integer (get-response "~a index ~a" (window-path lbox) ind)))

(defun listbox-nearest (lbox y)
  "Returns the nearest item for y-coordinate Y."
  (parse-integer (get-response "~a nearest ~a" (window-path lbox) y)))

(defun listbox-insert (lbox ind elts)
  "Inserts new elements into the listbox."
  (unless (listp elts)
    (setf elts (list elts)))
  (send-command "~a insert ~a ~{~s~^ ~}" (window-path lbox) ind elts))

(defun listbox-see (lbox ind)
  "Adjusts the view so that IND is visible."
  (send-command "~a see ~a" (window-path lbox) ind))

(defun listbox-scan-mark (lbox x y)
  "Sets the scrolling mark."
  (send-command "~a scan mark ~a ~a" (window-path lbox) x y))

(defun listbox-scan-dragto (lbox x y &optional gain)
  "Scrolls the listbox according to the scan mar."
  (send-command "~a scan dragto ~a ~a ~a" (window-path lbox) x y (or gain "")))

(defun listbox-selection-anchor (lbox ind)
  "Sets the anchor for the selection in LBOX."
  (send-command "~a selection anchor ~a" (window-path lbox) ind))

(defun listbox-selection-clear (lbox start &optional end)
  "Deselect elements in the listbox."
  (send-command "~a selection clear ~a ~a"
                (window-path lbox)
                start
                (or end "")))

(defun listbox-selection-includes (lbox ind)
  "Checks if item at index IND is selected."
  (string= "1" (get-response "~a selection includes ~a" (window-path lbox) ind)))

(defun listbox-selection-set (lbox start &optional end)
  "Sets the selection in the listbox."
  (send-command "~a selection set ~a ~a"
                (window-path lbox)
                start
                (or end "")))

(defun listbox-size (lbox)
  "Returns the number of elements in the listbox."
  (parse-integer (get-response "~a size" (window-path lbox))))

(defun listbox-xview (lbox)
  "Returns the xview of the window"
  (mapcar #'read-from-string
          (split-sequence #\Space
                          (get-response "~a xview" (window-path lbox)))))

(defun (setf listbox-xview) (ind lbox)
  "Sets the xview of the listbox so that the char at IND is visible."
  (send-command "~a xview ~a" (window-path lbox) ind))

(defun listbox-xview-moveto (lbox frac)
  "Sets the view so that the char at fraction FRAC is visible."
  (send-command "~a xview moveto ~a" (window-path lbox) frac))

(defun listbox-xview-scroll (lbox number what)
  "Scrolls the window according to NUMBER and WHAT in the x direction.

NUMBER is an integer and WHAT can be \"units\" or \"pages\"."
  (send-command "~a xview moveto ~a ~a ~a" (window-path lbox) number what))

(defun listbox-yview (lbox)
  "Returns the yview of the window."
  (mapcar #'read-from-string
          (split-sequence #\Space
                          (get-response "~a yview" (window-path lbox)))))

(defun (setf listbox-yview) (ind lbox)
  "Sets the yview of the listbox so that the char at IND is visible."
  (send-command "~a yview ~a" (window-path lbox) ind))

(defun listbox-yview-moveto (lbox frac)
  "Sets the view so that the char at fraction FRAC is visible."
  (send-command "~a yview moveto ~a" (window-path lbox) frac))

(defun listbox-yview-scroll (lbox number what)
  "Scrolls the window according to NUMBER and WHAT in the y direction.

NUMBER is an integer and WHAT can be \"units\" or \"pages\"."
  (send-command "~a yview moveto ~a ~a ~a" (window-path lbox) number what))
