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

(define-tk-window (treeview "ttk::treeview" "tw" window))

(defun treeview-bbox (tw item &optional col)
  "Returns the bounding box of the ITEM."
  (mapcar #'parse-integer
          (split-sequence #\Space
                          (get-response "~a bbox ~a ~a"
                                        (window-path tw) item (or col "")))))

(defun treeview-children (tw item)
  "Returns the list of childtren blonging to ITEM."
  (let ((r (get-response "~a children ~a" (window-path tw) item)))
    (if (string= "" r)
        ()
        (split-sequence #\Space r))))

(defun treeview-column-id (tw col)
  "Returns the ID of the column."
  (get-response "~a column ~a -id" (window-path tw) col))

(defun treeview-column-width (tw col)
  "Returns the width of column COL.

Can be modified with SETF."
  (get-response "~a column ~a -width" (window-path tw) col))

(defun (setf treeview-column-width) (val tw col)
  "Sets the column width."
  (send-command "~a column ~a -width ~a" (window-path tw) col val))

(defun treeview-column-stretch (tw col)
  "Returns the column stretch property.

Can be modified with SETF."
  (string=  "1" (get-response "~a column ~a -stretch" (window-path tw) col)))

(defun (setf treeview-column-stretch) (val tw col)
  "Sets the column stretch property."
  (send-command "~a column ~a -stretch ~a" (window-path tw) col (if val "1" "0")))

(defun treeview-column-minwidth (tw col)
  "Returns the min-width of column COL.

Can be modified with SETF."
  (get-response "~a column ~a -minwidth" (window-path tw) col))

(defun (setf treeview-column-minwidth) (val tw col)
  "Sets the column min-width."
  (send-command "~a column ~a -minwidth ~a" (window-path tw) col val))

(defun treeview-column-anchor (tw col)
  "Returns the anchor property of column COL.

Can be modified with SETF."
  (get-response "~a column ~a -anchor" (window-path tw) col))

(defun (setf treeview-column-anchor) (val tw col)
  "Sets the column anchor property."
  (send-command "~a column ~a -anchor ~a" (window-path tw) col val))

(defun treeview-heading-command (tw heading fun)
  "Sets the callback FUN for HEADING.

The callback is called when the heading is clicked."
  (let ((id (format nil "tw_heading~a" (next-id))))
    (send-command "~a heading ~a -command {call_lisp ~a}"
                  (window-path tw) heading id)
    (setf (gethash id *event-table*) fun)))

(defun treeview-delete (tw items)
  "Deletes the ITEMS from the treeview."
  (unless (listp items)
    (setf items (list items)))
  (send-command "~a delete ~{~a~^ ~}" (window-path tw) items))

(defun treeview-detach (tw items)
  "Detaches the ITEMS from the treeview."
  (unless (listp items)
    (setf items (list items)))
  (send-command "~a detach ~{~a~^ ~}" (window-path tw) items))

(defun treeview-exists (tw item)
  "Checks if the ITEM exists in TW."
  (string= "1" (get-response "~a exists ~a" (window-path tw) item)))

(defun treeview-focus (tw)
  "Returns the item which has the focus."
  (get-response "~a focus" (window-path tw)))

(defun (setf treeview-focus) (item tw)
  "Sets the focus to ITEM."
  (send-command "~a focus ~a" (window-path tw) item))

(defun treeview-heading-text (tw col)
  "Returns the text of the heading in column COL.

Can be modified with SETF."
  (get-response "~a heading ~a -text" (window-path tw) col))

(defun (setf treeview-heading-text) (val tw col)
  "Sets the text of the heading COL."
  (send-command "~a heading ~a -text ~s" (window-path tw) col val))

(defun treeview-heading-image (tw col)
  "Returns the image of the heading in column COL.

Can be modified with SETF."
  (get-response "~a heading ~a -image" (window-path tw) col))

(defun (setf treeview-heading-image) (val tw col)
  "Sets the image of the heading COL."
  (send-command "~a heading ~a -image ~a" (window-path tw) col val))

(defun treeview-heading-anchor (tw col)
  "Returns the anchor of the heading the column COL.

Can be modified with SETF."
  (get-response "~a heading ~a -anchor" (window-path tw) col))

(defun (setf treeview-heading-anchor) (val tw col)
  "Sets the anchor of the heading COL."
  (send-command "~a heading ~a -anchor ~a" (window-path tw) col val))

(defun treeview-insert (tw parent index &key (id) (text) (image) (values) (open) (tags))
  "Inserts a new item.

PARENT is the parent line in the treeview (use empty string for no
parent). INDEX is the position between children of PARENT. TEXT option
is for the label in the first column and VALUES is a list of values in
the other columns."
  (get-response "~a insert ~a ~a ~a ~a ~a ~a ~a ~a"
                (window-path tw)
                (if (string= "" parent) "{}" parent)
                index
                (if id     (format nil "-id ~a" id)                    "")
                (if image  (format nil "-image ~a" image)              "")
                (if text   (format nil "-text ~s" text)                "")
                (if values (format nil "-values {~{~s~^ ~}}" values) "")
                (if open   (format nil "-open \"1\"")                  "")
                (if tags   (format nil "-tags ~{~s~^ ~}" tags)         "")))

(defun treeview-item-id (tw item)
  "Returns the ID of the item."
  (get-response "~a item ~a -id" (window-path tw) item))

(defun treeview-item-text (tw item)
  "Returns the text of the item."
  (get-response "~a item ~a -text" (window-path tw) item))

(defun (setf treeview-item-text) (val tw item)
  "Sets the text of the item."
  (get-response "~a item ~a -text ~a" (window-path tw) item) val)

(defun treeview-item-values (tw item)
  "Returns the values of the item."
  (parse-tcl-string (get-response "~a item ~a -values" (window-path tw) item)))

(defun (setf treeview-item-values) (val tw item)
  "Sets the values of the item."
  (get-response "~a item ~a -values \{~{~s~^ ~}\}" (window-path tw) item) val)

(defun treeview-move (tw item parent index)
  "Moves ITEM to position INDEX in PARENT."
  (send-command "~a move ~a ~a ~a" (window-path tw) item parent index))

(defun treeview-next (tw item)
  "Returns the ID of the next sibling of ITEM."
  (get-response "~a next ~a" (window-path tw) item))

(defun treeview-parent (tw item)
  "Returns the ID of the parent of ITEM."
  (get-response "~a parent ~a" (window-path tw) item))

(defun treeview-prev (tw item)
  "Returns the ID of the previous sibling of ITEM."
  (get-response "~a prev ~a" (window-path tw) item))

(defun treeview-get (tw item col)
  "Returns the conent of column COL in ITEM."
  (get-response "~a set ~a ~a" (window-path tw) item col))

(defun treeview-set (tw item col &optional val)
  "Sets the conent of column COL in ITEM."
  (get-response "~a set ~a ~a" (window-path tw) item col (or val "")))

(defun treeview-selection (tw)
  "Returns the list of selected items."
  (let ((r (get-response "~a selection" (window-path tw))))
    (if (string= r "")
        ()
        (split-sequence #\Space r))))

(defun treeview-selection-set (tw ilist)
  "Sets the selection in TW."
  (unless (listp ilist)
    (setf ilist (list ilist)))
  (send-command "~a selection set ~{~a~^ ~}" (window-path tw) ilist))

(defun treeview-selection-add (tw ilist)
  "Adds ILIST to the selection in TW."
  (unless (listp ilist)
    (setf ilist (list ilist)))
  (send-command "~a selection add ~{~a~^ ~}" (window-path tw) ilist))

(defun treeview-selection-remove (tw ilist)
  "Removes from ILIST the selection in TW."
  (unless (listp ilist)
    (setf ilist (list ilist)))
  (send-command "~a selection remove ~{~a~^ ~}" (window-path tw) ilist))

(defun treeview-selection-toggle (tw ilist)
  "Toggles the selection of ILIST in TW."
  (unless (listp ilist)
    (setf ilist (list ilist)))
  (send-command "~a selection toggle ~{~a~^ ~}" (window-path tw) ilist))
