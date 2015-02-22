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

(define-tk-window (text "text" "txt" window))

(defun text-bbox (txt ind)
  "Returns the bounding box of the char at index IND."
  (let ((r (get-response "~a bbox ~a" (window-path txt) ind)))
    (if (> (length r) 0)
        (mapcar #'read-from-string (split-sequence #\Space r))
        ())))

(defun text-count-chars (txt ind1 ind2)
  "Counts the number of chars between IND1 and IND2."
  (get-response "~a count -chars ~a ~a" (window-path txt) ind1 ind2))

(defun text-count-displaychars (txt ind1 ind2)
  "Counts the number of displaychars between IND1 and IND2."
  (get-response "~a count -displaychars ~a ~a" (window-path txt) ind1 ind2))

(defun text-count-displayindices (txt ind1 ind2)
  "Counts the number of displayindices between IND1 and IND2."
  (get-response "~a count -displayindices ~a ~a" (window-path txt) ind1 ind2))

(defun text-count-displaylines (txt ind1 ind2)
  "Counts the number of lines  between IND1 and IND2."
  (get-response "~a count -displaylines ~a ~a" (window-path txt) ind1 ind2))

(defun text-count-indices (txt ind1 ind2)
  "Counts the number of indices between IND1 and IND2."
  (get-response "~a count -indices ~a ~a" (window-path txt) ind1 ind2))

(defun text-count-lines (txt ind1 ind2)
  "Counts the number of lines between IND1 and IND2."
  (get-response "~a count -lines ~a ~a" (window-path txt) ind1 ind2))

(defun text-count-xpixels (txt ind1 ind2)
  "Counts the number of xpixels between IND1 and IND2."
  (get-response "~a count -xpixels ~a ~a" (window-path txt) ind1 ind2))

(defun text-count-ypixels (txt ind1 ind2)
  "Counts the number of ypixels between IND1 and IND2."
  (get-response "~a count -ypixels ~a ~a" (window-path txt) ind1 ind2))

(defun text-delete (txt ind1 &optional ind2)
  "Counts the number of ypixels between IND1 and IND2."
  (get-response "~a count -ypixels ~a ~a" (window-path txt) ind1 (or ind2)))

(defun text-dlineinfo (txt ind)
  "Retuls the line info of the line containing IND."
  (get-response "~a dlineinfo ~a" (window-path txt) ind))

(defun text-edit-modified (txt)
  "Returns the modified flag."
  (if (string= "0" (get-response "~a edit modified" (window-path txt)))
      nil
      t))

(defun (setf text-edit-modified) (val txt)
  "Sets the modified flag."
  (send-command "~a eidit modified ~a" (window-path txt) (if val "1" "0")))

(defun text-edit-redo (txt)
  "Reapplies the last undone edit."
  (send-command "~a edit redo" (window-path txt)))

(defun text-edit-reset (txt)
  "Clear the undo/redo stacks."
  (send-command "~a edit reset" (window-path txt)))

(defun text-edit-separator (txt)
  "Inserts a separator into the undo stack."
  (send-command "~a edit separator" (window-path txt)))

(defun text-edit-undo (txt)
  "Undoes the last edit."
  (send-command "~a edit undo" (window-path txt)))

(defun text-get (txt ind1 &optional ind2)
  "Undoes the last edit."
  (send-command "~a get ~a ~a" (window-path txt) ind1 (or ind2 "")))

(defun text-get-displaychars (txt ind1 &optional ind2)
  "Undoes the last edit."
  (send-command "~a get ~a ~a" (window-path txt) ind1 (or ind2 "")))

(defun text-index (txt ind)
  "Returns the position corresponding to IND.

The ruturn value is a string \"l.c\" where l is the line and char is 
the character number. IND can be a string representation of an 
index (\"end\", mark, tag.first, tag.last)."
  (get-response "~a index ~a") (window-path txt) ind)

(defun text-insert (txt ind str)
  "Inserts STR at indes IND in editable text in the window TXT."
  (send-command "~a insert ~a ~s" (window-path txt) ind str))

(defun text-image-cget (txt ind opt)
  "Returns the option OPT for the image embedded at IND."
  (get-response "~a image cget ~a ~a" (window-path txt) ind (key-to-string opt)))

(defun text-image-create (txt ind &rest options)
  "Inserts an image at index IND.

The image is specified with the :image option."
  (let ((r (get-response "~a image create ~a -image ~a"
                         (window-path txt) ind (option-to-string (getf options :image)))))
    (loop for opt on options by #'cddr
       unless (eq (car opt) :image)
       do
         (send-command "~a image config ~a -~a ~a"
                       (window-path txt) ind (key-to-string (car opt)) (option-to-string (cadr opt))))
    r))

(defun text-image-configure (txt ind &rest options)
  "Configures the image at index IND."
  (loop for opt on options by #'cddr do
       (send-command "~a image config ~a -~a ~a"
                     (window-path txt) ind (key-to-string (car opt)) (option-to-string (cadr opt)))))

(defun text-image-names (txt)
  "Returns the names of images embedded in TXT."
  (let ((r (get-response "~a image names" (window-path txt))))
    (if (string= "" r)
        ()
        (split-sequence #\Space r))))

(defun text-mark-gravity (txt mark)
  "Returns the gracity of the mark MARK."
  (get-response "~a mark gravity ~a" (window-path txt) mark))

(defun text-mark-names (txt)
  "Returns the marks in the window."
  (let ((r (get-response "~a mark names" (window-path txt))))
    (if (string= "" r)
        ()
        (split-sequence #\Space r)))9)

(defun text-mark-next (txt ind)
  "Returns the name of the next mark after index IND."
  (get-response "~a mark next ~a" (window-path txt) ind))

(defun text-mark-previous (txt ind)
  "Returns the name of the mark before index IND."
  (get-response "~a mark previous ~a" (window-path txt) ind))

(defun text-mark-set (txt mark ind)
  "Sets the mark MARK at index IND."
  (send-command "~a mark set ~a ~a" (window-path txt) mark ind))

(defun text-mark-unset (txt &rest marks)
  "Unsets the MARKS."
  (send-command "~a mark unset ~{~a~^ ~}" (window-path txt) marks))

(defun text-replace (txt ind1 ind2 str)
  "Replaces the text between IND1 IND2 with STR."
  (send-command "~a replace ~a ~a ~s" (window-path txt) ind1 ind2 str))

(defun text-see (txt ind)
  "Mmodifies the view to that char at index IND is visible."
  (send-command "~a see ~a" (window-path txt) ind))

(defun text-search (txt s ind &rest switches)
  "Searches for S in TXT."
  (get-response "~a search ~{-~a~^ ~} ~s ~a"
                (window-path txt) switches s ind))

(defun text-tag-add (txt tag &rest indxs)
  "Set the TAG in the window"
  (send-command "~a tag add ~s ~{~a~^ ~}" (window-path txt) tag indxs))

(defun text-tag-bind (txt tag ev fun)
  "Binds the event EV on the tag TAG.

Function FUN accepts one argument."
  (let ((id (string-downcase (format nil "~a.~a.~a" (window-path txt) tag ev))))
    (send-command "~a tag bind ~a ~a \{call_lisp ~a %x %y %A %W\}"
                  (window-path txt) tag ev id)
    (setf (gethash id *event-table*) fun)))

(defun text-tag-cget (txt tag opt)
  "Returns the value of the option OPT for the TAG."
  (get-response "~a tag cget ~a ~a" (window-path txt) tag (key-to-string opt)))

(defun text-tag-config (txt tag &rest options)
  "Configures the TAG in TXT."
  (loop for opt on options by #'cddr do
       (send-command "~a tag configure ~s -~a ~a"
                     (window-path txt) tag
                     (key-to-string (car opt))
                     (option-to-string (cadr opt)))))

(defun text-tag-delete (txt &rest tags)
  "Removes TAGS."
  (send-command "~a tag delete ~{~a~^ ~}" (window-path txt) tags))

(defun text-tag-lower (txt tid &optional below)
  "Lowers the tag TID."
  (send-command "~a tag lower ~a ~a" (window-path txt) tid (or below "")))

(defun text-tag-names (txt &optional ind)
  "Returns the all tags (at position IND if specified)."
  (let ((r (get-response "~a tag names ~a" (window-path txt) (or ind ""))))
    (if (string= r "")
        ()
        (split-sequence #\Space r))))

(defun text-tag-raise (txt tid &optional above)
  "Raises the tag TID."
  (send-command "~a tag raise ~a ~a" (window-path txt) tid (or above "")))

(defun text-window-cget (txt ind opt)
  "Returns the option OPT for the window embedded at IND."
  (get-response "~a window cget ~a ~a" (window-path txt) ind (key-to-string opt)))

(defun text-window-create (txt ind &rest options)
  "Creates the window at index IND."
  (let ((r (get-response "~a window create ~a -window ~a"
                         (window-path txt) ind (option-to-string (getf options :window)))))
    (loop for opt on options by #'cddr
       unless (eq (car opt) :window)
       do
         (send-command "~a window config ~a -~a ~a"
                       (window-path txt) ind (key-to-string (car opt)) (option-to-string (cadr opt))))
    r))

(defun text-window-configure (txt ind &rest options)
  "Configures the window at index IND."
  (loop for opt on options by #'cddr do
       (send-command "~a window config ~a -~a ~a"
                     (window-path txt) ind (key-to-string (car opt)) (option-to-string (cadr opt)))))

(defun text-window-names (txt)
  "Returns the names of windows embedded in TXT."
  (let ((r (get-response "~a window names" (window-path txt))))
    (if (string= "" r)
        ()
        (split-sequence #\Space r))))

(defun text-xview (text)
  "Returns the xview of the window.

Uset (setf (text-view txt) ind) to ajust the view so that index IND is 
visible."
  (mapcar #'read-from-string
          (split-sequence #\Space
                          (get-response "~a xview" (window-path text)))))

(defun (setf text-xview) (ind text)
  "Sets the xview of the text so that the char at IND is visible."
  (send-command "~a xview ~a" (window-path text) ind))

(defun text-xview-moveto (text frac)
  "Sets the view so that the char at fraction FRAC is visible."
  (send-command "~a xview moveto ~a" (window-path text) frac))

(defun text-xview-scroll (text number what)
  "Scrolls the window according to NUMBER and WHAT in the x direction.

NUMBER is an integer and WHAT can be \"units\" or \"pages\"."
  (send-command "~a xview moveto ~a ~a ~a" (window-path text) number what))

(defun text-yview (text)
  "Returns the yview of the window.

Use (setf (text-yview txt) ind) to adjust the view so that index IND 
is visible."
  (mapcar #'read-from-string
          (split-sequence #\Space
                          (get-response "~a yview" (window-path text)))))

(defun (setf text-yview) (ind text)
  "Sets the yview of the text so that the char at IND is visible."
  (send-command "~a yview ~a" (window-path text) ind))

(defun text-yview-moveto (text frac)
  "Sets the view so that the char at fraction FRAC is visible."
  (send-command "~a yview moveto ~a" (window-path text) frac))

(defun text-yview-scroll (text number what)
  "Scrolls the window according to NUMBER and WHAT in the y direction.

NUMBER is an integer and WHAT can be \"units\" or \"pages\"."
  (send-command "~a yview moveto ~a ~a ~a" (window-path text) number what))
