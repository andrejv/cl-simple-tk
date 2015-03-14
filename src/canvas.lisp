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

(define-tk-window (canvas "canvas" "cnv" window))

(defun canvas-create (c type coords)
   "Adds a new item to the canvas.

The type of the item should be a string ARC, OVAL, LINE, POLYGON,
RECTANGLE or TEXT. Returns the id of the creaated item."
   (get-response "~a create ~a ~{~a~^ ~}"
                 (window-path c) type coords))

(defun canvas-addtag (canvas tag spec &rest args)
  "Adds tags to all items according to spec"
  (send-command "~a addtag ~a ~a ~{~a~^ ~}" (window-path canvas) tag spec args))

(defmacro canvas-tag-method ((name args) doc &body body)
  `(defun ,name ,(append '(canvas newtag) args)
     ,doc
     (canvas-addtag canvas newtag ,@body)))

(canvas-tag-method (canvas-addtag-above (tag))
    "Adds NEWTAG to the item directly above TAG."
  "above" tag)

(canvas-tag-method (canvas-addtag-below (tag))
    "Adds NEWTAG to the item directly below TAG."
  "below" tag)

(canvas-tag-method (canvas-addtag-enclosed (x1 y1 x2 y2))
    "Adds NEWTAG to the items inclosed in the rectangle (x1,y1) (x2,y2)."
  "enclosed" x1 y1 x2 y2)

(canvas-tag-method (canvas-addtag-overlapping (x1 y1 x2 y2))
    "Adds NEWTAG to the items which overlap the rectangle (x1,y1) (x2,y2)."
  "overlapping" x1 y1 x2 y2)

(canvas-tag-method (canvas-addtag-withtag (oldtag))
    "Adds NEWTAG to the items with TAG."
  "withtag" oldtag)

(canvas-tag-method (canvas-addtag-closest (x y &key (halo) (start)))
    "Add NEWTAG to the item closest to X,Y."
  "closest" x y
  (if halo (format nil "~a" halo) "")
  (if start (format nil "~a" start) ""))

(canvas-tag-method (canvas-addtag-all ())
    "Adds NEWTAG to all items."
  "all")

(defun canvas-itemconfig (canvas id &rest options)
  "Sets OPTION for item with id/tag ID in CANVAS to VALUE."
  (loop for opt on options by #'cddr do
       (send-command "~a itemconfig ~a -~a ~s"
                     (window-path canvas) id
                     (key-to-string (car opt))
                     (option-to-string (cadr opt)))))

(defun canvas-itemcget (canvas tid opt)
  "Returns the option OPT for item TID."
  (get-response "~a itemcget ~a ~a" (window-path canvas) tid (key-to-string opt)))

(defun canvas-bbox (canvas &rest tags)
  "Returns the bounding box of the items with tagged tags."
  (mapcar #'parse-integer
          (split-sequence #\Space
                          (get-response "~a bbox ~{~a~^ ~}"
                                        (window-path canvas) tags))))

(defun canvas-bind (canvas tag ev fun)
  "Binds the event EV on the item .

Function FUN accepts one argument."
  (let ((id (string-downcase (format nil "~a.~a.~a" (window-path canvas) tag ev))))
    (send-command "~a bind ~a ~a \{call_lisp ~a %x %y %A %W\}"
                  (window-path canvas) tag ev id)
    (setf (gethash id *event-table*) fun)))

(defun canvas-canvasx (canvas x)
  "Returns the canvas x-cooddinate for a window coordinate X."
  (parse-integer (get-response "~a canvasx ~d" (window-path canvas) x)
                 :junk-allowed t))

(defun canvas-canvasy (canvas y)
  "Returns the canvas y-cooddinate for a window coordinate y."
  (parse-integer (get-response "~a canvasy ~d" (window-path canvas) y)
                 :junk-allowed t))

(defun canvas-scrolled-coords (canvas coords)
  "Transforms the window coordinates COORDS to canvas coordinates."
  (list (canvas-canvasx canvas (car coords))
        (canvas-canvasy canvas (cadr coords))))

(defun canvas-coords (canvas tid)
  "Returns the coordinates of the item TID."
  (get-response "~a coords ~a" (window-path canvas) tid))

(defun (setf canvas-coords) (coords canvas tid)
  "Sets the coordinates of the item TID."
  (send-command "~a coords ~a ~{~a~^ ~}" (window-path canvas) tid coords))

(defmacro create-canvas-method (method type)
  `(defun ,method (cnv coords &rest options)
     ,(concatenate 'string "Create an intem of type " (string-upcase type) " in the canvas.")
     (let* ((id (canvas-create cnv ,type coords))
            (args (append (list cnv id) options)))
       (apply #'canvas-itemconfig args)
       id)))

(create-canvas-method canvas-create-arc       "arc")
(create-canvas-method canvas-create-poylgon   "polygon")
(create-canvas-method canvas-create-rectangle "rectangle")
(create-canvas-method canvas-create-text      "text")
(create-canvas-method canvas-create-window    "window")
(create-canvas-method canvas-create-oval      "oval")
(create-canvas-method canvas-create-bitmap    "bitmap")
(create-canvas-method canvas-create-image     "image")
(create-canvas-method canvas-create-line      "line")

(defun canvas-dchars (canvas tid first &optional last)
  "Deletes chars or coordinates from item TID given by range FIRST,LAST."
  (send-command "~a dchars ~a ~a ~a" (window-path canvas) tid first
                (or last "")))

(defun canvas-delete (canvas id)
  "Deletes the item with id/tag ID from the CANVAS."
  (send-command "~a delete ~a" (window-path canvas) id))

(defun canvas-dtag (canvas tag &optional tag1)
  "Removes the TAG from items."
  (send-command "~a dtag ~a ~a" (window-path canvas) tag (or tag1 "")))

(defun canvas-find (canvas spec &rest args)
  "Returns ids of all items according to spec."
  (let ((r (get-response "~a find ~a ~{~a~^ ~}" (window-path canvas) spec args)))
    (if (string= r "")
        ()
        (split-sequence #\Space r))))

(defmacro canvas-find-method ((name args) doc &body body)
  `(defun ,name ,(cons 'canvas args)
     ,doc
     (canvas-find canvas ,@body)))

(canvas-find-method (canvas-find-above (tag))
    "Returs ids of all items directly above TAG."
  "above" tag)

(canvas-find-method (canvas-find-below (tag))
    "Returs ids of all items directly below TAG."
  "below" tag)

(canvas-find-method (canvas-find-enclosed (x1 y1 x2 y2))
    "Returs ids of all items inclosed in the rectangle (x1,y1) (x2,y2)."
  "enclosed" x1 y1 x2 y2)

(canvas-find-method (canvas-find-overlapping (x1 y1 x2 y2))
    "Returs ids of all items which overlap the rectangle (x1,y1) (x2,y2)."
  "overlapping" x1 y1 x2 y2)

(canvas-find-method (canvas-find-withtag (tag))
    "Returs ids of all items inclosed with TAG."
  "withtag" tag)

(canvas-find-method (canvas-find-closest (x y &key (halo) (start)))
    "Finds the item closest to X,Y."
  "closest" x y
  (if halo (format nil "~a" halo) "")
  (if start (format nil "~a" start) ""))

(canvas-find-method (canvas-find-all ())
    "Finds all items."
  "all")

(defun canvas-focus (canvas)
  "Returns the ID of the item with keyboard focus."
  (get-response "~a focus" (window-path canvas)))

(defun (setf canvas-focus) (tid canvas)
  "Sets the keyboard focus to TID."
  (send-command "~a focus ~a" (window-path canvas) tid))

(defun canvas-gettags (canvas tid)
  "Returns the ID of the item with keyboard focus."
  (let ((r (get-response "~a gettags ~a" (window-path canvas) tid)))
    (if (string= r "")
        ()
        (split-sequence #\Space r))))

(defun canvas-icursor (canvas tid ind)
  "Sets the position of cursor in item TID."
  (send-command "~a icursor ~a ~a" (window-path canvas) tid ind))

(defun canvas-imove (canvas tid i x y)
  "Sets the I-th coordinate in item TID to X,Y."
  (send-command "~a imove ~a ~a ~a ~a" (window-path canvas) tid i x y))

(defun canvas-index (canvas tid ind)
  "Returns a numerical index for a textual index IND in item TID."
  (read-from-string (get-response "~a index ~a ~a" (window-path canvas) tid ind)))

(defun canvas-insert (canvas tid before str)
  "Inserts string STR into items TID just before index BEFORE."
  (send-command "~a insert ~a ~a ~s" (window-path canvas) tid before str))

(defun canvas-lower (canvas tid &optional below)
  "Moves items given by TID in the display below item given by BELOW."
  (send-command "~a lower ~a ~a" (window-path canvas) tid (or below "")))

(defun canvas-move (canvas id x y)
  "Moves the item with id/tag ID by x and y."
  (send-command "~a move ~a ~a ~a" (window-path canvas) id x y))

(defun canvas-moveto (canvas id x y)
  "Moves the item with id/tag ID to x and y."
  (send-command "~a moveto ~a ~a ~a" (window-path canvas) id x y))

(defun canvas-raise (canvas tid &optional above)
  "Moves items given by TID in the display above item given by ABOVE."
  (send-command "~a raise ~a ~a" (window-path canvas) tid (or above "")))

(defun canvas-rchars (canvas tid first last str)
  "Replaces the chars between FIRST and LAST in item TID with STR."
  (send-command "~a rchars ~a ~a ~a ~s" (window-path canvas) tid first last str))


(defun canvas-scale (canvas tid xorigin yorigin xscale yscale)
  "Scales the item TID.

XORIGN,YORIGIN is the origin of the scale and XSCALE, YSCALE are scale
factors."
  (send-command "~a scale ~a ~a ~a ~a ~a"
                (window-path canvas) tid
                xorigin yorigin xscale yscale))

(defun canvas-select-adjust (canvas tid ind)
  "Adjusts the selection in item TID to include the char at IND."
  (send-command "~a select adjust ~a ~a" (window-path canvas) tid ind))

(defun canvas-select-clear (canvas)
  "Clears the selection in the canvas."
  (send-command "~a select clear" (window-path canvas)))

(defun canvas-select-item (canvas)
  "Returns the selected item."
  (get-response "~a select item" (window-path canvas)))

(defun canvas-select-from (canvas tid ind)
  "Sets the selection anchor at ."
  (send-command "~a select from ~a ~a" (window-path canvas) tid ind))

(defun canvas-select-to (canvas tid ind)
  "Sets the selection from anchor to IND."
  (send-command "~a select to ~a ~a" (window-path canvas) tid ind))

(defun canvas-scan-mark (canvas x y)
  "Sets the scrolling mark."
  (send-command "~a scan mark ~a ~a" (window-path canvas) x y))

(defun canvas-scan-dragto (canvas x y &key gain)
  "Scrolls the canvas according to the scan mar."
  (send-command "~a scan dragto ~a ~a ~a" (window-path canvas) x y (or gain "")))

(defun canvas-type (canvas tid)
  "Returns the type of item TID."
  (get-response "~a type ~a" (window-path canvas) tid))

(defun canvas-xview (canvas)
  "Returns the xview of the window"
  (mapcar #'read-from-string
          (split-sequence #\Space
                          (get-response "~a xview" (window-path canvas)))))

(defun (setf canvas-xview) (ind canvas)
  "Sets the xview of the canvas so that the char at IND is visible."
  (send-command "~a xview ~a" (window-path canvas) ind))

(defun canvas-xview-moveto (canvas frac)
  "Sets the view so that the char at fraction FRAC is visible."
  (send-command "~a xview moveto ~a" (window-path canvas) frac))

(defun canvas-xview-scroll (canvas number what)
  "Scrolls the window according to NUMBER and WHAT in the x direction.

NUMBER is an integer and WHAT can be \"units\" or \"pages\"."
  (send-command "~a xview moveto ~a ~a ~a" (window-path canvas) number what))

(defun canvas-yview (canvas)
  "Returns the yview of the window"
  (mapcar #'read-from-string
          (split-sequence #\Space
                          (get-response "~a yview" (window-path canvas)))))

(defun (setf canvas-yview) (ind canvas)
  "Sets the yview of the canvas so that the char at IND is visible."
  (send-command "~a yview ~a" (window-path canvas) ind))

(defun canvas-yview-moveto (canvas frac)
  "Sets the view so that the char at fraction FRAC is visible."
  (send-command "~a yview moveto ~a" (window-path canvas) frac))

(defun canvas-yview-scroll (canvas number what)
  "Scrolls the window according to NUMBER and WHAT in the y direction.

NUMBER is an integer and WHAT can be \"units\" or \"pages\"."
  (send-command "~a yview moveto ~a ~a ~a" (window-path canvas) number what))

(defun canvas-postscript (canvas &rest options)
  "Generates a postscript representation of the CANVAS."
  (let ((cmd (format nil "~a postscript" (window-path canvas))))
    (loop for opt on options by #'cddr do
         (setf cmd (concatenate 'string cmd
                                (format nil " ~a ~a"
                                        (key-to-string (car opt))
                                        (option-to-string (cadr opt)))))
         (send-command cmd))))
