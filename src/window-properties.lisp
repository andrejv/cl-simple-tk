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

(defun window-instate (w spec)
  "Checks if the window W is in state SPEC."
  (string= "1" (get-response "~a instate ~a" (window-path w) spec)))

(defun window-resizable (w)
  "Checks if the window W is resizable."
  (let* ((r (get-response "wm resizable ~a" (window-path w)))
         (r-split (split-sequence #\Space r)))
    (list (string= "1" (car r-split))
          (string= "1" (cadr r-split)))))

(defun (setf window-resizable) (r w)
  "Sets the resizable property for window W.

R should be a list with two boolean values, first for vertical and
second for horihontal property."
  (send-command "wm resizable ~a ~s ~s"
                (window-path w)
                (if (car r) "1" "0")
                (if (cadr r) "1" "0")))

(defun window-identify-element (w x y)
  "Identifies the subwindow in W at coordinates X,Y."
  (get-response "~a identify element ~a ~a" (window-path w) x y))

(defun window-geometry (w)
  "Checks if the window W is resizable."
  (let* ((r (get-response "wm geometry ~a" (window-path w)))
         (r-split (split-sequence #\+ r))
         (wh (split-sequence #\x (car r-split))))
    (list (parse-integer (car wh))
          (parse-integer (cadr wh))
          (parse-integer (cadr r-split))
          (parse-integer (caddr r-split)))))

(defun (setf window-geometry) (g w)
  "Sets the geometry of the window W.

G lis a list of ((w h) x y)."
  (if (stringp g)
      (send-command "wm geometry ~a ~a" (window-path w) g)
      (let ((size (if (car g)
                      (format nil "~ax~a" (caar g) (cadar g))
                      ""))
            (pos (if (cdr g)
                     (format nil "+~a+~a" (cadr g) (caddr g))
                     "")))
        (send-command "wm geometry ~a ~a~a" (window-path w) size pos))))

(defun window-children (w)
  "Returns the list of children of the window W."
  (let ((r (get-response "winfo children ~a" (window-path w))))
    (if (string= r "")
        ()
        (mapcar #'window-from-path
                (split-sequence #\Space r)))))

(defun window-screenwidth (w)
  "Returns the width of the screen on which W is displayed."
  (parse-integer (get-response "winfo screenwidth ~a" (window-path w))))

(defun window-screenheight (w)
  "Returns the height of the screen on which W is displayed."
  (parse-integer (get-response "winfo screenheight ~a" (window-path w))))

(defun window-width (w)
  "Returns the width of the window W."
  (parse-integer (get-response "winfo width ~a" (window-path w))))

(defun window-height (w)
  "Returns the height of the window W."
  (parse-integer (get-response "winfo width ~a" (window-path w))))

(defun window-reqwidth (w)
  "Returns the requested width of the window W."
  (parse-integer (get-response "winfo reqwidth ~a" (window-path w))))

(defun window-reqheight (w)
  "Returns the requested height of the window W."
  (parse-integer (get-response "winfo reqwidth ~a" (window-path w))))

(defun window-minsize (w)
  "Returns the minsize of the window."
  (mapcar #'parse-integer
          (get-response "wm minsize ~a" (window-path w))))

(defun (setf window-minsize) (l w)
  "Sets the minsize of the window."
  (send-command "wm minsize ~a ~a ~a" (window-path w) (car l) (cadr l)))

(defun window-maxsize (w)
  "Returns the maxsize of the window."
  (mapcar #'parse-integer
          (get-response "wm maxsize ~a" (window-path w))))

(defun (setf window-maxsize) (l w)
  "Sets the maxsize of the window."
  (send-command "wm maxsize ~a ~a ~a" (window-path w) (car l) (cadr l)))

(defun window-withdraw (w)
  "Withdraws the window W."
  (send-command "wm withdraw ~a" (window-path w)))

(defun window-destroy (w)
  "Withdraws the window W.

W can be nil for the root window \".\""
  (if (null w)
      (send-command "destroy .")
      (send-command "destroy ~a" (window-path w))))

(defun window-iconphoto (w photo &key (default nil))
  "Sets the icon for the window W to the named image PHOTO.

If :default is t, sets the icon for subwindows."
  (send-command "wm iconphoto ~a ~a ~a"
                (window-path w)
                (if default "-default" "")
                photo))

(defun window-iconify (w)
  "Iconifies the window W."
  (send-command "wm iconify ~a" (window-path w)))

(defun window-deiconify (w)
  "Deiconifies the window w."
  (send-command "wm deiconify ~a" (window-path w)))

(defun window-title (w)
  "Returns the title of the window W."
  (get-response "wm title ~a" (window-path w)))

(defun (setf window-title) (title w)
  "Sets the title of the window W."
  (send-command "wm title ~a ~s" (window-path w) title))

(defun window-pointerx (w)
  "Returns the X coordinate of the mouse if it is on the same screen as W."
  (parse-integer (get-response "winfo pointerx ~a" (window-path w))))

(defun window-pointerxy (w)
  "Returns the coordinates of the mouse if it is on the same screen as W."
  (mapcar #'parse-integer
          (split-sequence #\Space (get-response "winfo pointerxy ~a" (window-path w)))))

(defun window-pointery (w)
  "Returns the X coordinate of the mouse if it is on the same screen as W."
  (parse-integer (get-response "winfo pointery ~a" (window-path w))))

(defun window-raise (w &optional above)
  "Raises the window W (above ABOVE)."
  (send-command "raise ~a ~a" (window-path w) (if above (window-path above) "")))

(defun window-lower (w &optional below)
  "Lowers the window W (below BELOW)."
  (send-command "lower ~a ~a" (window-path w) (if below (window-path below) "")))

(defun window-focus (&optional w)
  "Sets the input focus to W."
  (if w
      (send-command "focus ~a" (window-path w))
      (get-response "focus")))

(defun get-tk-themes ()
  "Returns a list of Tk themes."
  (let ((themes (get-response "ttk::style theme names")))
    (split-sequence #\Space themes)))

(defun set-tk-theme (theme)
  "Sets the Tk Theme."
  (send-command "ttk::style theme use ~a" theme))

(defun font-create (&key name family size weight slant underline overstrike)
  "Creates the a new font. Returns the name of the font."
  (get-response "font create ~a ~a ~a ~a ~a ~a ~a"
                (if name       (format nil "~s" name)               "")
                (if family     (format nil "-family ~s"     family) "")
                (if size       (format nil "-size ~s"       size)   "")
                (if weight     (format nil "-weight ~s"     weight) "")
                (if slant      (format nil "-slant ~s"      slant)  "")
                (if underline  (format nil "-underline ~s"  family) "")
                (if overstrike (format nil "-overstrike ~s" family) "")))

(defun font-delete (&rest fonts)
  "Deletes the FONTS."
  (send-command "font delete ~{~s~^ ~}" fonts))

(defun font-configure (font &rest options)
  "Configures the named FONT."
  (loop for opt on options by #'cddr do
       (send-command "font configure ~s -~a ~s"
                     font (key-to-string (car opt)) (option-to-string (cadr opt)))))
