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

(define-tk-window (menu "menu" "mn" window))

(defun menu-popup (m x y)
  "Shows the menu M at X,Y."
  (send-command "tk_popup ~a ~a ~a"
                (window-path m) x y))

(defun menu-add-command (m label fun &rest options)
  "Adds a new command item to the menu.

FUN is the function with no arguments which is called then the menu is selected."
  (let ((id (string-downcase (format nil "~a.mcmd~a" (window-path m) (next-id)))))
    (send-command "~a add command -label ~s -command \{call_lisp ~a} ~a"
                  (window-path m) label id (options-conf-string options))
    (when (functionp fun)
      (setf (gethash id *event-table*) fun))))

(defun menu-add-radio (m label variable value &rest options)
  "Adds a new radio item to the menu.

VARIABLE and VALUE are used to check if the radio is selected."
  (send-command "~a add radio -label ~s -variable ~a -value ~a ~a"
                (window-path m) label variable value (options-conf-string options)))

(defun menu-add-separator (m)
  "Adds a separator to the menu M."
  (send-command "~a add separator" (window-path m)))

(defun menu-add-cascade (m label submenu &rest options)
  "Adds a new SUBMENU to the menu M."
  (send-command "~a add cascade -menu ~a ~a ~a"
                (window-path m) (window-path submenu)
                (if (string= label "") "" (format nil "-label ~s" label))
                (options-conf-string options)))

(defun menu-add-checkbutton (m label variable &rest options)
  "Adds a new checkbutton to the menu M.

VARIABLE is the tcl variable which can be used to check the state of
menu item."
  (send-command "~a add checkbutton -label ~s -variable ~a ~a"
                (window-path m) label variable (options-conf-string options)))

(defun menu-delete (m start &optional end)
  "Deletes entries from M."
  (send-command "~a delete ~a ~a" (window-path m) start (or end "")))

(defun menu-insert-command (m ind label fun)
  "Inserts a new command item to the menu.

FUN is the function with no arguments which is called then the menu is selected."
  (let ((id (string-downcase (format nil "~a.mcmd~a" (window-path m) (next-id)))))
    (send-command "~a insert ~a command -label ~s -command \{call_lisp ~a}"
                  (window-path m) ind label id)
    (when (functionp fun)
      (setf (gethash id *event-table*) fun))))

(defun menu-insert-radio (m ind label variable value &rest options)
  "Inserts a new radio item to the menu.

VARIABLE and VALUE are used to check if the radio is selected."
  (send-command "~a insert ~a radio -label ~s -variable ~a -value ~a ~a"
                (window-path m) ind label variable value
                (options-conf-string options)))

(defun menu-insert-separator (m ind)
  "Inserts a separator to the menu M."
  (send-command "~a insert ~a separator" (window-path m) ind))

(defun menu-insert-cascade (m ind label submenu &rest options)
  "Inserts a new SUBMENU to the menu M."
  (send-command "~a insert ~a cascade -menu ~a ~a ~a"
                (window-path m) ind (window-path submenu)
                (if (string= label "") "" (format nil "-label ~s" label))
                (options-conf-string options)))

(defun menu-insert-checkbutton (m ind label variable &rest options)
  "Inserts a new checkbutton to the menu M.

VARIABLE is the tcl variable which can be used to check the state of
menu item."
  (send-command "~a insert ~a checkbutton -label ~s -variable ~a ~a"
                (window-path m) ind label variable (options-conf-string options)))

(defun menu-activate (m ind)
  "Activates the entry at index IND."
  (send-command "~a activate ~a" (window-path m) ind))

(defun menu-entrycget (m ind opt)
  "Returns the value of the option OPT for the entry at index IND."
  (get-response "~a entrycget ~a ~a" (window-path m) ind (key-to-string opt)))

(defun menu-entryconfigure (m ind &rest options)
  "Configures the entry at index IND with OPTIONS."
  (loop for (opt val) on options by #'cddr do
       (send-command "~a entryconfigure ~a -~a ~a"
                     (window-path m) ind (key-to-string opt) (option-to-string val))))

(defun menu-index (m ind)
  "Returns the numerical index for index IND."
  (get-response "~a index ~a" (window-path m) ind))

(defun menu-invoke (m ind)
  "Invokes the entry at index IND."
  (send-command "~a invoke ~a" (window-path m) ind))


