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

(defun message-box (message &key (title "Message") (icon "info") (default "ok")
                            (detail "") (parent) (type "ok"))
  "Opens a dialog box.

Options supported are TITLE, ICON, DEFAULT, DETAUL, PARENT and
TYPE. Returns the button clicked as a string."
  (get-response "tk_messageBox -message ~s -title ~s -icon ~s -default ~s -type ~s -detail ~s -parent ~s" 
                message title icon default type detail (if parent (window-path parent) ".")))

(defun choose-color (&key (title "Choose color") (parent) (initial ""))
  "Opens a dialog for choosing a color.

Returns the color as a string."
  (get-response "tk_chooseColor -title ~s -initialcolor ~s -parent ~s"
                title initial (if parent (window-path parent) ".")))

(defun choose-directory (&key (title "Choose directory") (parent) (initial ""))
  "Opens a dialog for choosing a directory.

Returns the result as a string."
  (get-response "tk_chooseDirectory -title ~s -initialdir ~s -parent ~s"
                title initial (if parent (window-path parent) ".")))

(defun get-open-file (&key (title "Choose file") (parent) (initial ""))
  "Opens a \"file open\" dialog.

Returns the result as a string."
  (get-response "tk_getOpenFile -title ~s -initialdir ~s -parent ~s"
                title initial (if parent (window-path parent) ".")))

(defun get-save-file (&key (title "Choose file") (parent) (initial ""))
  "Opens a \"file save\" dialog.

Returns the result as a string."
  (get-response "tk_getSaveFile -title ~s -initialdir ~s -parent ~s"
                title initial (if parent (window-path parent) ".")))
