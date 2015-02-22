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


(asdf:defsystem :simple-tk
    :description "A simple TCL/TK binding for common lisp."
    :author "Andrej Vodopivec <andrej.vodopivec@gmail.com>"
    :license "MIT"
    :depends-on (:cffi
                 :split-sequence)
    :components
    ((:module "src"
              :serial t
              :components
              ((:file "package")
               (:file "cffi")
               (:file "variables")
               (:file "window")
               (:file "geometry")
               (:file "window-properties")
               (:file "simple-windows")
               (:file "menu")
               (:file "notebook")
               (:file "paned-window")
               (:file "canvas")
               (:file "treeview")
               (:file "listbox")
               (:file "bind")
               (:file "text")
               (:file "scrollbar")
               (:file "dialogs")
               (:file "image")))))
