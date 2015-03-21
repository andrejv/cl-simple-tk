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

(defun image-create-bitmap (&key (name) (background) (data) (file)
                              (foreground) (maskdata) (maskfile))
  "Creates a bitmap image."
  (get-response "image create bitmap ~a ~a ~a ~a ~a ~a ~a"
                (if name       (format nil "-name ~a"       name)       "")
                (if background (format nil "-background ~a" background) "")
                (if data       (format nil "-data ~s"       data)       "")
                (if file       (format nil "-file ~s"       file)       "")
                (if foreground (format nil "-foreground ~a" foreground) "")
                (if maskdata   (format nil "-maskdata ~a"   maskdata)   "")
                (if maskfile   (format nil "-maskfile ~a"   maskfile)   "")))

(defun image-create-photo (&key (data) (format) (file) (gamma)
                             (height) (palette) (width))
  "Creates a photo image."
  (get-response "image create photo ~a ~a ~a ~a ~a ~a ~a"
                (if data    (format nil "-data ~a"    data)    "")
                (if format  (format nil "-format ~s"  format)  "")
                (if file    (format nil "-file ~s"    file)    "")
                (if gamma   (format nil "-gamma ~a"   gamma)   "")
                (if height  (format nil "-height ~a"  height)  "")
                (if palette (format nil "-palette ~a" palette) "")
                (if width   (format nil "-width ~a"   width)   "")))

(defun image-blank (img)
  "Blanks the image IMG."
  (send-command "~a black" (window-path img)))

(defun image-cget (img opt)
  "Returns the value of the option OPT."
  (get-response "~a cget ~a" img (key-to-string opt)))

(defun image-configure (img &rest options)
  "Configures the image according to OPTIONS."
  (loop for opt on options by #'cddr do
       (send-command "~a configure ~a ~a" img (key-to-string (car opt))
                     (option-to-string (cadr opt)))))


(defun image-copy (img source &key (from) (to) (shrink) (zoom) (subsample)
                                (compositingrule))
  "Copies to IMG from SOURCE."
  (send-command "~a copy ~a ~a ~a ~a ~a ~a ~a ~a ~a"
                img source
                (if from      (format nil "-from ~{~a~^ ~}"      from)      "")
                (if to        (format nil "-to ~{~a~^ ~}"        to)        "")
                (if shrink    (format nil "-shrink")                        "")
                (if zoom      (format nil "-zoom ~{~a~^ ~}"      zoom)      "")
                (if subsample (format nil "-subsample ~{~a~^ ~}" subsample) "")
                (if compositingrule
                    (format nil "-compositingrule ~a" compositingrule) "")))

(defun image-data (img &key (background) (format) (from) (grayscale))
  "Returns the image data as a string."
  (get-response "~a get ~a ~a ~a ~a"
                img
                (if background (format nil "-background ~a"  background) "")
                (if format     (format nil "-format ~a"      format)     "")
                (if from       (format nil "-from ~{~a~^ ~}" from)       "")
                (if grayscale  (format nil "-grayscale")                 "")))

(defun image-get (img x y)
  "Returns the color of the pixel at X,Y."
  (get-response "~a get ~a ~a" img x y))

(defun image-read (img filename &key (format) (from) (to) (shrink))
  "Reads image data for IMG from the file FILENAME."
  (send-command "~a read ~a ~a ~a ~a ~a"
                img filename
                (if from      (format nil "-from ~{~a~^ ~}" from)    "")
                (if to        (format nil "-to ~{~a~^ ~}"   to)      "")
                (if shrink    (format nil "-shrink")                 "")
                (if format    (format nil "-format ~a"       format) "")))

(defun image-redither (img)
  "Redither the image"
  (send-command "~a redither" img))

(defun image-transparency (img x y)
  "Checks if the pixel at X,Y is transparent."
  (string= "1" (get-response "~a transparency get ~a ~a" img x y)))

(defun (setf image-transparency) (val img x y)
  "Sets the transparency at pixel X,Y."
  (send-command "~a transpacency set ~a ~a ~a"
                img x y (if val "1" "0")))

(defun image-write (img filename &key (background) (format) (from) (grayscale))
  "Writes the image data from IMG to the file FILENAME."
  (send-command "~a write ~a ~a ~a ~a ~a"
                img filename
                (if from       (format nil "-from ~{~a~^ ~}" from)       "")
                (if background (format nil "-background ~a"  background) "")
                (if format     (format nil "-format ~a"      format)     "")
                (if grayscale  (format nil "-grayscale")                 "")))
