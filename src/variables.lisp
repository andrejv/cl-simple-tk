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

(defclass tcl-var ()
  ((name
    :initform (format nil "tcl_var~a" (next-id))
    :initarg :name
    :reader tcl-var-name
    :documentation "The tcl name of the variable.")
   (var-type
    :initform :string
    :initarg :type
    :reader tcl-var-type))
  (:documentation
   "A simple wrapper for a tcl variable. Use INTEGER-VARIABLE,
   STRING-VARIABLE, FLOAT-VARIABLE or BOOLEAN-VARIABLE functions to
   create variable.

   The value of the variable is read with the VAR-VALUE function. Use
   the SETF macro to set the value."))

(defun tcl-variable (&key (type :string))
  "Creates a new TCL-VAR with name NAME.

TYPE can be :STRING (default), :INTEGER or :FLOAT."
  (make-instance 'tcl-var :type type))

(defun integer-var ()
  "Defines an integer tcl variable.

The value of the variable is obtaines with VAR-VALUE function and can
be modified with SETF."
  (tcl-variable :type :integer))

(defun string-var ()
  "Defines a string tcl variable.

The value of the variable is obtaines with VAR-VALUE function and can
be modified with SETF."
  (tcl-variable :type :string))

(defun float-var ()
  "Defines a float tcl variable.

The value of the variable is obtaines with VAR-VALUE function and can
be modified with SETF."
  (tcl-variable :type :float))

(defun boolean-var ()
  "Defines a string tcl variable.

The value of the variable is obtaines with VAR-VALUE function and can
be modified with SETF."
  (tcl-variable :type :boolean))

(defun var-value (var)
  "Returns the value of a tcl variable VAR.

VAR can be a string or an instance of a tcl variable."
  (if (stringp var)
      (get-variable var)
      (case (tcl-var-type var)
        (:integer
         (parse-integer (get-variable (tcl-var-name var))))
        (:float
         (float (read-from-string (get-variable (tcl-var-name var)))))
        (:boolean
         (if (string= (get-variable (tcl-var-name var)) "0") nil t))
        (otherwise
         (get-variable (tcl-var-name var))))))

(defun (setf var-value) (val var)
  "Sets the tcl variable VAR to VAl.

VAR can be a string of a TCL-VAR."
  (if (stringp var)
      (set-variable var val)
      (if (eq (tcl-var-type var) :boolean)
          (if val
              (set-variable (tcl-var-name var) "1")
              (set-variable (tcl-var-name var) "0"))
          (set-variable (tcl-var-name var) val))))
