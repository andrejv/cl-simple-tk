SIMPLE-TK
==========

A very simple wrapper for `tcl/tk` in common lisp inspired by the python `tkinter` package.

## Example

A small example of a window with one button.

```lisp
(with-tk-root (r)
  (setf (window-title r) "Hi!")
  (pack (button :parent r
                :text "Hello world"
                :command (lambda ()
                           (window-destroy r)))))
```
