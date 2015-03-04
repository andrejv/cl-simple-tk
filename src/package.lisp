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

(defpackage :simple-tk
  (:use :cl
        :split-sequence
        :cffi)
  (:nicknames :tk)
  (:documentation
"This package is a simple wrapper for the TCL interpreter and TK gui. 

For a more detailed information about functions look at the tcl/tk 
documentation. For instance, the method @code{canvas-addtag-withtag} 
corresonds to the command @code{addtag}, subcommand @code{withtag} on 
the canvas object. The call 
@code{(canvas-addtag-withtag cnv ntag otag)} is transformed into the tcl 
call @code{cnv addtag ntag withtag otag}.")
  (:export :after
           :after-cancel
           :after-idle
           :bind-command
           :bind-event
           :boolean-variable
           :button
           :button-invoke
           :canvas
           :canvas-addtag
           :canvas-addtag-above
           :canvas-addtag-all
           :canvas-addtag-blow
           :canvas-addtag-closest
           :canvas-addtag-enclosed
           :canvas-addtag-overlapping
           :canvas-addtag-withtag
           :canvas-bbox
           :canvas-bind
           :canvas-canvasx
           :canvas-canvasy
           :canvas-coords
           :canvas-create
           :canvas-create-arc
           :canvas-create-bitmap
           :canvas-create-image
           :canvas-create-line
           :canvas-create-oval
           :canvas-create-photo
           :canvas-create-poylgon
           :canvas-create-rectangle
           :canvas-create-text
           :canvas-create-window
           :canvas-dchars
           :canvas-delete
           :canvas-dtag
           :canvas-find
           :canvas-find-above
           :canvas-find-all
           :canvas-find-below
           :canvas-find-closest
           :canvas-find-enclosed
           :canvas-find-overlapping
           :canvas-find-withtag
           :canvas-focus
           :canvas-gettags
           :canvas-icursor
           :canvas-imove
           :canvas-index
           :canvas-insert
           :canvas-image-cget
           :canvas-image-configure
           :canvas-image-names
           :canvas-itemcget
           :canvas-itemconfig
           :canvas-lower
           :canvas-move
           :canvas-moveto
           :canvas-postscript
           :canvas-raise
           :canvas-rchars
           :canvas-scale
           :canvas-scan-dragto
           :canvas-scan-mark
           :canvas-scrolled-coords
           :canvas-select-adjust
           :canvas-select-clear
           :canvas-select-from
           :canvas-select-item
           :canvas-select-to
           :canvas-type
           :canvas-xview
           :canvas-xview-moveto
           :canvas-xview-scroll
           :canvas-yview
           :canvas-yview-moveto
           :canvas-yview-scroll
           :checkbutton
           :choose-color
           :choose-directory
           :clipboard-append
           :clipboard-clear
           :clipboard-get
           :combobox
           :configure-window
           :create-command
           :entry
           :entry-bbox
           :entry-delete
           :entry-get
           :entry-icursor
           :entry-index
           :entry-insert
           :entry-selection-clear
           :entry-selection-present
           :entry-selection-range
           :entry-validate
           :entry-xview
           :entry-xview-moveto
           :entry-xview-scroll
           :event-key-code
           :event-mouse-position
           :event-window-path
           :float-variable
           :frame
           :get-open-file
           :get-save-file
           :get-tk-themes
           :grid
           :grid-col-configure
           :grid-row-configure
           :image-blank
           :image-cget
           :image-configure
           :image-copy
           :image-create-bitmap
           :image-create-photo
           :image-data
           :image-get
           :image-read
           :image-redither
           :image-transparency
           :image-write
           :integer-variable
           :label
           :label-frame
           :listbox
           :listbox-activate
           :listbox-bbox
           :listbox-curselection
           :listbox-delete
           :listbox-insert
           :listbox-scan-dragto
           :listbox-scan-mark
           :listbox-see
           :listbox-xview
           :listbox-xview-moveto
           :listbox-xview-scroll
           :listbox-yview
           :listbox-yview-moveto
           :listbox-yview-scroll
           :menu
           :menu-add-cascade
           :menu-add-checkbutton
           :menu-add-command
           :menu-add-radio
           :menu-add-separator
           :menu-popup
           :menu-toplevel
           :menubutton
           :message
           :message-box
           :notebook
           :notebook-add
           :notebook-forget
           :notebook-hide
           :notebook-identify-element
           :notebook-identify-tab
           :notebook-index
           :notebook-insert
           :notebook-select
           :notebook-tab
           :notebook-tabs
           :pack
           :panedwindow
           :panedwindow-add
           :panedwindow-forget
           :panedwindow-identify-element
           :panedwindow-identify-sash
           :panedwindow-insert
           :panedwindow-sashpos
           :place
           :progressbar
           :progressbar-start
           :progressbar-step
           :progressbar-stop
           :radiobutton
           :scale
           :scrollbar
           :scrollbar-connect
           :separator
           :set-tk-theme
           :spinbox
           :string-variable
           :text
           :text-bbox
           :text-count-chars
           :text-count-displaychars
           :text-count-displayindices
           :text-count-displaylines
           :text-count-indices
           :text-count-lines
           :text-count-xpixels
           :text-count-ypixels
           :text-delete
           :text-dlineinfo
           :text-edit-modified
           :text-edit-redo
           :text-edit-reset
           :text-edit-separator
           :text-edit-undo
           :text-get
           :text-get-displaychars
           :text-index
           :text-insert
           :text-image-cget
           :text-image-configure
           :text-image-create
           :text-image-names
           :text-mark-gravity
           :text-mark-names
           :text-mark-next
           :text-mark-previous
           :text-mark-set
           :text-mark-unset
           :text-replace
           :text-search
           :text-see
           :text-tag-add
           :text-tag-bind
           :text-tag-cget
           :text-tag-config
           :text-tag-delete
           :text-tag-lower
           :text-tag-names
           :text-tag-raise
           :text-window-create
           :text-window-cget
           :text-window-configure
           :text-window-names
           :text-xview
           :text-xview-moveto
           :text-xview-scroll
           :text-yview
           :text-yview-moveto
           :text-yview-scroll
           #+darwin :tk-mac-about-panel
           #+darwin :tk-mac-show-help
           #+darwin :tk-mac-show-preferences
           #+darwin :tk-mac-quit
           :tk-version
           :toplevel
           :treeview
           :treeview-bbox
           :treeview-children
           :treeview-column-anchor
           :treeview-column-id
           :treeview-column-minwidth
           :treeview-column-stretch
           :treeview-column-width
           :treeview-delete
           :treeview-detach
           :treeview-exists
           :treeview-focus
           :treeview-get
           :treeview-heading-anchor
           :treeview-heading-image
           :treeview-heading-text
           :treeview-insert
           :treeview-item-id
           :treeview-item-text
           :treeview-item-values
           :treeview-move
           :treeview-next
           :treeview-parent
           :treeview-prev
           :treeview-selection
           :treeview-selection-add
           :treeview-selection-remove
           :treeview-selection-set
           :treeview-selection-toggle
           :treeview-set
           :var-value
           :window-cget
           :window-configure
           :window-destroy
           :window-geometry
           :window-identify
           :window-identify-element
           :window-minsize
           :window-resizable
           :window-selected
           :window-state
           :window-withdraw
           :with-tk))

