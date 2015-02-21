(defun radio-frame (nb)
  (let* ((f (tk:frame :parent nb))
         (r1 (tk:radiobutton :text "Prvi" :variable "radio" :value "prvi" :parent f))
         (r2 (tk:radiobutton :text "Drugi" :variable "radio" :value "drugi" :parent f))
         (e (tk:entry :parent f :textvariable "evar"))
         (b (tk:button :text "Value" :parent f
                        :command (lambda ()
                                   (setf (tk:var-value "evar")
                                         (tk:var-value "radio"))))))
    (tk:pack (list r1 r2 e b) :anchor "w" :padx 5 :pady 2)
    (setf (tk:var-value "radio") "prvi")
    f))

(defun scale-frame (nb)
  (let* ((f (tk:frame :parent nb))
         (s-var (tk:float-variable))
         (l (tk:label :text "Value" :parent f :textvariable "lvar"))
         (s (tk:scale :parent f
                       :variable s-var
                       :command (lambda (args)
                                  (declare (ignore args))
                                  (setf (tk:var-value "lvar")
                                        (tk:var-value s-var)))))
         (b (tk:button :text "Value" :parent f
                        :command (lambda ()
                                   (setf (tk:var-value "lvar")
                                         (tk:var-value s-var)))))
         (cb (tk:combobox :parent f
                            :text "andrej" :values (list "andrej" "vodopivec")))
         (sp-var (tk:integer-variable))
         (sp (tk:spinbox :parent f :textvariable sp-var 
                          :from "0" :to "10" :increment "2")))
    (setf (tk:var-value s-var) 0.0)
    (setf (tk:var-value "lval") "Nothing yet!")
    (setf (tk:var-value sp-var) 0)
    (tk:pack (list s l b cb sp) :pady 2 :padx 5)
    f))

(defun progress-frame (nb)
  (let* ((f (tk:frame :parent nb))
         (running nil)
         (pr-v (tk:float-variable))
         (btn-v (tk:string-variable))
         (pr (tk:progressbar :parent f :variable pr-v))
         (start (tk:button :parent f
                            :textvariable btn-v
                            :command (lambda ()
                                       (if running
                                           (progn
                                             (setf running nil)
                                             (setf (tk:var-value btn-v) "Start")
                                             (tk:progressbar-stop pr))
                                           (progn
                                             (setf running t)
                                             (setf (tk:var-value btn-v) "Stop")
                                             (tk:progressbar-start pr))))))
         (m (tk:label :parent f :textvariable pr-v)))
    (setf (tk:var-value btn-v) "Start")
    (setf (tk:var-value pr-v) 50.0)
    (tk:pack (list pr m start) :pady 2 :padx 5)
    f))

(defun canvas-frame (nb)
  (let* ((f (tk:frame :parent nb))
         (fc (tk:frame :parent f))
         (cnv (tk:canvas :parent fc :bg "white" :relief "sunken"
                          :borderwidth 2  :scrollregion '(-300 -300 300 300)))
         (hs (tk:scrollbar :orient "horizontal" :parent fc))
         (vs (tk:scrollbar :orient "vertical" :parent fc))
         (b (tk:button :text "Clear" :parent f))
         (cb (tk:button :text "Blue" :parent cnv))
         (mouse-down nil))
    (tk:bind-event cnv "<Double-Button-1>"
                    (lambda (evt)
                      (destructuring-bind (x y)
                          (tk:canvas-scrolled-coords cnv (tk:event-mouse-position evt))
                        (tk:canvas-create-oval cnv (list (- x 5) (- y 5)
                                                          (+ x 5) (+ y 5))
                                                :outline "blue" :width "2" :tags "Point"))))
    (tk:bind-event cnv "<Button-1>"
                    (lambda (evt)
                      (destructuring-bind (x y)
                          (tk:canvas-scrolled-coords cnv (tk:event-mouse-position evt))
                        (tk:canvas-itemconfig cnv "sel-tag" :outline "green")
                        (tk:canvas-scan-mark cnv x y)
                        (tk:canvas-dtag cnv "sel-tag")
                        (setf mouse-down t)
                        (tk:canvas-addtag-closest cnv "sel-tag" x y)
                  (tk:canvas-itemconfig cnv "sel-tag" :outline "yellow"))))
    (tk:bind-event cnv "<Motion>"
                    (lambda (evt)
                      (destructuring-bind (x y)
                          (tk:canvas-scrolled-coords cnv (tk:event-mouse-position evt))
                        (when mouse-down
                          (tk:canvas-scan-dragto cnv x y :gain 1)))))
    (tk:bind-event cnv "<ButtonRelease-1>"
                    (lambda (evt)
                      (declare (ignore evt))
                      (setf mouse-down nil)))
    (tk:bind-event cnv "<2>"
                    (lambda (evt)
                      (destructuring-bind (x y)
                          (tk:canvas-scrolled-coords cnv (tk:event-mouse-position evt))
                        (tk:canvas-create-oval cnv (list (- x 5) (- y 5)
                                                          (+ x 5) (+ y 5))
                                                :outline "red" :width "2"))))
    (tk:canvas-bind cnv "arc" "<Enter>"
                     (lambda (evt)
                       (print evt)
                       (tk:canvas-itemconfig cnv "arc"
                                              :outline "red")))
    (tk:canvas-create-arc cnv (list 100 100 200 200) :start "45" :tags '("arc"))
    (tk:canvas-create-window cnv (list 30 30) :window cb)
    (tk:bind-command cb (lambda ()
                           (tk:canvas-addtag-withtag cnv "all-blue" "Point")
                           (tk:canvas-itemconfig cnv "all-blue" :outline "blue")))
    (tk:bind-command b (lambda ()
                          (print (tk:canvas-find-all cnv))
                          (tk:canvas-addtag-withtag cnv "all-items" "Point")
                          (tk:canvas-delete cnv "all-items")))
    (tk:pack fc :expand "1" :fill "both")
    (tk:pack b)
    (tk:grid (list cnv vs) :sticky "nwes")
    (tk:grid hs :row 1 :sticky "nwes")
    (tk:grid-col-configure fc 0 :weight 1)
    (tk:grid-row-configure fc 0 :weight 1)
    (tk:scrollbar-connect cnv hs)
    (tk:scrollbar-connect cnv vs)
    f))

(defun text-frame (nb)
  (let* ((f (tk:frame :parent nb))
         (txt (tk:text :parent f :width "50" :height "10"))
         (vs (tk:scrollbar :parent f :orient "vertical")))
    (tk:grid (list txt vs) :sticky "nsew")
    (tk:grid-col-configure f 0 "weight" "1")
    (tk:grid-row-configure f 0 "weight" "1")
    (tk:scrollbar-connect txt vs)
    (tk:text-insert txt "insert" "Andrej")
    (print (tk:text-search txt "nd" "0.1"))
    (print (tk:text-bbox txt "0.1"))
    (tk:text-tag-add txt "an" "1.1" "1.3")
    (tk:text-tag-config txt "an" :background "yellow")
    (tk:text-tag-bind txt "an" "<Enter>"
                       (lambda (ev)
                         (declare (ignore ev))
                         (print "Tag enter")
                         (tk:text-tag-config txt "an" :underline t)))
    (tk:text-tag-bind txt "an" "<Leave>"
                       (lambda (ev)
                         (declare (ignore ev))
                         (print "Tag leave")
                         (tk:text-tag-config txt "an" :underline nil)))
    (tk:after-idle (lambda () (print (tk:text-bbox txt "0.2"))))
    f))

(defun entry-frame (nb)
  (let* ((t-var (tk:string-variable))
         (f (tk:frame :parent nb))
         (e (tk:entry :parent f :textvariable t-var :show "*"))
         (l (tk:label :parent f :textvariable "tva1"))
         (m (tk:message :parent f :textvariable t-var :width 400)))
    (tk:pack (list e l m) :pady 2 :padx 5)
    (tk:bind-event e "<KeyPress>"
                    (lambda (evt)
                      (setf (tk:var-value "tva1") (tk:event-key-code evt))))
    f))

(defun theme-frame (nb)
  (let* ((themes (tk:get-tk-themes))
         (f (tk:frame :parent nb))
         (b (tk:menubutton :parent f :text "Select theme"))
         (m (tk:menu :parent b)))
    (dolist (theme themes)
      (tk:menu-add-command m theme
                           (lambda () (tk:set-tk-theme theme))))
    (tk:window-configure b :menu m)
    (tk:pack b :pady 5)
    f))

(defun treeview-frame (nb)
  (let* ((tw (tk:treeview :parent nb :columns (list "one" "two"))))
    (setf (tk:treeview-column-width tw "one") 100)
    (setf (tk:treeview-column-width tw "two") 100)
    (setf (tk:treeview-heading-text tw "one") "Column A")
    (setf (tk:treeview-heading-text tw "two") "Column B")
    (let ((id (tk:treeview-insert tw "" 0 :text "Line 1" :values (list "1" "2"))))
      (tk:treeview-insert tw id 0 :text "Line 2" :values (list "11" "22"))
      (tk:treeview-selection-set tw id))
    (tk:bind-event tw "<<TreeviewSelect>>"
                    (lambda (ev)
                      (declare (ignore ev))
                      (print (tk:treeview-selection tw))))
    tw))

(defun listbox-frame (nb)
  (let* ((f (tk:frame :parent nb))
         (l (tk:listbox :parent f)))
    (tk:pack l :fill "x" :expand t :anchor "n" :padx 10 :pady 10)
    (tk:listbox-insert l 0 "One" "Two" "Three" "Four")
    (tk:bind-event l "<<ListboxSelect>>"
                    (lambda (ev)
                      (declare (ignore ev))
                      (print (tk:listbox-curselection l))))
    f))

(defun demo ()
  (tk:with-tk (:title "Demo")
    (let* ((nb (tk:notebook))
           (mbar (tk:menu))
           (file (tk:menu :parent mbar))
           (radios (tk:menu :parent file)))
      (setf (tk:window-minsize nil) (list 400 400))
      (tk:menu-add-cascade mbar "File" file)
      (tk:menu-add-radio radios "Prvi" "radio" "prvi")
      (tk:menu-add-radio radios "Drugi" "radio" "drugi")
      (tk:menu-add-cascade file "Selection" radios)
      (tk:menu-add-command file "Hey"
                           (lambda () (print (tk:message-box "Hello!"))))
      (tk:menu-add-command file"Choose Color"
                           (lambda () (print (tk:choose-color :initial "white"))))
      (tk:menu-add-command file "File Open"
                           (lambda () (print (tk:get-open-file))))
      (tk:menu-add-command file "Select Dir"
                           (lambda () (print (tk:choose-directory))))
      (tk:menu-add-command file "Save File"
                           (lambda () (print (tk:get-save-file))))
      (tk:menu-add-separator file)
      (tk:menu-add-command file "Exit" (lambda () (tk:window-destroy nil)))
      (tk:menu-toplevel mbar)
      (tk:notebook-add nb (radio-frame nb) :text "Radio")
      (tk:notebook-add nb (scale-frame nb) :text "Scale")
      (tk:notebook-add nb (canvas-frame nb) :text "Canvas" :sticky "nsew")
      (tk:notebook-add nb (entry-frame nb) :text "Entry")
      (tk:notebook-add nb (text-frame nb) :text "Text")
      (tk:notebook-add nb (theme-frame nb) :text "Themes")
      (tk:notebook-add nb (progress-frame nb) :text "Progress")
      (tk:notebook-add nb (treeview-frame nb) :text "Treeview")
      (tk:notebook-add nb (listbox-frame nb) :text "Listbox")
      (tk:pack nb :expand "1" :fill "both")
      (setf (tk:notebook-select nb) (nth 4 (tk:notebook-tabs nb)))
      (print (tk:tk-version))
      (tk:after 2000 (lambda () (print "Hello!!"))))))