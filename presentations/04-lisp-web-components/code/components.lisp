(defpackage :lisp-web-components
  (:use :cl)
  (:export :defcomponent :render :mount :update-state :emit-event))

(in-package :lisp-web-components)

(defmacro defcomponent (name (&rest props) &body body)
  "Define a web component that works in both server and browser environments"
  `(progn
     (defclass ,name ()
       ,(mapcar (lambda (prop)
                  `(,prop :initarg ,(intern (symbol-name prop) :keyword)
                          :accessor ,(intern (format nil "~A-~A" name prop))))
                props))
     
     (defmethod render ((component ,name))
       (let (,@(mapcar (lambda (prop)
                         `(,prop (,(intern (format nil "~A-~A" name prop)) component)))
                       props))
         ,@body))
     
     (defmethod component-name ((component ,name))
       ,(string-downcase (symbol-name name)))
     
     ;; Generate JavaScript version for browser
     #+transpile-to-js
     (generate-js-component ',name ',props ',body)))

;; Example component definition
(defcomponent todo-item (text completed-p id)
  `(:div :class ,(if completed-p "todo-item completed" "todo-item")
         (:input :type "checkbox" 
                 :checked ,completed-p
                 :onchange ,(format nil "toggleTodo('~A')" id))
         (:span :class "todo-text" ,text)
         (:button :class "delete-btn"
                  :onclick ,(format nil "deleteTodo('~A')" id)
                  "×")))

(defcomponent todo-list (items)
  `(:div :class "todo-list"
         (:h2 "My Todo List")
         (:ul ,@(mapcar (lambda (item)
                          (render (make-instance 'todo-item
                                                 :text (getf item :text)
                                                 :completed-p (getf item :completed-p)
                                                 :id (getf item :id))))
                        items))
         (:form :onsubmit "addTodo(event)"
                (:input :type "text" :id "new-todo" :placeholder "Add new todo...")
                (:button :type "submit" "Add"))))

(defun render-to-html (component)
  "Render component to HTML string (server-side)"
  (labels ((serialize-element (element)
             (if (atom element)
                 (html-escape (princ-to-string element))
                 (case (first element)
                   (:text (html-escape (second element)))
                   (t (let ((tag (first element))
                            (attrs '())
                            (children '()))
                        ;; Parse attributes and children
                        (loop for item in (rest element)
                              if (keywordp item)
                                do (push item attrs)
                                   (push (pop (rest element)) attrs)
                              else
                                do (push item children))
                        (format nil "<~A~{ ~A=\"~A\"~}~@[>~{~A~}~]~@[</~A>~]"
                                (string-downcase (symbol-name tag))
                                (reverse attrs)
                                (when children 
                                  (mapcar #'serialize-element (reverse children)))
                                (when children (string-downcase (symbol-name tag))))))))))
    (serialize-element (render component))))
