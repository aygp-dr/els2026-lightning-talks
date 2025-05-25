(defpackage :lisp-web-components/transpiler
  (:use :cl :lisp-web-components)
  (:export :transpile-to-javascript :generate-component-library))

(in-package :lisp-web-components/transpiler)

(defun transpile-to-javascript (lisp-form)
  "Transpile Lisp forms to equivalent JavaScript"
  (cond
    ((null lisp-form) "null")
    ((atom lisp-form)
     (typecase lisp-form
       (string (format nil "\"~A\"" lisp-form))
       (keyword (format nil "\"~A\"" (string-downcase (symbol-name lisp-form))))
       (symbol (string-downcase (symbol-name lisp-form)))
       (number (princ-to-string lisp-form))
       (t (princ-to-string lisp-form))))
    ((listp lisp-form)
     (case (first lisp-form)
       ('if (format nil "(~A ? ~A : ~A)"
                    (transpile-to-javascript (second lisp-form))
                    (transpile-to-javascript (third lisp-form))
                    (transpile-to-javascript (fourth lisp-form))))
       ('+ (format nil "(~{~A~^ + ~})"
                   (mapcar #'transpile-to-javascript (rest lisp-form))))
       ('- (format nil "(~{~A~^ - ~})"
                   (mapcar #'transpile-to-javascript (rest lisp-form))))
       ('format (format nil "`~A`"
                        (substitute-format-args (second lisp-form) (cddr lisp-form))))
       ('let (let ((bindings (second lisp-form))
                   (body (cddr lisp-form)))
               (format nil "(() => {~%~{  let ~A = ~A;~%~}~{  ~A;~%~}~})()"
                       (loop for (var val) in bindings
                             collect (string-downcase (symbol-name var))
                             collect (transpile-to-javascript val))
                       (mapcar #'transpile-to-javascript body))))
       (t (format nil "~A(~{~A~^, ~})"
                  (string-downcase (symbol-name (first lisp-form)))
                  (mapcar #'transpile-to-javascript (rest lisp-form))))))))

(defun generate-js-component (component-name props body)
  "Generate JavaScript class for a Lisp component"
  (format nil "
class ~A extends Component {
  constructor(props) {
    super(props);
    ~{this.~A = props.~A || null;~%    ~}
  }
  
  render() {
    const {~{~A~^, ~}} = this.props;
    return ~A;
  }
}

// Register component for server-side rendering compatibility
if (typeof module !== 'undefined') {
  module.exports.~A = ~A;
}"
          (string-capitalize (symbol-name component-name))
          (loop for prop in props
                collect (string-downcase (symbol-name prop))
                collect (string-downcase (symbol-name prop)))
          (mapcar (lambda (p) (string-downcase (symbol-name p))) props)
          (transpile-dom-to-jsx (first body))
          (string-capitalize (symbol-name component-name))
          (string-capitalize (symbol-name component-name))))

(defun transpile-dom-to-jsx (dom-expr)
  "Convert Lisp DOM expression to JSX"
  (cond
    ((atom dom-expr) (format nil "{~A}" (transpile-to-javascript dom-expr)))
    ((keywordp (first dom-expr))
     (let ((tag (string-downcase (symbol-name (first dom-expr))))
           (attrs '())
           (children '()))
       ;; Parse attributes and children
       (loop for rest on (cdr dom-expr)
             while (keywordp (first rest))
             do (push (format nil "~A={~A}"
                              (string-downcase (symbol-name (first rest)))
                              (transpile-to-javascript (second rest)))
                      attrs)
                (pop rest)
                (pop rest)
             finally (setf children rest))
       (format nil "<~A~@[ ~{~A~^ ~}~]~@[>~{~A~}~]~@[</~A>~]"
               tag
               (when attrs (reverse attrs))
               (when children (mapcar #'transpile-dom-to-jsx children))
               (when children tag))))
    (t (transpile-to-javascript dom-expr))))
