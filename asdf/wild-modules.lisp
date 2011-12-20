(in-package :asdf)

(defclass wild-module (module)
  ((component-class :accessor wild-module-component-class
                    :initform 'static-file :initarg :component-class)
   (component-options :accessor wild-module-component-options
                      :initform nil :initarg :component-options)))

(defmethod (setf module-components) (new-value (module wild-module))
  (when  new-value
    (sysdef-error "Cannot explicitly set wild-module ~A's components. Please ~
use a wild pathname instead." module)))

(defmethod reinitialize-instance :after ((self wild-module) &key)
  (let ((pathname (component-pathname self)))
    (unless (and pathname (wild-pathname-p pathname))
      (sysdef-error "Wild-module ~A specified with non-wild pathname ~A."
                    self pathname))
    (setf (slot-value self 'components)
          (let* ((files (directory pathname))
                 (class (wild-module-component-class self))
                 (options (wild-module-component-options self)))
            (mapcar (lambda (file)
                      (apply #'make-instance class
                             :name (namestring file)
                             :pathname file
                             :parent self
                             options))
                    files)))
    (compute-module-components-by-name self)
    (values)))

(export 'wild-module)
