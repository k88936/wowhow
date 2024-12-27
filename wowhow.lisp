(defmacro dovector ((index vector)  &body body) 
  `(do ((,index  0 (1+ ,index))) 
       ((>= ,index  (length ,vector)))
     ,@body )) 
(defun s-rightarrow(left right)
    (lambda (context) 
      (vector-push-extend (funcall left -1 ) context )
      (vector-push-extend (funcall right 1 ) context )))
(defmacro with-delay ( &key at-left at-left1 at-right at-right1)
  `(lambda (lr) 
     (lambda (context contexts) 
        (if  (= lr -1) (progn ,at-left1 ,at-left) 
                        (progn ,at-right1 ,at-right) ))))
(defparameter *var-index* 0)
(defmacro  declvar (name) 
  (incf *var-index*)
  `(defvar ,name (with-delay :at-left  ,(* -1 *var-index*) :at-right ,(* 1 *var-index*))) )
(defmacro push-to-context ( v1 lr1 &optional v2 lr2 ) 
  (if (eq lr2 nil)
      `(progn (vector-push-extend (funcall  ,v1 ,lr1)  context ) 0 )
      `(progn (vector-push-extend (funcall  ,v1 ,lr1)  context )  (vector-push-extend (funcall  ,v2 ,lr2)  context ) 0)))
(defmacro push-to-branch ( v1 lr1 &optional  v2 lr2 ) 
   `(progn (let* ((len (length context))(branch (make-array len  :fill-pointer len  :adjustable t)))
                (dotimes (i len)
                  (setf (elt branch i) (elt context i)))
                  ,@(if (eq lr2 nil)
                    `((vector-push-extend (funcall  ,v1 ,lr1)  branch) )
                    `((vector-push-extend (funcall  ,v1 ,lr1)   branch) (vector-push-extend (funcall  ,v2 ,lr2)   branch)))
                (vector-push-extend branch contexts ))
           0))
(defun  neg (left)
  (with-delay 
   :at-left (push-to-context left 1 ):at-right (push-to-context left -1)))
(defun lor (left right)
  (with-delay
    :at-left (push-to-context left -1):at-left1 (push-to-branch right -1):at-right (push-to-context left 1 right 1)))
(defun land (left right) 
  (with-delay 
    :at-left (push-to-context left -1 right -1):at-right (push-to-context left 1):at-right1 (push-to-branch right 1)))
(defun rightarrow (left right)
  (with-delay
    :at-left (push-to-context right -1):at-left1 (push-to-branch left 1):at-right (push-to-context left -1 right 1)))
(defun leftrightarrow (left right )
  (with-delay :at-left (push-to-context left -1 right -1):at-right (push-to-branch left 1 right 1)))
(defun check (exp) 
    (let ((contexts (make-array 0  :fill-pointer 0 :adjustable t )))
      (vector-push-extend (make-array 0  :fill-pointer 0 :adjustable t ) contexts )
      (funcall (eval exp ) (elt contexts 0))
      (dovector (i contexts) 
        (let (ret(context (elt contexts i))(flag (make-array (1+ *var-index*) :fill-pointer 0))) 
          (dotimes (j (1+ *var-index*)) (vector-push  (vector 0 0) flag))
          (dovector  (j context)
            (let ((func (elt context j)))
            (setf ret (if (typep func 'function ) 
                        (progn 
                          (setf  (elt context j) 0)
                          (setf (elt context j)   (funcall func  context contexts)))
                        func )))
            (incf (elt(elt flag (abs ret)) (if (> ret 0) 0 1))))
          (if (= 1 (count-if (lambda (x) (= 1 (min (elt x 0) (elt x 1))))   flag :start 1)) 
            (format t "passed: ~S ~&" context)
            (progn (format t "failed: ~S ~&" context) (return-from check ))))))
  (format t "all right~&"))
