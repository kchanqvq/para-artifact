(in-package #:prf)
(defclass bfs (o:problem)
  ((visited) (queue)))
(o:defmethod-1 shared-initialize (:after) (bfs slot-names &key &allow-other-keys)
  (psetf visited (make-hash-table :test 'equalp) queue (min-max-heap:make-heap)))
(o:defmethod-1 o:run () (bfs)
  (bind ((par (min-max-heap:pop-min queue))
         (par (if (eql par 0) o:parameter par))
         (cost (o:cost o::self par)))
    (klet ((cont (par)
             (unless (gethash par visited)
               (setf (gethash par visited) t)
               (min-max-heap:insert queue cost par))))
      (o:mutate o::self par #'cont))))
