(ql:quickload :uiop)
;; (ql:quickload :iterate)
;; (use-package :iterate)

(defparameter *input* (uiop:read-file-lines "./input/06.txt"))
(defparameter *test-input* (uiop:read-file-lines "./test-input/06.txt"))

;; (print *test-input*)

(defun get-group-answers (input)
    (let ((group-answers nil) (current nil))
        (loop for line in input
            if (equal line "")
                do (push current group-answers)
                and do (setf current nil)
            else
                do (push line current)
            finally (push current group-answers)
        )
    group-answers))

(defun join-answers (answers)
    (remove-duplicates (apply #'concatenate 'string answers)))

(defun 06-1 (input)
    (apply #'+ (mapcar (lambda (x) 
                        (length (join-answers x))) 
                    (get-group-answers input))))

;; (print (06-1 *input*))


(defun 06-2 (input) 
    (let* ((group-answers (get-group-answers input))
          (group-combined (mapcar #'join-answers group-answers)))
         (apply #'+  (loop for group in group-answers
              for combined in group-combined

              collect (count-if-not #'null (loop for chr across combined
                    collect (every (lambda (x) (search (string chr) x)) group)
              )  )  
        ))
    )
)

(print (06-2 *input*))
