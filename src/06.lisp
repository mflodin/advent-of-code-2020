(ql:quickload :uiop)
;; (ql:quickload :iterate)
;; (use-package :iterate)

(defparameter *input* (uiop:read-file-lines "./input/06.txt"))
(defparameter *test-input* (uiop:read-file-lines "./test-input/06.txt"))

;; (print *test-input*)

(defun 06-1 (input)
    (let ((group-answers nil) (current ""))
        (loop for line in input
            if (equal line "")
                do (push current group-answers)
                and do (setf current "")
            else
                do (setf current (concatenate 'string line current))
            finally (push current group-answers)
        )
        (apply #'+ (mapcar (lambda (x) (length (remove-duplicates x))) group-answers))
    ))


    ;;  (loop repeat 10
    ;;    for x = (random 100)
    ;;    if (evenp x)
    ;;       collect x into evens
    ;;       and do (format t "~a is even!~%" x)
    ;;    else
    ;;       collect x into odds
    ;;       and count t into n-odds
    ;;    finally (return (values evens odds n-odds)))

(print (06-1 *input*))