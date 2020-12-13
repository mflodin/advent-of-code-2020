(ql:quickload :uiop)
(ql:quickload :iterate)
(use-package :iterate)

(defparameter *input* (uiop:read-file-lines "./input/05.txt"))

(defun get-seat (boarding-pass)
    (let ((rows (subseq boarding-pass 0 7))
          (columns (subseq boarding-pass 7 10))
          (rowstep 127) 
          (columnstep 7)
          (row 0)
          (column 0))
        ;; (format t "~%~a ~a" rows columns)
        
        (iter (for letter in-string rows)
            (setf rowstep (ceiling rowstep 2))
            (cond 
                ((equal letter #\B) (setf row (+ row rowstep)))
                ;; ((equal letter #\F) (setf rowmax (- rowmax rowstep)))

            ))
        (iter (for letter in-string columns)
            (setf columnstep (ceiling columnstep 2))
            (cond 
                ((equal letter #\R) (setf column (+ column columnstep)))
                ;; ((equal letter #\L) (setf column (- column columnstep)))
            ))
        
        (values (+ (* row 8) column) row column)))

(defun get-seats (input)
    (mapcar #'get-seat input))

(defun 05-1 (input)
    (print (apply #'max (get-seats input))))
    


(05-1 *input*)



;; FBFBBFFRLR: row 44, column 5, seat ID 357.
;; BFFFBBFRRR: row 70, column 7, seat ID 567.
;; FFFBBBFRRR: row 14, column 7, seat ID 119.
;; BBFFBBFRLL: row 102, column 4, seat ID 820.