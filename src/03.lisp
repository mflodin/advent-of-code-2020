(load "./lib/split-str.lisp")

(defun 03-1 (dx dy)
    (princ #\Linefeed)
    (let ((in (open "./input/03.txt" :if-does-not-exist nil))
          (width 0)
          (chr #\-)
          (x 0)
          (y 0)
          (l 0)
          (n 0))
        (when in
            (loop for line = (read-line in nil)
                while line do
                    (when (= l y)
                        (when (> y 0)
                            (setf width (length line))
                            (setf x (mod (+ x dx) width))
                            (setf chr (char line x))
                            (when (equal chr #\#)
                                (setf n (1+ n)))
                        )
                        (setf y (+ y dy))
                    )
                    (format t "~a, ~a, ~a, ~a~%" line chr x n)
                    (setf l (1+ l))
                    (setf chr #\-)

            )

        (close in))
        (print n)
        ))


(print (* (03-1 1 1) (03-1 3 1) (03-1 5 1) (03-1 7 1) (03-1 1 2)))