(load "./lib/split-str.lisp")

(defun 03-1 ()
    (let ((in (open "./input/03.txt" :if-does-not-exist nil))
        ;;   (slope ())
          (width 0)
          (chr nil)
          (x 0)
          (y 0)
          (n 0))
        (when in
            (loop for line = (read-line in nil)
                while line do
                    (when (> y 0)
                        (setf width (length line))
                        (setf x (mod (+ x 3) width))
                        (setf chr (char line x))
                        (when (equal chr #\#)
                            (setf n (1+ n)))
                        ;; (push (split-str line) slope)
                        (format t "~a, ~a, ~a, ~a~%" line chr x n)
                    )
                    (setf y (+ y 1))
            )

        (close in))
        (princ n)
        ))


(03-1)