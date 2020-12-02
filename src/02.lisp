; https://gist.github.com/siguremon/1174988/babcbdcbbfcb9f42df34f000f9326a26caa64be4
(defun split-str (string &optional (separator " "))
  (split-1 string separator))

(defun split-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))

(defun count-occurrances (str chr)
  (let ((occurrances 0))
    (loop for c across str do 
      (when (string= c chr)
        (setq occurrances (1+ occurrances))))
    
    
    occurrances))
    ;; (format t "~a, ~a~%" str x)))


(defun part-1 ()
  (let ((in (open "./input/02.txt" :if-does-not-exist nil))
        (correct 0))
    (when in
      (loop for line = (read-line in nil)
          while line do 
              (let ((parts (split-str line))
                    (occurrances 0))
                  (let ((limits (split-str (car parts) "-"))
                        (chr (subseq (cadr parts) 0 1))
                        (password (caddr parts)))
                      (let ((lower (car limits))
                            (upper (cadr limits)))
                          (setq occurrances (count-occurrances password chr))
                          ;; (format t "~a, ~a, ~a, ~a, ~a~%" lower upper chr password occurrances)
                          (when (and (>= occurrances (parse-integer lower)) (<= occurrances (parse-integer upper)))
                            (setf correct (1+ correct))
                          )
                          ))))
      (close in))
    (print correct)))

;; (defun xor (a b)
;;   (and 
;;    (not (and a b))
;;    (or a b)))

(defun part-2 ()
  (let ((in (open "./input/02.txt" :if-does-not-exist nil))
        (correct 0))
    (when in
      (loop for line = (read-line in nil)
          while line do 
              (let ((parts (split-str line))
                    (occurrances 0))
                  (let ((limits (split-str (car parts) "-"))
                        (chr (char (cadr parts) 0))
                        (password (caddr parts)))
                      (let ((lower (parse-integer (car limits)))
                            (upper (parse-integer (cadr limits))))
                          (let ((1st (char password (1- lower)))
                                (2nd (char password (1- upper))))
                                ;; (format t "~a, ~a, ~a, ~a, ~a, ~a~%" lower upper chr password 1st 2nd)
                                (when (xor (char= chr 1st) (char= chr 2nd))
                                  (setf correct (1+ correct))
                                )
                          )
                      )
                  )
              )
      )

                          
                          


      (close in))
    (print correct)))

(part-2)