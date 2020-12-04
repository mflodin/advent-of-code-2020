;; byr (Birth Year)
;; iyr (Issue Year)
;; eyr (Expiration Year)
;; hgt (Height)
;; hcl (Hair Color)
;; ecl (Eye Color)
;; pid (Passport ID)
;; cid (Country ID)

(load "./lib/split-str.lisp")

(setq *required-fields* '(byr iyr eyr hgt hcl ecl pid))

(defun extract-field (x)
    (mapcar (lambda (y) (intern (string-upcase y))) (split-str x ":")))

(defun is-valid (x)
    (and (find 'iyr x) (find 'ecl x) (find 'hgt x) (find 'hcl x) (find 'eyr x) (find 'pid x) (find 'byr x)))    

(defun 04-1 ()
    ;; (princ #\Linefeed)
    (let ((in (open "./input/04.txt" :if-does-not-exist nil))
          (passport nil)
          (passports nil)
          (fields nil)
          (valid 0))
        (when in
            (loop for line = (read-line in nil)
                while line do
                    (cond ((equal "" line)
                            (push passport passports)
                            (setf passport nil))
                        (T 
                            (setf passport (append (mapcar (lambda (x) (extract-field x)) (split-str line)) passport))
                            ;; (print passport)
                            ))

            )
            (push passport passports)
            (setf passport nil)
        )
    (setf fields (mapcar (lambda (x) (mapcar #'car x)) passports))
    (setf valid (remove nil (mapcar (lambda (x) (is-valid x)) fields)))
    (print (loop :for element :in  valid
      :counting t
      
      ))
    ))

(04-1)


;; (setq *x* (mapcar (lambda (x) (intern (string-upcase x))) '("iyr" "ecl" "hgt" "hcl" "eyr" "pid") ))
;; (setq *y* '(iyr ecl hgt hcl eyr pid))

;; (print (find 'iyr *x*))
;; (print (find 'iyr *y*))