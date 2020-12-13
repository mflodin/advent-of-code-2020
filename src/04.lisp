(ql:quickload "cl-ppcre")
(load "./lib/split-str.lisp")


(defun extract-field (x)
    (let ((pair (split-str x ":")))
        (setf (car pair) (intern (string-upcase (car pair))))
        pair))

;; byr (Birth Year)
;; iyr (Issue Year)
;; eyr (Expiration Year)
;; hgt (Height)
;; hcl (Hair Color)
;; ecl (Eye Color)
;; pid (Passport ID)
;; cid (Country ID)

;; (setq *required-fields* '(byr iyr eyr hgt hcl ecl pid))

(defun has-required-fields (x)
    (and (find 'iyr x) (find 'ecl x) (find 'hgt x) (find 'hcl x) (find 'eyr x) (find 'pid x) (find 'byr x))) 

(defun count-valid (arr)
    (loop :for element :in arr :counting t))  

(defun get-passports (file)
    (let ((in (open file :if-does-not-exist nil))
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
                            (setf passport (append (mapcar #'extract-field (split-str line)) passport))
                            ;; (print passport)
                            ))

            )
            (push passport passports)
            (setf passport nil))
        (close in)
        passports))



(defun get-fields (passports)
    (mapcar #'get-fields-from-each passports))

(defun get-fields-from-each (passport) 
    (mapcar #'car passport))

(defun zip (x y)
    (and x y))

(defun get-passports-with-all-required-fields (passports)
    (remove nil 
        (mapcar #'zip 
            (mapcar #'has-required-fields (get-fields passports))
            passports )
    )
)

(defun 04-1 ()
    (let* ((passports (get-passports "./test-input/04.txt"))
          (valid (get-passports-with-all-required-fields passports)))
    (print valid)
    (print (count-valid valid))))

;; (04-1)


(defun get-valid-passports (passports)
    (remove nil (mapcar #'is-valid-passport passports)))

(defun is-valid-passport (passport)
    ;; (format t "~%byr: ~a, iyr: ~a, eyr: ~a, hgt: ~a, hcl: ~a, ecl: ~a, pid: ~a" 
    ;;            (valid-byr passport) 
    ;;            (valid-iyr passport) 
    ;;            (valid-eyr passport) 
    ;;            (valid-hgt passport) 
    ;;            (valid-hcl passport) 
    ;;            (valid-ecl passport) 
    ;;            (valid-pid passport))

    (when (and (valid-byr passport) 
               (valid-iyr passport) 
               (valid-eyr passport) 
               (valid-hgt passport) 
               (valid-hcl passport) 
               (valid-ecl passport) 
               (valid-pid passport))
        passport)
)



;; byr (Birth Year) - four digits; at least 1920 and at most 2002.
;; byr valid:   2002
;; byr invalid: 2003
(defun valid-byr (passport)
    (let ((byr (parse-integer (cadr (assoc 'byr passport)))))
        (and (>= byr 1920) (<= byr 2002))))

(unless (valid-byr '((byr "2002")))
    (print "byr 2002 should be valid"))
(when (valid-byr '((byr "2003")))
    (print "byr 2003 should be invalid"))

;; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
(defun valid-iyr (passport)
    (let ((iyr (parse-integer (cadr (assoc 'iyr passport)))))
        (and (>= iyr 2010) (<= iyr 2020))))


(when (valid-iyr '((iyr "2002")))
    (print "iyr 2002 should be invalid"))
(unless (valid-iyr '((iyr "2012")))
    (print "iyr 2012 should be valid"))
(when (valid-iyr '((iyr "2023")))
    (print "iyr 2023 should be invalid"))


;; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
(defun valid-eyr (passport)
    (let ((eyr (parse-integer (cadr (assoc 'eyr passport)))))
        (and (>= eyr 2020) (<= eyr 2030))))

(when (valid-eyr '((eyr "2002")))
    (print "eyr 2002 should be invalid"))
(unless (valid-eyr '((eyr "2022")))
    (print "eyr 2022 should be valid"))
(when (valid-eyr '((eyr "2033")))
    (print "eyr 2033 should be invalid"))


;; hgt (Height) - a number followed by either cm or in:
    ;; If cm, the number must be at least 150 and at most 193.
    ;; If in, the number must be at least 59 and at most 76.
;; hgt valid:   60in
;; hgt valid:   190cm
;; hgt invalid: 190in
;; hgt invalid: 190
(defun valid-hgt (passport)
    (setq hgt (cadr (assoc 'hgt passport)))
    (cl-ppcre::register-groups-bind (height unit inches cm) ("(.+)((in)|(cm))" hgt)
        (setf height (parse-integer height))
        (cond (cm (and (>= height 150) (<= height 193)))
              (inches (and (>= height 59) (<= height 76)))
              (t nil))))

(unless (valid-hgt '((hgt "60in")))
    (print "(hgt 60in) should be valid"))
(unless (valid-hgt '((hgt "190cm")))
    (print "(hgt 190cm) should be valid"))
(when (valid-hgt '((hgt "190in")))
    (print "(hgt 190in) should be invalid"))
(when (valid-hgt '((hgt "190")))
    (print "(hgt 190) should be invalid"))

;; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;; hcl valid:   #123abc
;; hcl invalid: #123abz
;; hcl invalid: 123abc
(defun valid-hcl (passport)
    (let ((hcl (cadr (assoc 'hcl passport))))
        (cl-ppcre::scan "#[0-9a-f]{6}" hcl)))

(unless (valid-hcl '((hcl "#123abc")))
    (print "hcl #123abc should be valid"))
(unless (valid-hcl '((hcl "#a0d5f9")))
    (print "hcl #a0d5f9 should be valid"))
(when (valid-hcl '((hcl "#123abz")))
    (print "hcl #123abc should be invalid"))
(when (valid-hcl '((hcl "123abc")))
    (print "hcl #123abc should be invalid"))

;; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;; ecl valid:   brn
;; ecl invalid: wat
(defun valid-ecl (passport)
    (let ((ecl (string-downcase (string (cadr (assoc 'ecl passport))))))
        (cl-ppcre::scan "amb|blu|brn|gry|grn|hzl|oth" ecl)))

(unless (valid-ecl '((ecl brn)))
    (print "ecl brn should be valid"))
(when (valid-ecl '((ecl wat)))
    (print "ecl brn should be invalid"))


;; pid (Passport ID) - a nine-digit number, including leading zeroes.
;; pid valid:   000000001
;; pid invalid: 0123456789
(defun valid-pid (passport)
    (let ((pid (cadr (assoc 'pid passport))))
        (cl-ppcre::scan "^[0-9]{9}$" pid)))

(unless (valid-pid '((pid "000000001")))
    (print "pid 000000001 should be valid"))
(when (valid-pid '((pid "0123456789")))
    (print "pid 0123456789 should be invalid"))

;; cid (Country ID) - ignored, missing or not.

(defun 04-2 (file)
    (setq passports (get-passports file))
    ;; (print passports)
    (setq passports-to-check (get-passports-with-all-required-fields passports))

    (setq valid (get-valid-passports passports-to-check))
    (print valid)
    (print (count-valid valid)))

(04-2 "./input/04.txt")



;; (print (mapcar #'zip '(nil x nil x) '(1 2 3 4) ))

;; (mapcar #'print (cl-ppcre::scan-to-strings "(.+)(in|cm)" "60in"))