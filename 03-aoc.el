;;; package --- Summary
;;; Commentary:
;;; Code:
(defun read-lines (file-path)
  "Return a list of lines of a file at FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun split-bytes (str)
  "Splits STR into list of numbers."
  (mapcar 'string-to-number (split-string str "" t)))

(defconst diagnostic-report
  (let ((bytes (mapcar 'split-bytes (read-lines "./03-aoc.txt"))))
    (mapcar
     (lambda (i) (mapcar (lambda (l) (nth i l)) bytes))
     (number-sequence 0 (1- (length (car bytes)))))))

(defun count-bytes (bytes)
  "Count list of BYTES and return the most common byte."
  (if (> 0
         (seq-reduce
          (lambda (acc b) (if (= b 0) (1- acc) (1+ acc)))
          bytes 0))
      0 1))

(defun invert-bytes (bytes)
  "Invert list of BYTES."
  (mapcar (lambda (bit) (logxor bit 1)) bytes))

(defun numlist-to-string (numlist)
  "Convert NUMLIST to string."
  (mapconcat 'number-to-string numlist ""))

(defun p1 (report)
  "Calculates power of the submarine, based on the REPORT."
  (let* ((gamma (mapcar 'count-bytes report))
         (epsilon (invert-bytes gamma)))
    (format "Power consumption of the submarine is %s"
             (*
              (string-to-number (numlist-to-string gamma) 2)
              (string-to-number (numlist-to-string epsilon) 2)))))

(message (p1 diagnostic-report))

;; Part 2 has too many rules, boring

;;; 03-aoc.el ends here
