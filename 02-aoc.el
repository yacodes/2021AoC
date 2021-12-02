;;; package --- Summary
;;; Commentary:
;;; Code:

;; Example of the list of submarine commands
;; (defconst submarine-commands '(("forward" 5)
;;                                ("down" 5)
;;                                ("forward" 8)
;;                                ("up" 3)
;;                                ("down" 8)
;;                                ("forward" 2)))

(defun read-lines (file-path)
  "Return a list of lines of a file at FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defconst submarine-commands
  (mapcar
   (lambda (string) (let ((command (split-string string "\s" t)))
                      (list
                       (car command)
                       (string-to-number (nth 1 command)))))
   (read-lines "./02-aoc.txt")))

(defun seq* (sequence)
  "Multiply all elements in a SEQUENCE."
  (seq-reduce #'* sequence 1))

(defun part-1-main ()
  "Solution of the advent of code puzzle for day 2."
  (seq* (seq-reduce
         (lambda (submarine-position command)
           (let ((command-name (car command))
                 (command-value (nth 1 command))
                 (x (car submarine-position))
                 (y (nth 1 submarine-position)))
             (cond ((equal command-name "forward") (list (+ x command-value) y))
                   ((equal command-name "down") (list x (+ y command-value)))
                   ((equal command-name "up") (list x (- y command-value))))))
         submarine-commands '(0 0))))

(message "Part 1 answer is %d" (part-1-main))

(defun part-2-main ()
  "Solution of the advent of code puzzle for day 2."
  (seq* (butlast (seq-reduce
         (lambda (submarine-position command)
           (let ((command-name (car command))
                 (command-value (nth 1 command))
                 (x (car submarine-position))
                 (y (nth 1 submarine-position))
                 (aim (nth 2 submarine-position)))
             (cond ((equal command-name "forward") (list (+ x command-value) (+ y (* aim command-value)) aim))
                   ((equal command-name "down") (list x y (+ aim command-value)))
                   ((equal command-name "up") (list x y (- aim command-value))))))
         submarine-commands '(0 0 0)))))

(message "Part 2 answer is %d" (part-2-main))

;;; 02-aoc.el ends here
