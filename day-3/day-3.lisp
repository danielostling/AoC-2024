;;;; day-3.lisp

(in-package #:day-3)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format."
  ;; Do nothing for this puzzle.
  lines)

(defun do-mul (line)
  "Search given line for substring `mul(x,y)` where x and y are integers.
   Return a sum of all the x * y found, or nil if nothing is found."
  (let* ((parse-regex "mul\\(\\d+,\\d+\\)")
         (args-regex "\\d+")
         (raw-operations (cl-ppcre:all-matches-as-strings parse-regex line)))
    (loop :for op :in raw-operations
          :for vals = (mapcar
                       #'parse-integer
                       (cl-ppcre:all-matches-as-strings args-regex op))
          :summing (* (first vals) (second vals)))))


(defun do-mul-with-toggle (line)
  "Search given line for substring `mul(x,y)` where x and y are integers.
   Return a sum of all the x * y found, or nil if nothing is found.

   Additionally, operations are enabled or disabled, starting enabled.
   The do() instruction enables future mul instructions.
   The don't() instruction disables future mul instructions."
  (flet ((calculate (op)
           "Calculate mul(x,y) and return as integer."
           (let ((args-regex "\\d+"))
             (apply #'*
              (mapcar #'parse-integer
               (cl-ppcre:all-matches-as-strings args-regex op))))))
    (let* ((parse-regex "do\\(\\)|don\\'t\\(\\)|mul\\(\\d+,\\d+\\)")
           (raw-operations (cl-ppcre:all-matches-as-strings parse-regex line))
           (mul-enabled t))
      (format t "~a~%" raw-operations)
      (loop :for op :in raw-operations
            ;; Test for "do()"
            :when (string= (subseq op 0 3) "do(")
              :do (setf mul-enabled t)
            ;; Test for "don't()"
            :when (and (>= (length op) 6)
                       (string= (subseq op 0 6) "don't("))
              :do (setf mul-enabled nil)
            ;; Test for "mul(x,y)"
            :when (and (string= (subseq op 0 4) "mul(")
                       mul-enabled)
              :sum (calculate op)
            :when (and (string= (subseq op 0 4) "mul(")
                       (not mul-enabled))
              :do (format t "Skipping ~a~%" op)
            ))))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (loop :for line :in input
        :summing (do-mul line)))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
    (loop :for line :in input
          :summing (do-mul-with-toggle line)))

(defun main (&optional (mode :full))
  "AoC 2024 day 2 solution.
   Mode is one of
   :full - use full puzzle input
   :test - use test puzzle input"
  (let* ((path-input-full #P"./input-full")
         (path-input-test #P"./input-test")
         (path-input (if (equal mode :full)
                         path-input-full
                         path-input-test))
         (input (parse-input (read-input path-input))))
    (format t "Part 1: ~a~%" (solve-part-1 input))
    (format t "Part 2: ~a~%" (solve-part-2 input))))
