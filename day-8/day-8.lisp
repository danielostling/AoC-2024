;;;; day-8.lisp

(in-package #:day-8)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format."
  )

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  )

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  )

(defun main (&optional (mode :full))
  "AoC 2024 day 8 solution.
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
