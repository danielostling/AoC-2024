;;;; day-12.lisp

(in-package #:day-12)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse INPUT, a list of strings, into solution-friendly format.

   Input represents a 2D grid of plant plot indicators. Return an array with two
   dimensions, where (row col) position (0 0) is 'top-left' in the input, and
   each value in the array is the corresponding character from the input."
  (let ((rows (length lines))
        (cols (length (first lines))))
    (make-array `(,rows ,cols)
     :element-type 'character :adjustable nil  :fill-pointer nil :displaced-to nil
     :initial-contents (mapcar (lambda (line) (coerce line 'list)) lines))))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."

  
  )

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  )

(defun main (&optional (mode :full))
  "AoC 2024 day 12 solution.
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
