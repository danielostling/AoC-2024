;;;; day-6.lisp

(in-package #:day-6)


(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.

   Input is a 2D map of characters . and #, and a single guard position marked
   by a single ^ character.

   Make a 2D array, and replace . by 0, # by -1. Return 2D array and (row col)
   of guard position."
  (let* ((cols (length (first lines)))
         (rows (length lines))
         (room (make-array `(,rows ,cols)))
         (guard-pos nil))
    (loop :for line :in lines
          :for row = 0 :then (1+ row)
          :do (loop :for char :in (coerce line 'list)
                    :for col = 0 then (1+ col)
                    :do (cond ((char= char #\.) (setf (aref room row col) 0))
                              ((char= char #\#) (setf (aref room row col) -1))
                              (t (setf guard-pos `(,row ,col))))))
    `(,room ,guard-pos)))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  )

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  )

(defun main (&optional (mode :full))
  "AoC 2024 day 6 solution.
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
