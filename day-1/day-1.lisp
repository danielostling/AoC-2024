;;;; day-5.lisp

(in-package #:day-1)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.

   Input is a list of rows of two integers, represented as text.
   Example:
   3   4
   4   3
   2   5
   1   3
   3   9
   3   3

   The first column is one list, and the second column is another list.  Return
   a list of these two lists, where the first list is the first column, and the
   second list is the second column.  All sublist elements should be converted
   from text to integers."

  (let ((lst1 nil)
        (lst2 nil))
    (dolist (line lines)
      (let* ((parts (uiop:split-string line))
             (val1 (parse-integer (first parts)))
             (val2 (parse-integer (first (reverse parts)))))
        (push val1 lst1)
        (push val2 lst2)))
    (list lst1 lst2)))

(defun freq (lst)
  "Given a list of integers, return a number frequency hash where hash key is the
   integer value, and hash value is the number of times that value ocurred in the
   list."
  (let ((result (make-hash-table)))
    (loop :for elt :in lst
          :do (incf (gethash elt result 0)))
    result))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (loop :for a :in (sort (copy-list (first input)) #'<)
        :for b :in (sort (copy-list (second input)) #'<)
        :for distance = (abs (- (abs a) (abs b)))
        :summing distance))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (let* ((lst1 (first input))
         (lst2 (second input))
         (frequency (freq lst2)))
    (loop :for elt :in lst1
          :for count = (gethash elt frequency 0)
          :summing (* elt count))))

(defun main (&optional (mode :full))
  "AoC 2024 day 1 solution.
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
