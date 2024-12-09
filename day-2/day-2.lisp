;;;; day-2.lisp

(in-package #:day-2)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.

   Turn input of the form
   '7 6 4 2 1'
   '1 2 7 8 9'
   '9 7 6 2 1'
   '1 3 2 4 5'
   '8 6 4 4 1'
   '1 3 6 7 9'

   into a list of lists of integers like this
   ((7 6 4 2 1)
    (1 2 7 8 9)
    (9 7 6 2 1)
    (1 3 2 4 5)
    (8 6 4 4 1)
    (1 3 6 7 9))"
  
  (loop :for line :in lines
        :collect (loop :for elt :in (uiop:split-string line)
                       :collect (parse-integer elt))))


(defun ok-delta-p (a b)
  "Return T if integers a and b differ by 1, 2 or 3 and NIL otherwise."
  (let ((delta (abs (- a b))))
    (and (>= delta 1)
         (<= delta 3))))

(defun safe-p (readings)
  "Return T if consecutive values in readings are all 1-3 in distance, else NIL."
  (let* ((shifted-readings (rest readings))
         (slopes (loop named safe
                       :for first-reading :in readings
                       :for second-reading :in shifted-readings
                       :for slope = (- second-reading first-reading)
                       :if (ok-delta-p first-reading second-reading)
                         :collect (plusp slope)
                       :else
                         :do (return-from safe nil))))
    (if (null slopes)
        nil
        (every #'eql slopes (rest slopes)))))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (loop :for readings :in input
        :counting (safe-p readings)))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  )

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
