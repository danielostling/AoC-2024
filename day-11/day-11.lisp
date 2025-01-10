;;;; day-11.lisp

(in-package #:day-11)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.
   Input is a single string of integer numbers separated by a single whitespace. Return a list of
   these numbers in the same order they are found in the input."
  (mapcar #'parse-integer (uiop:split-string (first lines))))

(defun transform (stones)
  "Transform list of stones according to puzzle rules and return new list of stones.

  Rules
  -----
  1. If the stone is engraved with the number 0, it is replaced by a stone
     engraved with the number 1.

  2. If the stone is engraved with a number that has an even number of digits,
     it is replaced by two stones. The left half of the digits are engraved on
     the new left stone, and the right half of the digits are engraved on the
     new right stone. (The new numbers don't keep extra leading zeroes: 1000
     would become stones 10 and 0.)

  3. If none of the other rules apply, the stone is replaced by a new stone; the
     old stone's number multiplied by 2024 is engraved on the new stone."
  (let ((new-stones nil))
    (loop :for stone :in stones
          :for str-digits = (format nil "~a" stone)
          :do (cond ((zerop stone) (push 1 new-stones))
                    ((evenp (length str-digits))
                     (progn
                       (push (parse-integer
                              (subseq str-digits 0 (/ (length str-digits) 2)))
                             new-stones)
                       (push (parse-integer
                              (subseq str-digits (/ (length str-digits) 2)))
                             new-stones)))
                    (t (push (* stone 2024) new-stones))))
    (reverse new-stones)))

(defun blink (times input)
  (loop :for blinks :from 0 :to times
        :for stones = input :then (transform stones)
        :when t
          :do (format t "Blinked ~a times (~a stones)~%" blinks (length stones))
        :finally (return (length stones))))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (blink 25 input))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (blink 36 input)
  )

(defun main (&optional (mode :full))
  "AoC 2024 day 11 solution.
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
