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

(defun safe-p(levels)
  "Return T if consecutive values in levels are all ok-delta-p (1-3) in
   distance, else NIL."
  (let* ((shifted-levels (rest levels))
         (slopes (loop
                   :for this-level :in levels
                   :for next-level :in shifted-levels
                   :for slope = (- next-level this-level)
                   :if (ok-delta-p this-level next-level)
                     :collect (plusp slope)
                   :else
                     :return nil)))
    (if (null slopes)
        nil
        (every #'eql slopes (rest slopes)))))

(defun remove-elt (idx lst)
  "Remove one element at index idx from list lst. Return a newly allocated list.
   No bounds checking is done."
  (concatenate 'list (subseq lst 0 idx) (copy-list (nthcdr (1+ idx) lst))))

(defun safe-ish-p (levels)
  "Brute force solution by removing one measurement at a time.
   Return t if a safe set is found, else nil."
  (if (safe-p levels)
      t
      (loop :for idx :from 0 :below (length levels)
            :when (safe-p (remove-elt idx levels))
              :return t)))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (loop :for levels :in input
        :count (safe-p levels)))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  ;; This is a straight brute-force of all combinations.
  ;; Not elegant but it gets the solution done.
  (loop :for levels :in input
        :count (safe-ish-p levels)))

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
