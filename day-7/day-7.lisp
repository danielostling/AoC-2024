;;;; day-7.lisp

(in-package #:day-7)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.

   Input format is, per line
   result: term term term ... term

   `result` and `term` are all integers, colon and space are literals.

   Return a list of lists where first element is result and following elements
   are terms in same order as read from input. All elements are converted to
   integer values."
  (loop :for line :in lines
        :collect (mapcar #'parse-integer
                         (remove-if
                          (lambda (elt) (zerop (length elt)))
                          (uiop:split-string line :separator '(#\: #\Space))))))

(defun equation-solvable-p (goal terms)
  "Evaluate if equation is solvable.

   Combine terms with binary operators 'special-add' and 'special-mul', where
   operators have same precedence. Return t if there is an operator-and-term
   combination with same value as given goal. Else, return nil."

  ;; Do a bit of a "hack" here; let 1 represent add and 0 represent mul.
  ;; Produce all combinations og add and mul and loop over them.
  ;; Exit early if applied operators overshoot goal.
  ;; Return as soon as a valid combination is found.
  (let* ((operator-slots (1- (len terms)))
         

         )
    


    ))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (loop :for parts :in input
        :for goal = (first parts)
        :for terms = (rest parts)
        :count (solve-equation goal terms)))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  )

(defun main (&optional (mode :full))
  "AoC 2024 day 7 solution.
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
