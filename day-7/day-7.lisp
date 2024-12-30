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
        :collect (mapcar
                  #'parse-integer
                  (remove-if
                   (lambda (elt) (zerop (length elt)))
                   (uiop:split-string line :separator '(#\: #\Space))))))

(defun slice-last-bit (n)
  (let ((sliced-bit (logand n #B1))
        (remaining (ash n -1)))
    (list remaining sliced-bit)))

(defun combination-to-operators (combination bit-length)
  "Convert an operator combination to a list of actual operators.

   Note: There is a special case where combination is all zeroes, when just mul
   op should be used.  Another case is when leading zeroes matter, as they also
   represent mul. Because of this, we can't use just integer-length function."
  (let ((operators ())
        (bin-ops combination))
    (dotimes (counter bit-length)
      (destructuring-bind (remaining-bin-ops op-bit) (slice-last-bit bin-ops)
        (case op-bit
          (0 (push '* operators))
          (1 (push '+ operators)))
        (setf bin-ops remaining-bin-ops)))
    ;; This returns operators from left to right, same order as problem
    ;; evaluation order.
    operators))

(defun evaluate (terms combination)
  "Return value of evaluated terms using operator combination."
  (let ((running-total (first terms))
        (operators (combination-to-operators combination (1- (length terms)))))
    (loop :for term :in (rest terms)
          :for operator :in operators
          :do (setf running-total (funcall operator running-total term)))
    running-total))

(defun print-solution-and-goal (terms combination goal)
  (let* ((running-total (first terms))
         (operators (combination-to-operators combination (1- (length terms))))
         (expression `(,running-total)))
    (loop :for term :in (rest terms)
          :for operator :in operators
          :do (progn
                (push operator expression)
                (push term expression)))
    (format nil "~a = ~{~a~^ ~}"
            goal (reverse (alexandria:flatten expression)))))

(defun equation-solvable-p (goal terms)
  "Evaluate if equation is solvable.

   Combine terms with binary operators 'special-add' and 'special-mul', where
   operators have same precedence. Return t if there is an operator-and-term
   combination with same value as given goal. Else, return nil."

  ;; Do a bit of a "hack" here; let 1 represent add and 0 represent mul.
  ;; Produce all combinations og add and mul and loop over them.
  ;; Exit early if applied operators overshoot goal.
  ;; Return as soon as a valid combination is found.
  (let* ((operator-slots (1- (length terms)))
         (possible-operator-combinations (expt 2 operator-slots)))
    (loop :for combination :from 0 :to possible-operator-combinations
          :when (= (evaluate terms combination) goal)
            :return t)))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (loop :for parts :in input
        :for goal = (first parts)
        :for terms = (rest parts)
        :when (equation-solvable-p goal terms)
          :sum goal))

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