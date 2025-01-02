;;;; day-7.lisp

(in-package #:day-7)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input strings into solution-friendly format.

   Input format is, per line
   result: term term term ... term

   `result` and `term` are all string represented integer base 10 numbers, colon
   and space are literals.

   Return a list of lists where first element is result and following elements
   are terms in same order as read from input. All elements are converted from
   string representation to integer values."
  (loop :for line :in lines
        :collect (mapcar
                  #'parse-integer
                  (remove-if
                   (lambda (elt) (zerop (length elt)))
                   (uiop:split-string line :separator '(#\: #\Space))))))

(defun || (a b)
  "Concatenate base 10 integer numbers a and b into a new base 10 integer number ab."
  (parse-integer (format nil "~a~a" a b)))

(defun evaluate (terms operators)
  "Return integer value of evaluated terms using operator combination.

   terms is a list of integers to be used for the left-to-right evaluation and
   operators is the list of binary operators to apply to the terms."
  (let ((running-total (first terms)))
    (loop :for term :in (rest terms)
          :for operator :in operators
          :do (setf running-total (funcall operator running-total term)))
    running-total))

(defun print-solution-and-goal (goal terms operators)
  (let* ((running-total (first terms))
         (expression `(,running-total)))
    (loop :for term :in (rest terms)
          :for operator :in operators
          :do (progn
                (push operator expression)
                (push term expression)))
    (format nil "Goal: ~a, ~{~s~^ ~} = "
            goal (reverse (alexandria:flatten expression)))))

(defun get-operator-combinations (terms operators operator-combinations-hash)
  "Return the list of operator combinations given terms list.

   If the requested operator combination is missing from operator-combinations,
   the operator-combinations hash will be *DESTRUCTIVLY* updated with the
   missing combination.
 
   terms is a list of non-negative base 10 integers. operators is a list of
   binary operator references. operator-combinations is a hash with operator
   slot count as key, and as values, a list of lists of all possible operator
   combinations in relation to term count and operator reference list.

   Operator slots is a place where a binary operator can sit between terms in
   the terms list. For example, if terms is (3 4 2), a binary operator can sit
   between 3 and 4, and between 4 and 2. Formula is (1- (length terms)).

   Return requested operator-combination."
  (flet ((cartesian-product (values length)
           "Generate the Cartesian product of VALUES repeated LENGTH times."
           (let ((result (list nil))) ; Start with a single empty combination
             (dotimes (_ length result)
               (setf result
                     (mapcan (lambda (current)
                               (mapcar (lambda (value)
                                         (cons value current))
                                       values))
                             result))))))
    (let ((operator-slots (1- (length terms))))
      (if (member operator-slots (alexandria:hash-table-keys operator-combinations-hash) :test '=)
          (gethash operator-slots operator-combinations-hash)
          (let ((new-combination (cartesian-product operators operator-slots)))
            (setf (gethash operator-slots operator-combinations-hash) new-combination)
            new-combination)))))

(defun equation-solvable-p (goal terms operator-combinations)
  "Evaluate if equation is solvable.

   Combine terms with binary operators 'special-add', 'special-mul' and
   'special-concat', where operators have same precedence. Return t if there is
   an operator-and-term combination with same value as given goal. Else, return
   nil. Straight brute force, no attempt at run time optimization."
  (loop :for combination :in operator-combinations
        :for evaluated = (evaluate terms combination)
        :when (= evaluated goal)
          :return t))

(defun solve (input base)
  (let ((operator-combinations-hash (make-hash-table))
        (operators (subseq (list '* '+ '||) 0 base)))
    (loop :for parts :in input
          :for goal = (first parts)
          :for terms = (rest parts)
          :for operator-slots = (1- (length terms))
          :for operator-combinations = (get-operator-combinations
                                        terms
                                        operators
                                        operator-combinations-hash)
          :for solvable = (equation-solvable-p goal terms operator-combinations)
          :when solvable
            :sum goal)))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (solve input 2))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (solve input 3))

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
