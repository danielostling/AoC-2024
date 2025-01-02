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

(defun slice-last-number (n base)
  "Get least significant radix digit for given base `base` number n.

   Return a list (remaining lsn) where remaining is the remaining operators (as
   a base `base` number) and lsn is least significant radix number."
  (let* ((sliced-number (mod n base))
         (remaining (/ (- n sliced-number) base)))
    (list remaining sliced-number)))

;; (defun concat-numbers (a b))
(defun || (a b)
  "Concatenate base 10 integer numbers a and b into a new number ab.

   If a = 10 and b = 5, then return 105.
   Found the algebra at https://math.stackexchange.com/a/579531

   Could have saved myself a lot of thought and trouble by just doing the
   strings-concat-and-convert route, but meh. This variant is pretty quick, at least."

  ;; c(a,b) = b + a * 10^((floor(log10(b+1)) + abs(b+1/2) - (b-1/2)))
  ;;
  ;; Final truncate is to convert float back into integer.
  (truncate (+ b
               (* a
                  (expt 10
                        (+
                         (ffloor (log (+ b 1) 10))
                         (abs (+ b 1/2))
                         (- (- b 1/2))))))))

(defun base10-to-base3 (base10-number)
  "Convert a base 10 number to base 3, return as an integer.
   It's up to the user to keep track of the base.

   Also, 'cheat' by using strings. Sorry."
  (parse-integer (format nil "~3R" base10-number)))

(defun combination-to-operators (combination combination-length &optional (base 2))
  "Convert a base `base` numerical operator combination to a list of actual
   operators."
  (let ((actual-operators ())
        (binary-operators combination))
    (dotimes (counter combination-length)
      (destructuring-bind
          (remaining-binary-operators op-bit)
          (slice-last-number binary-operators base)
        (case op-bit
          (0 (push '* actual-operators))
          (1 (push '+ actual-operators))
          (2 (push '|| actual-operators)))
        (setf binary-operators remaining-binary-operators)))
    ;; Return operators left to right, same order as problem evaluation order.
    actual-operators))

(defun evaluate (terms combination &optional (base 2))
  "Return value of evaluated terms using operator combination."
  (let ((running-total (first terms))
        (operators (combination-to-operators combination (1- (length terms)) base)))
    (loop :for term :in (rest terms)
          :for operator :in operators
          :do (progn
;;                (format t "~a ~a ~a => ~a~%" running-total operator term (funcall operator running-total term))
                (setf running-total (funcall operator running-total term))))
    running-total))

(defun print-solution-and-goal (terms combination goal &optional (base 2))
  (let* ((running-total (first terms))
         (operators (combination-to-operators combination (1- (length terms)) base))
         (expression `(,running-total)))
    (loop :for term :in (rest terms)
          :for operator :in operators
          :do (progn
                (push operator expression)
                (push term expression)))
    (format nil "~a = ~{~s~^ ~}"
            goal (reverse (alexandria:flatten expression)))))

(defun equation-solvable-p (goal terms &optional (base 2))
  "Evaluate if equation is solvable.

   Combine terms with binary operators 'special-add', 'special-mul' and
   'special-concat', where operators have same precedence. Return t if there is
   an operator-and-term combination with same value as given goal. Else, return
   nil. Straight brute force, no attempt at run time optimization."

  ;; Do a bit of a "hack" here; let 0 represent mul, 1 represent add, and 2
  ;; represent concat-numbers.  This is just for funsies, not really a good
  ;; solution. While compact, it's not easy to extend.  Produce all combinations
  ;; of add, mul and concat and loop over them.  Return when valid combination
  ;; is found.
  (let* ((operator-slots (1- (length terms)))
         (possible-operator-combinations (expt base operator-slots)))
    ;; (format t "Operator slots is ~a and base is ~a, checking ~a combinations for goal ~a with terms ~a~%"
    ;;         operator-slots base possible-operator-combinations goal terms)
    (loop :for combination :from 0 :below possible-operator-combinations
          :for evaluated = (evaluate terms combination base)
          ;; :when t
          ;;   :do (progn
          ;;         (format t "~%----------------~%Evaluated: ~a, Goal: ~a; ~a~%" evaluated goal (= evaluated goal))
          ;;         (format t "Combination ~a yields: ~a~%" combination (print-solution-and-goal terms combination goal base))

          ;;         )
          ;; :when (= evaluated goal)
          ;;   :return (print-solution-and-goal terms combination goal base)
          :when (= evaluated goal)
            :return t)
    )
  )

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (loop :for parts :in input
        :for count = 0 :then (1+ count)
        :for goal = (first parts)
        :for terms = (rest parts)
        :for solvable = (equation-solvable-p goal terms)
        :when solvable
          :sum goal))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (loop :for parts :in input
        :for goal = (first parts)
        :for terms = (rest parts)
        :for solvable = (equation-solvable-p goal terms 3)
        :when solvable
          :sum goal))


(defun compare-parts (input)
  (let ((part1 (loop :for parts :in input
                     :for goal = (first parts)
                     :for terms = (rest parts)
                     :collect (equation-solvable-p goal terms 2)))
        (part2 (loop :for parts :in input
                     :for goal = (first parts)
                     :for terms = (rest parts)
                     :collect (equation-solvable-p goal terms 3)))
        )

    (loop :for result1 :in part1
          :for result2 :in part2
          :for count = 0 :then (1+ count)
          :when (not (equal result1 result2))
            :do (format t "At index ~a, part1: ~a, part2: ~a~%" count result1 result2)
          :when (not (equal result1 result2))
            :sum (parse-integer result2 :junk-allowed t)
          )
    
    )
  )

;; Part 1: 2941973819040
;; Part 2: 3076148546298
;;          134200695622


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
