;;;; day-11.lisp

(in-package #:day-11)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.
   Input is a single string of integer numbers separated by a single
   whitespace. Return a list of these numbers in the same order they are found
   in the input."
  (mapcar #'parse-integer (uiop:split-string (first lines))))

(defun transform (stone-marking)
  "Transform a stone-marking according to puzzle rules and return new stone-marking
   or stone-markings depending on rules.

   Rules
   -----
   1. If the stone is engraved with the number 0, it is replaced by a stone
      engraved with the number 1.

   2. If the stone is engraved with a number that has an even number of digits,
      it is replaced by two stones. The left half of the digits are engraved on
      the new left stone, and the right half of the digits are engraved on the
      new right stone. (The new numbers don't keep extra leading zeroes: 1000
      would become stones 10 and 0.)

   3. If none of the other rules apply, the stone is replaced by a new stone;
      the old stones number multiplied by 2024 is engraved on the new stone."
  ;;(format t "  transform: current-stone: ~a~%" stone-marking)
  (if (zerop stone-marking)
      1
      (let* ((str-digits (format nil "~a" stone-marking))
             (marking-length (length str-digits)))
        (if (evenp marking-length)
            (let* ((split-idx (/ marking-length 2))
                   (first-stone (parse-integer (subseq str-digits 0 split-idx)))
                   (second-stone (parse-integer (subseq str-digits split-idx))))
              `(,first-stone ,second-stone))
            (* stone-marking 2024)))))

(defun transform-with-blinks (input-stone transformations)
  (destructuring-bind (stone blinks) input-stone
    (let ((transformed-stone (gethash stone transformations))
          (next-blink (1+ blinks)))
      (mapcar (lambda (elt) `(,elt ,next-blink)) transformed-stone)
      ;; (if (typep transformed-stone 'integer)
      ;;     `(,transformed-stone ,next-blink)
      ;;     `((,(first transformed-stone) ,next-blink)
      ;;       (,(second transformed-stone) ,next-blink))
      ;;     )
      )
    )
  )

(defun generate-cycles-manually ()
  "Return a hash with starting 0-9 numbers and rule results as value.

   Rules
   -----
   1. If the stone is engraved with the number 0, it is replaced by a stone
      engraved with the number 1.

   2. If the stone is engraved with a number that has an even number of digits,
      it is replaced by two stones. The left half of the digits are engraved on
      the new left stone, and the right half of the digits are engraved on the
      new right stone. (The new numbers don't keep extra leading zeroes: 1000
      would become stones 10 and 0.)

   3. If none of the other rules apply, the stone is replaced by a new stone;
      the old stones number multiplied by 2024 is engraved on the new stone."
  (let ((progression (make-hash-table :test #'equal)))
    (setf (gethash 0 progression) 1)
    (setf (gethash 1 progression) (* 1 2024))
    (setf (gethash 2 progression) (* 2 2024))
    (setf (gethash 3 progression) (* 3 2024))
    (setf (gethash 4 progression) (* 4 2024))
    (setf (gethash 5 progression) (* 5 2024))
    (setf (gethash 6 progression) (* 6 2024))
    (setf (gethash 7 progression) (* 7 2024))
    (setf (gethash 8 progression) (* 8 2024))
    (setf (gethash 9 progression) (* 9 2024))
    (setf (gethash 2024 progression) '(20 24))   ;; 1 * 2024 = 2024 => 20, 24
    (setf (gethash 4048 progression) '(40 48))   ;; 2 * 2024 = 4048 => 40, 48
    (setf (gethash 6072 progression) '(60 72))   ;; 3 * 2024 = 6072 => 60, 72
    (setf (gethash 8096 progression) '(80 96))   ;; 4 * 2024 = 8096 => 80, 96
    (setf (gethash 10120 progression) 20482880)  ;; 5 * 2024 = 10120 => 20482880
    (setf (gethash 12144 progression) 24579456)  ;; 6 * 2024 = 12144 => 24579456
    (setf (gethash 14168 progression) 28676032)  ;; 7 * 2024 = 14168 => 28676032
    (setf (gethash 16192 progression) 32772608)  ;; 8 * 2024 = 16192 => 32772608
    (setf (gethash 18216 progression) 36869184)  ;; 9 * 2024 = 18216 => 36869184
    (setf (gethash 20 progression) '(2 0))
    (setf (gethash 24 progression) '(2 4))
    (setf (gethash 40 progression) '(4 0))
    (setf (gethash 48 progression) '(4 8))
    (setf (gethash 60 progression) '(6 0))
    (setf (gethash 72 progression) '(7 2))
    (setf (gethash 80 progression) '(8 0))
    (setf (gethash 96 progression) '(9 6))
    (setf (gethash 20482880 progression) '(2048 2880))
    (setf (gethash 24579456 progression) '(2457 9456))
    (setf (gethash 28676032 progression) '(2867 6032))
    (setf (gethash 32772608 progression) '(3277 2608))
    (setf (gethash 36869184 progression) '(3686 9184))
    (setf (gethash 2048 progression) '(20 48))
    (setf (gethash 2880 progression) '(28 80))
    (setf (gethash 2457 progression) '(24 57))
    (setf (gethash 9456 progression) '(94 56))
    (setf (gethash 2867 progression) '(28 67))
    (setf (gethash 6032 progression) '(60 32))
    (setf (gethash 3277 progression) '(32 77))
    (setf (gethash 2608 progression) '(26 08))
    (setf (gethash 3686 progression) '(36 86))
    (setf (gethash 9184 progression) '(91 84))
    (setf (gethash 28 progression) '(2 8))
    (setf (gethash 57 progression) '(5 7))
    (setf (gethash 94 progression) '(9 4))
    (setf (gethash 56 progression) '(5 6))
    (setf (gethash 28 progression) '(2 8))
    (setf (gethash 67 progression) '(6 7))
    (setf (gethash 32 progression) '(3 2))
    (setf (gethash 77 progression) '(7 7))
    (setf (gethash 26 progression) '(2 6))
    (setf (gethash 36 progression) '(3 6))
    (setf (gethash 86 progression) '(8 6))
    (setf (gethash 91 progression) '(9 1))
    (setf (gethash 84 progression) '(8 4))
    progression))

(defun make-progression (base-numbers)
  "Generate and return stone marking progresion cycles hash.

   If transformation rules are followed, it turns out previous starting numbers
   will appear again after a few blinks.

   Generate the possible base cases and return them as a hash where starting
   stone mark is the hash key (an integer).

   The hash values are integers or a list of two integers, depending on what the
   rules state for the hash key. The hash value integers and lists of integers
   are keys in the hash as well. In essence, the hash is a directed graph of all
   traversal possibilities based on the rules."
  (let ((stone-progression (make-hash-table :test #'equal)))
    (loop :for base-number :in base-numbers
          :do (loop
                :with current-stones = `(,base-number)
                :when (null current-stones)
                  :do (return)
                :do (progn
                      (if (gethash (first current-stones) stone-progression)
                          (pop current-stones)
                          (let* ((current-stone (pop current-stones))
                                 (transformed (transform current-stone)))
                            (if (typep transformed 'integer)
                                (progn
                                  (setf (gethash current-stone stone-progression) `(,transformed))
                                  (push transformed current-stones))
                                (progn
                                  (setf (gethash current-stone stone-progression) transformed)
                                  (push (first transformed) current-stones)
                                  (push (second transformed) current-stones))))))))
    stone-progression))

(defun expand-stone-marking-steps (blinks input)
  "Given a blink count and an input sequence, return an expansion hash.
   The hash key is a starting stone-marking. The hash value is a list of lists,
   where each list represents another level of blink expansion.

   For example, starting at 0 and five blinks
   - Hash key is 0
   - Hash value is
     ((1) (2024) (20 24) (2 0 2 4) (4048 0 4048 8096))"
  (let* ((expanded-blinks (make-hash-table :test #'equal))
         (base-expansions (make-progression input))
         (starting-stone-stack (alexandria:hash-table-keys base-expansions)))
    (loop
      :for current-starting-stone = (pop starting-stone-stack)
      :when (null current-starting-stone)
        :return expanded-blinks
      :do (let ((expansion `(,(gethash current-starting-stone base-expansions)))
                (temp-stack nil))
            (dotimes (blink (1- blinks))
              (setf temp-stack
                    (mapcan (lambda (stone)
                              (copy-list (gethash stone base-expansions)))
                            (first expansion)))
              (push temp-stack expansion)
              (setf temp-stack nil))
            (setf (gethash current-starting-stone expanded-blinks) expansion)))))

;; (defun blink (times input)
;;   "Return number of stones after TIMES blinks, given INPUT as starting point.
;;    TIMES is a base 10 positive integer.
;;    INPUT is a list of base 10 integers 0 or larger.
;;    Returns a base 10 integer."

;;   (let ((base-expansion (make-progression input))
;;         (20x-expansion (expand-stone-marking-steps 20 input))
;;         (5x-expansion (expand-stone-marking-steps 5 input))
;;         (stone-stack input)
;;         (stone-count 0))
;;     (loop
;;       :for current-stone = (pop stone-stack)
;;       :unless current-stone
;;         :return stone-count
      

;;       )
;;     )
  
;;   )

(defun run-one (times input)
  (let ((transformations (make-progression input))
        (path `(,(first input)))
        )

    (dotimes (blink times)
      (let ((next-stone (gethash (first path) transformations))
            )
        (if (typep next-stone 'integer)
            (push next-stone path)
            (push (first next-stone) path)
            )
        )
      )
    path
    )

  )


(defun blink (times input)
  "Return how many stones the rules will produce after times blinks."
  (let* ((stone-stack (mapcar (lambda (stone) (list stone 0)) (copy-list input)))
         (transformations (make-progression input))
         (current-stone nil)
         (stone-count 0)
         (loop-counter 0))
    (setf current-stone (pop stone-stack))
    (format t "blink: start; stone-stack=~a, current-stone=~a~%"
            stone-stack current-stone)
    (loop
      :when t
        :do (incf loop-counter)
;;       :when (zerop (mod loop-counter 10000000))
;;         :do (progn
;; ;;              (incf stone-stack-checks)
;; ;;              (incf stone-stack-size (length stone-stack))
;; ;;              (setf avg-stone-stack-size (/ stone-stack-size stone-stack-checks))
;;               (format t "blink: stone-stack: ~a stones~%"
;;                       (length stone-stack)))
      ;; :when t
      ;;   :do (format t "blink: stone-count=~a, stone-stack (~a stones), current-stone=~a~%"
      ;;               stone-count (length stone-stack) current-stone)
      :when (and
             (null stone-stack)
             (null current-stone))
        :do (progn
              (format t "blink: all stones blinked ~a times, exiting~%" times)
              (return stone-count))
      :when (= (second current-stone) times)
        :do (progn
;;              (format t "blink: hit blink limit for ~a, removing~%" current-stone)
              (incf stone-count)
              (setf current-stone (pop stone-stack)))
      :if (and
             current-stone
             (< (second current-stone) times)) ;; Popped stone may already be at blink limit.
        :do (let* ((new-stone (gethash (first current-stone) transformations))
                   (next-blink (1+ (second current-stone)))
                   (new-stone-with-blinks
                     (mapcar (lambda (elt) `(,elt ,next-blink)) new-stone)))
      
 ;;             (format t "blink; new-stone-with-blinks=~a~%" new-stone-with-blinks)
              (if (= (length new-stone-with-blinks) 2)
                  (progn
 ;;                   (format t "blink: new-stone-with-blinks split into two~%")
                    (setf current-stone (first new-stone-with-blinks))
                    (push (second new-stone-with-blinks) stone-stack))
                  (setf current-stone (first new-stone-with-blinks))))
  ;;          :else :do (format t "blink: skipping transformation, current-stone: ~a~%" current-stone)
      )))
 
(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (blink 1 input))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (blink 75 input)
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
