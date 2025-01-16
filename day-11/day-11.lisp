;;;; day-11.lisp

;;;; I can not take credit for this solution. I didn't quite make the recursive
;;;; bits work properly with a different memoization.
;;;; This is an adoption of https://youtu.be/pVfsmQSlVOQ
;;;;
;;;; On my MacBook Pro M1 Max, no optimizations other than memoization
;;;; -----------------------------------------------------------------
;;;; DAY-11> (time (main :full))
;;;; Part 1: 183620
;;;; Part 2: 220377651399268
;;;; Evaluation took:
;;;;   0.042 seconds of real time
;;;;   0.041363 seconds of total run time (0.036146 user, 0.005217 system)
;;;;   97.62% CPU
;;;;   33,593,216 bytes consed

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

(defparameter *cache* (make-hash-table :test #'equal))

(defun stone-count (stone-marking blinks)
  "Given stone marking and blinks, return the number of stones there will be after
   that many blinks. This is a recursive function."
  (flet ((return-via-cache (&rest args)
           "Memoization to reduce computation time using global *cache* hash.
            More or less from OnLisp by Graham.

            args are the function parameter values, and for this implementation,
            the same function is always called, which is the stone-count
            function. If the parameter values was already computed, return the
            result from the cache, else compute it and store into cache for
            future speed-up."
           (multiple-value-bind (result exists) (gethash args *cache*)
             (if exists
                 result
                 (setf (gethash args *cache*)
                       (apply #'stone-count args))))))
    (if (zerop blinks)
        1
        (if (zerop stone-marking)
            (return-via-cache 1 (1- blinks))
            (let* ((stone-as-str (format nil "~a" stone-marking))
                   (stone-str-len (length stone-as-str)))
              (if (evenp stone-str-len)
                  (let* ((split-idx (/ stone-str-len 2))
                         (left-stone (parse-integer (subseq stone-as-str 0 split-idx)))
                         (right-stone (parse-integer (subseq stone-as-str split-idx))))
                    (+
                     (return-via-cache left-stone (1- blinks))
                     (return-via-cache right-stone (1- blinks))))
                  (return-via-cache (* stone-marking 2024) (1- blinks))))))))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (loop :for stone :in input
        :sum (stone-count stone 25)))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (loop :for stone :in input
        :sum (stone-count stone 75)))

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
