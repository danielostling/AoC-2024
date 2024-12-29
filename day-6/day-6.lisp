;;;; day-6.lisp

(in-package #:day-6)


(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.

   Input is a 2D map of characters . and #, and a single guard position marked
   by a single ^ character.

   Make a 2D array, and replace . by 0, # by -1. Return 2D array and (row col)
   of guard position."
  (let* ((cols (length (first lines)))
         (rows (length lines))
         (room (make-array `(,rows ,cols)))
         (guard-pos nil))
    (loop :for line :in lines
          :for row = 0 :then (1+ row)
          :do (loop :for char :in (coerce line 'list)
                    :for col = 0 then (1+ col)
                    :do (cond ((char= char #\.) (setf (aref room row col) 0))
                              ((char= char #\#) (setf (aref room row col) -1))
                              (t (setf guard-pos `(,row ,col))))))
    `(,room ,guard-pos)))
  
(defun guard-step-function (direction)
  "Return a function that determines next coordinate of the guard given current
   coordinate, and next-direction.

   Note: Not sure how to get rid of the unused variable warnings for the lambdas
   cleanly."
  (case direction
    (:up    (lambda (row col) (list (1- row) col)))
    (:right (lambda (row col) (list row (1+ col))))
    (:down  (lambda (row col) (list (1+ row) col)))
    (:left  (lambda (row col) (list row (1- col))))))

(defun guard-turn (prev-direction)
  "Given previous direction, return new direction of guard.

   Guard rotates right 90 degrees each turn."
  (case prev-direction
    (:up :right)
    (:right :down)
    (:down :left)
    (:left :up)))

(defun guard-left-room-p (guard-row guard-col room-rows room-cols)
  "Check if guard stepped outside of the room boundaries.

   All arguments are integers. Return t if guard is outside, nil else."
  (cond ((>= guard-row room-rows) t)
        ((minusp guard-row) t)
        ((>= guard-col room-cols) t)
        ((minusp guard-col) t)
        (t nil)))

(defun walk-room (room guard-pos)
  "Predict how guard will move, and return room (destructively) updated with
   coordinates visited byguard.

   room is a 2D array of room tiles and objects, guard-pos is a (row col) list
   of indices into room array where the guard starts. Assume first direction is
   up."
  (let* ((guard-row (first guard-pos))
         (guard-col (second guard-pos))
         (guard-direction :up)
         (step-fn (guard-step-function guard-direction))
         (room-rows (array-dimension room 0))
         (room-cols (array-dimension room 1)))
    (setf (aref room guard-row guard-col) 1)
    (loop
      :for (guard-next-row guard-next-col) = (funcall step-fn guard-row guard-col)
      :when (guard-left-room-p guard-next-row guard-next-col room-rows room-cols)
        :return room
      :if (minusp (aref room guard-next-row guard-next-col))
        :do (progn
              (setf guard-direction (guard-turn guard-direction))
              (setf step-fn (guard-step-function guard-direction)))
      :else
        :do (setf guard-row guard-next-row
                  guard-col guard-next-col)
      :do (setf (aref room guard-row guard-col) 1))))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."

  (let* ((room (first input))
         (room-rows (array-dimension room 0))
         (room-cols (array-dimension room 1))
         (guard-pos (second input))
         (walked-room (walk-room room guard-pos)))
    (loop :for room-index :from 0 :below (* room-rows room-cols)
          :counting (plusp (row-major-aref walked-room room-index)))))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  )

(defun main (&optional (mode :full))
  "AoC 2024 day 6 solution.
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
