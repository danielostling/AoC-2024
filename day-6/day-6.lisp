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
         (room (make-array `(,rows ,cols) :adjustable nil
                                          :fill-pointer nil
                                          :displaced-to nil))
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

(defun guard-loops-p (room guard-pos)
  "Return t if given single obstacle position in room would cause the guard to
   'loop', else nil.

   This one is a bit too complicated for me to do an optimal solution. I think I
   have to brute force it and see how long it takes.

   Loop detection
   --------------
   Keep track of visited positions, and what direction guard had.
   If a new position with a direction appears in tracked list, the position was
   visited before and in the same direction. This means the guard is looping."
  (let* ((guard-row (first guard-pos))
         (guard-col (second guard-pos))
         (guard-direction :up)
         (step-fn (guard-step-function guard-direction))
         (room-rows (array-dimension room 0))
         (room-cols (array-dimension room 1))
         (guard-pos-and-dir ())
         (visited-positions ()))
    (setf (aref room guard-row guard-col) 1)
    (push (list guard-row guard-col guard-direction) visited-positions)
    (loop
      :for guard-steps = 1 :then (1+ guard-steps)
      :for (guard-next-row guard-next-col) = (funcall step-fn
                                                      guard-row
                                                      guard-col)
      ;; Guard does not loop.
      :when (guard-left-room-p guard-next-row guard-next-col room-rows room-cols)
        :return nil 
          
      ;; If next move would bump into an obstacle, turn right 90 degrees.
      :if (minusp (aref room guard-next-row guard-next-col))
        :do (progn
              (setf guard-direction (guard-turn guard-direction))
              (setf step-fn (guard-step-function guard-direction))
              (setf guard-pos-and-dir (list guard-row guard-col guard-direction)))
      :else ;; No obstacle, take a step.
      :do (progn
            (setf guard-row guard-next-row
                  guard-col guard-next-col)
            (setf guard-pos-and-dir (list guard-row guard-col guard-direction)))
      :if (member guard-pos-and-dir visited-positions :test #'equal)
        :return (length visited-positions) ;; Guard is looping.
      :else  ;; If this position and direction is new, store it.
      :do (push guard-pos-and-dir visited-positions))))

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
  "Solve part 2 of puzzle.

   This one is a bit too complicated for me to do an optimal solution. I think I
   have to brute force it and see how long it takes."
  (flet ((skip-pos-p (room row col)
           "Return t if a position is to be skipped over, else nil.

            row and col are integer indices of room position, and room is the
            walked room from part 1.

            A position is skipped if 1) there is an obstacle in there, or 2) if
            the room position is zero. A zero value position means that there is
            no obstacle there, and the guard never sat foot on the position. If
            a single new obstacle is to be placed, it must be somewhere in the
            original walked room guard path. Anything outside can be skipped."
           ;; Room pos = -1 means there is an obstacle there.
           ;; Room pos = 1 means guard has walked there.
           ;; So, skip all positions that are exactly zero; no obstacle, no guard.
           (zerop (aref room row col)))
         (progress (row col rows-total cols-total)
           (* 100 (/ (+ col (* rows-total row)) (* rows-total cols-total)))))
    (let* ((room (first input))
           (room-rows (array-dimension room 0))
           (room-cols (array-dimension room 1))
           (guard-pos (second input))
           (room-with-obstacle nil)
           (loop-count 0)
           (walked-room (walk-room room guard-pos))
           (skippable-positions (loop :for room-index
                                       :from 0
                                         :below (* room-rows room-cols)
                                     :counting (zerop
                                                (row-major-aref
                                                 walked-room room-index)))))
      (format t "There are ~a/~a skippable positions~%"
              skippable-positions (* room-rows room-cols))
      (loop :for obstacle-row :from 0 :below room-rows
            :do (progn
                  (format t "Checking row ~a~%" obstacle-row)
                  (loop :for obstacle-col :from 0 :below room-cols
                        :unless (skip-pos-p walked-room obstacle-row obstacle-col)
                          :do (progn
                                (setf room-with-obstacle (alexandria:copy-array room))
                                (setf (aref room-with-obstacle obstacle-row obstacle-col) -1)
                                (when (guard-loops-p room-with-obstacle guard-pos)
                                  (progn
                                    (format t "Loop with obstacle at ~a, ~a (~,2F%)~%"
                                            obstacle-row obstacle-col
                                            (progress obstacle-row obstacle-col
                                                      room-rows room-cols))
                                    (incf loop-count)))))))
      loop-count)))

(defun main (&optional (mode :full))
  "AoC 2024 day 6 solution.
   Mode is one of
   :full - use full puzzle input
   :test - use test puzzle input"
  ;; Beware, second part of the puzzle is not a fast solution. Sorry.
  (let* ((path-input-full #P"./input-full")
         (path-input-test #P"./input-test")
         (path-input (if (equal mode :full)
                         path-input-full
                         path-input-test))
         (input (parse-input (read-input path-input))))
    (format t "Part 1: ~a~%" (solve-part-1 input))
    (format t "Part 2: ~a~%" (solve-part-2 input))))
