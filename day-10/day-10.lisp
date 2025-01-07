;;;; day-10.lisp

(in-package #:day-10)

;;;; The following definitions are used in this code.
;;;;
;;;; - The topo-map is a 2D array of integers from 0 to 9, representing the elevation at various
;;;;   positions in the topo-map. 0 is lowest and 9 is highest.
;;;; - A position in the topo-map is a (row col) integer tuple.
;;;; - A trailhead is a position where the elevation is 0.
;;;; - A destination is a position where the elevation is 9.
;;;; - A trail is a path across the topo-map, starting at a trailhead (or fork position) and ending
;;;;   at a destination.
;;;; - A step is a move from one position to another position. Valid directions are north, east,
;;;;   south and west. Steps must be 1 in length and go from one elevation to the next higher elevation.
;;;; - A fork in a trail is a position where a trail takes two or three directions. The position of
;;;;   the fork in the topo-map is the first step on the forked trail.
;;;; - Trail score is defined as the number of destinations reachable directly or by forks from a trailhead.

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.

   From puzzle description:

   'The topographic map indicates the height at each position using a scale from 0 (lowest) to
    9 (highest). For example:

    0123
    1234
    8765
    9876'.

   Read the lines and build a 2D array of integers to represent the map."
  (let* ((rows (length lines))
         (cols (length (first lines)))
         (topo-map (make-array `(,rows ,cols)
                    :element-type 'unsigned-byte
                    :adjustable nil
                    :fill-pointer nil
                    :displaced-to nil)))
    (loop :for line :in lines
          :for row = 0 :then (1+ row)
          :do (loop :for char :across line
                    :for col = 0 :then (1+ col)
                    :do (setf
                         (row-major-aref topo-map (+ (* row rows) col))
                         (digit-char-p char))))
    topo-map))

(defun get-trailheads (topo-map)
  "Return a list of positions where elevation is 0 in topo-map."
  (destructuring-bind
      (rows cols)
      (array-dimensions topo-map)
    (loop :for row :from 0 :below rows
          :nconcing (loop :for col :from 0 :below cols
                          :when (zerop (aref topo-map row col))
                            :collect `(,row ,col)))))

(defun elevation-or-nil (row col topo-map)
  "Return elevation at position or nil if outside of topo-map."
  (destructuring-bind (rows cols) (array-dimensions topo-map)
    (if (or (minusp row)
            (minusp col)
            (> row (1- rows))
            (> col (1- cols)))
        nil
        (aref topo-map row col))))

(defun peek (position direction topo-map)
  "Standing at position, look one step in direction on topo-map.

   Return elevation of peeked position or nil if outside topo-map."
  (destructuring-bind (row col) position
    (case direction
      (:north (elevation-or-nil (1- row) col topo-map))
      (:east  (elevation-or-nil row (1+ col) topo-map))
      (:south (elevation-or-nil (1+ row) col topo-map))
      (:west  (elevation-or-nil row (1- col) topo-map)))))

(defun step (start topo-map)
  "Starting at position start, take a (valid) step.

   Return (new-position elevation-at-new-position forks)."

  (flet ((scan-for-forks (position topo-map)))


    )
  
  )

(defun walk-trail (trailhead topo-map)
  "Walk a trail from trailhead across topo-map to find a destination and possible forks.
   Return a list (destination forks) where forks is a list of trail fork positions."
  )

(defun score-trail (trailhead topo-map)
  "Return the score of the trail starting at trailhead."
  (let ((remaining-forks `(,trailhead))
        (destinations ()))
    (loop
      :for cur-trailhead = (pop remaining-forks)
      :for (destination new-forks) = (walk-trail cur-trailhead topo-map)
      :when (and (null remaining-forks)
                 (null new-forks))
        :return (pushnew destination destinations :test #'equal)  ;; Don't like this, not sure how to avoid.
      :do (progn
            (pushnew destination destinations :test #'equal)
            (nconc new-forks remaining-forks)))))

(defun score-trails (topo-map)
  "Find all trails and their trail scores. Return as list of (trailhead score) tuples."
  (let ((trailheads (get-trailheads topo-map)))
    (loop :for trailhead :in trailheads
          :collect (score-trail trailhead topo-map))))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (loop :for (trailhead score) :in (score-trails input)
        :summing score))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  )

(defun main (&optional (mode :full))
  "AoC 2024 day 10 solution.
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
