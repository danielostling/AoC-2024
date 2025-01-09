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

(defun elevation-or-nil (position topo-map)
  "Return elevation at position or nil if outside of topo-map."
  (if position
      (destructuring-bind (rows cols) (array-dimensions topo-map)
        (destructuring-bind (row col) position
          (if (or (minusp row)
                  (minusp col)
                  (> row (1- rows))
                  (> col (1- cols)))
              nil
              (aref topo-map row col))))
      nil))

(defun peek (position direction topo-map)
  "Standing at position, look one step in direction on topo-map.

   Return (peeked-position elevation-at-peeked-position), or nil if outside topo-map."
  (if position
      (destructuring-bind (row col) position
        (let* ((peeked-position
                 (case direction
                   (:north `(,(1- row) ,col))
                   (:east  `(,row ,(1+ col)))
                   (:south `(,(1+ row) ,col))
                   (:west  `(,row ,(1- col)))))
               (elevation-at-peeked-position (elevation-or-nil peeked-position topo-map)))
          (when elevation-at-peeked-position
            `(,peeked-position ,elevation-at-peeked-position))))
      nil))

(defun valid-next-step-p (other-position other-elevation current-elevation)
  (and other-position other-elevation (= other-elevation (1+ current-elevation))))

(defun possible-steps (position topo-map)
  "Return list of all valid, possible steps from position in topo-map."
  (let ((current-elevation (elevation-or-nil position topo-map)))
    (loop :for direction :in '(:west :north :east :south)
          :for (possible-position possible-elevation) = (peek position direction topo-map)
          ;; :when t
          ;;   :do (format t "      possible-steps: at ~a looking ~a towards ~a; is it a step? ~a~%"
          ;;               position direction possible-position (valid-next-step-p possible-position possible-elevation current-elevation))
          :when (valid-next-step-p possible-position possible-elevation current-elevation)
            :collect possible-position)))

(defun take-one-step (start topo-map)
  "Starting at position start, take a (valid) step.

   Return (new-position elevation-at-new-position forks). new-position,
   elevation-at-new-position and forks will be nil if there are no possible
   steps."
  (if (null start)
      '(nil nil nil)
      (let* ((forks (possible-steps start topo-map))
             (next-position (pop forks)))
        ;; (format t "      take-one-step: at ~a, move to ~a and deal with ~a later~%"
        ;;         start next-position forks)
        (if (null next-position)
            '(nil nil nil)
            `(,next-position
              ,(elevation-or-nil next-position topo-map)
              ,forks)))))

(defun walk-trail (trailhead topo-map)
  "Walk a trail from trailhead across topo-map to find a destination and possible
   forks.
   Return a list (destination forks) where forks is a list of trail fork
   positions.  If trail doesn't lead to a destination, return (nil forks)."
;;  (format t "    walk-trail: looking for initial forks at ~a~%" trailhead)
  (let ((forks (possible-steps trailhead topo-map))
        (current-position trailhead)
        (are-we-there-yet (equal (elevation-or-nil trailhead topo-map) 9)))
    ;; (format t "    walk-trail: initial forks: ~a~%" forks)
    ;; (format t "    walk-trail: trailhead = destination? ~a~%" are-we-there-yet)
    ;; First, check edge case where trail is part of a fork, and trailhead is
    ;; already at elevation 9.
    (if are-we-there-yet
        (progn
;;          (format t "    walk-trail: at destination, returning (~a ~a)~%" trailhead forks)
          `(,trailhead ,forks))
        (loop
          :for (new-position new-elevation new-forks) = (take-one-step current-position topo-map)
          ;; :when t
          ;;   :do (format t "    walk-trail: step ~a => ~a, elevation-next-pos = ~a, new-forks(old pos) = ~a, forks = ~a~%"
          ;;               current-position new-position new-elevation new-forks forks)
          :when new-forks
            :do (dolist (new-fork new-forks)
                  (pushnew new-fork forks :test #'equal))
          ;; :when (null new-position)
          ;;   :do (format t "    walk-trail: reached a nil position, this is a dead end, exiting~%")
          :when (null new-position)
            :return `(nil ,forks)
          ;; :when (equal new-elevation 9)
          ;;   :do (format t "    walk-trail: reached a destination at ~a, returning~%" new-position)
          :when (equal new-elevation 9)
            :return `(,new-position ,(remove new-position forks :test #'equal))
          :do (progn
                (setf current-position new-position)
;;                (format t "    walk-trail: now standing at ~a with forks ~a~%" current-position forks)
                (setf forks (remove current-position forks :test #'equal))
                ;; (format t "    walk-trail: removed current-position ~a from forks, now ~a~%"
                ;;         current-position forks)
                )

          ))))

(defun score-trail (trailhead topo-map)
  "Return the score of the trail starting at trailhead."
  (let ((remaining-forks `(,trailhead))
        (checked-forks nil)
        (destinations ()))
    ;; (format t "  score-trail: starting trail at ~a~%" trailhead)
    ;; (format t "  score-trail: looking for initial forks~%")
    (loop
      :for current-trailhead = (pop remaining-forks)
      :for (destination new-forks) = (walk-trail current-trailhead topo-map)
      :for counter = 100000 :then (1- counter)
      :when current-trailhead
        :do (progn
              ;; (format t "  score-trail: adding current-trailhead ~a to checked-forks ~a~%"
              ;;         current-trailhead checked-forks)
              (pushnew current-trailhead checked-forks :test #'equal))
      ;; :when t
      ;;   :do (format t "  score-trail: current-trailhead: ~a, destination: ~a, new-forks: ~a, remaining-forks: ~a~%"
      ;;               current-trailhead destination new-forks remaining-forks)
      :when destination
        :do (progn
              (pushnew destination destinations :test #'equal)
;;              (format t "  score-trail: added ~a to destinations, now ~a~%" destination destinations)
              )
      :when new-forks
        :do (progn
;;              (format t "  score-trail: added new forks ~a to remaining-forks~%" new-forks)
              (dolist (new-fork new-forks)
                (unless (member new-fork checked-forks :test #'equal)
                  (pushnew new-fork remaining-forks :test #'equal))))
      :when (and (null remaining-forks)
                 (null new-forks))
        :do (progn
              ;; (format t "  score-trail: after ~a loops, no more forks remain, returning destinations ~a~%"
              ;;         (- 100 counter) destinations)
              (return destinations))
        
      ;; :if (zerop counter)
      ;;   :do (progn
      ;;         (format t "  score-trail: ran out of loops, exiting~%")
      ;;         (return))
      ;; :else
      ;;   :do (format t "  score-trail: will next start another loop, examining trail fork ~a~%"
      ;;               (first remaining-forks))

      )
    ))

(defun score-trails (topo-map)
  "Find all trails and their trail scores. Return as list of (trailhead score) tuples."
  (let ((trailheads (get-trailheads topo-map)))
    (loop :for trailhead :in trailheads
          :for count = 2000000 :then (1- count)
          ;; :when t
          ;;   :do (format t "score-trails: checking trailhead ~a out of ~a~%" trailhead trailheads)
          :when (zerop count)
            :do (progn
;;                  (format t "score-trails: ran out of loops, exiting~%")
                  (return trail-scores))
          :collect `(,trailhead ,(score-trail trailhead topo-map)) :into trail-scores
          :finally (return trail-scores))
    )
  )

;;    0 1 2 3 4 5 6 7 
;;   +---------------+
;; 0 |8 9 0 1 0 1 2 3| 0
;; 1 |7 8 1 2 1 8 7 4| 1
;; 2 |8 7 4 3 0 9 6 5| 2
;; 3 |9 6 5 4 9 8 7 4| 3
;; 4 |4 5 6 7 8 9 0 3| 4
;; 5 |3 2 0 1 9 0 1 2| 5
;; 6 |0 1 3 2 9 8 0 1| 6
;; 7 |1 0 4 5 6 7 3 2| 7
;;   +---------------+
;;   0 1 2 3 4 5 6 7 


(defun test-one-head-rating-3 ()
  (make-array '(7 7)
              :element-type 'unsigned-byte
              :adjustable nil
              :fill-pointer nil
              :displaced-to nil
              :initial-contents '((15 15 15 15 15  0 15)
                                  (15 15  4  3  2  1 15)
                                  (15 15  5 15 15  2 15)
                                  (15 15  6  5  4  3 15)
                                  (15 15  7 15 15  4 15)
                                  (15 15  8  7  6  5 15)
                                  (15 15  9 15 15 15 15))))

(defun test-one-head-rating-13 ()
  (make-array '(7 7)
              :element-type 'unsigned-byte
              :adjustable nil
              :fill-pointer nil
              :displaced-to nil
              :initial-contents '((15 15  9  0 15 15  9)
                                  (15 15 15  1 15  9  8)
                                  (15 15 15  2 15 15  7)
                                  ( 6  5  4  3  4  5  6)
                                  ( 7  6  5 15  9  8  7)
                                  ( 8  7  6 15 15 15 15)
                                  ( 9  8  7 15 15 15 15))))

(defun test-one-head-rating-227 ()
  (make-array '(6 6)
              :element-type 'unsigned-byte
              :adjustable nil
              :fill-pointer nil
              :displaced-to nil
              :initial-contents '(( 0  1  2  3  4  5)
                                  ( 1  2  3  4  5  6)
                                  ( 2  3  4  5  6  7)
                                  ( 3  4  5  6  7  8)
                                  ( 4 15  6  7  8  9)
                                  ( 5  6  7  8  9 15))))

(defun test-9-heads-rating-81 ()
  (make-array '(8 8)
              :element-type 'unsigned-byte
              :adjustable nil
              :fill-pointer nil
              :displaced-to nil
              :initial-contents '((8 9 0 1 0 1 2 3)
                                  (7 8 1 2 1 8 7 4)
                                  (8 7 4 3 0 9 6 5)
                                  (9 6 5 4 9 8 7 4)
                                  (4 5 6 7 8 9 0 3)
                                  (3 2 0 1 9 0 1 2)
                                  (0 1 3 2 9 8 0 1)
                                  (1 0 4 5 6 7 3 2))))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (loop :for (trailhead destinations) :in (score-trails input)
         :summing (length destinations))
  )

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
