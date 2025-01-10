;;;; day-10.lisp

(in-package #:day-10)

;;;; This is a graph traversal problem. I will assume graph(s) are DAGs because of puzzle
;;;; description of valid steps.
;;;;
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
;;;; - A hiking trail is a distinct path from trailhead to destination, inclusive.
;;;; - A path is a list of positions.

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
  "Return t if other-position at elevation other-elevation is walkable from current-elevation, else
   nil. Test looks a bit weirc, but that's because other-postion and other-elevation may be nil."
  (and other-position other-elevation (= other-elevation (1+ current-elevation))))

(defun possible-steps (position topo-map)
  "Return list of all valid, possible steps from position in topo-map."
  (let ((current-elevation (elevation-or-nil position topo-map)))
    (loop :for direction :in '(:west :north :east :south)
          :for (possible-position possible-elevation) = (peek position direction topo-map)
          :when (valid-next-step-p possible-position possible-elevation current-elevation)
            :collect possible-position)))

(defun topo-map-to-dags (topo-map)
  "Convert a topo-map to a 'DAG hash'.
   Depending on the topo-map, this can really generate multiple isolated graphs. Reachability is
   dependent on starting node.
   - Hash keys are (row col) tuples in topo-map.
   - Hash values are (elevation reachable-nodes dag-id) triples where elevation is the topo-map
     elevation at hash key position, reachable-nodes is a list of (row col) tuples that are
     reachable from hash key position, and dag-id is an integer indicating which dag the key node
     belongs to."
  (let ((dag (make-hash-table :test #'equal)))
    (destructuring-bind (rows cols) (array-dimensions topo-map)
      (loop :for row :from 0 :below rows
            :do (loop :for col :from 0 :below cols
                      :for node = `(,row ,col)
                      :for reachable-nodes = (possible-steps node topo-map)
                      :for elevation = (elevation-or-nil node topo-map)
                      :do (setf (gethash node dag) `(,elevation ,reachable-nodes))))
      dag)))

(defun get-nodes-by-value (val dag &optional (test #'equal))
  "Return a list of nodes from dag where elevation is equal to val."
  (loop :for node :being :the :hash-key :using (:hash-value (elevation nil)) :of dag
        :when (funcall test val elevation)
          :collect node))

(defun path-exists (start-node goal-node dag)
  "Return t if any path exists between start-node and goal-node in dag, else nil"
  (labels ((dfs (current-node visited-nodes)
             (cond
               ((equal current-node goal-node) t)
               ((member current-node visited-nodes :test #'equal) nil)
               (t (some (lambda (neighbor-node)
                          (dfs neighbor-node (cons current-node visited-nodes)))
                        (second (gethash current-node dag)))))))
    (dfs start-node nil)))

(defun destinations-per-trailhead (trailhead dag)
  "Return list of destinations reachable from trailhead in dag."
  (let ((possible-destinations (get-nodes-by-value 9 dag)))
    (loop :for possible-destination :in possible-destinations
          :when (path-exists trailhead possible-destination dag)
            :collect possible-destination)))

(defun paths-between (trailhead destination dag)
  (let ((paths nil))
    (labels
        ((dfs (current-node path)
           "Collect paths between start-node and goal-node in dag."
           (push current-node path)
           (if (equal current-node destination)
               (push (reverse path) paths)
               (dolist (next-node (second (gethash current-node dag)))
                 (dfs next-node path)))))
      (dfs trailhead '())
      paths)))

(defun trails-per-trailhead (trailhead dag)
  "Return a list of distinct hiking paths from trailhead to reachable destinations."
  (let ((destinations (destinations-per-trailhead trailhead dag)))
          (loop :for destination :in destinations
                :nconc (paths-between trailhead destination dag))))

(defun score-trails (topo-map &optional (measure :destinations))
  "Find all trails and their trail scores. Return as list of (trailhead score) tuples.

  measure is one of :destinations or :paths.
    - When :destination is used, scoring is number of destinations reachable from given trailhead.
    - When :paths is used, scoring is number of distint paths from trailhead to a destination."
  (let* ((dag (topo-map-to-dags topo-map))
         (trailheads (get-nodes-by-value 0 dag))
         (score-func (case measure
                       (:destinations #'destinations-per-trailhead)
                       (:paths #'trails-per-trailhead))))
    (loop :for trailhead :in trailheads
          :collect `(,trailhead ,(funcall score-func trailhead dag)))) )

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
;;    0 1 2 3 4 5 6 7 


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
         :summing (length destinations)))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (loop :for (trailhead paths) :in (score-trails input :paths)
        :summing (length paths)))

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
