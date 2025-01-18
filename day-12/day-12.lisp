;;;; day-12.lisp

;;;; A few definitions
;;;; -----------------
;;;; GARDEN   - a 2D grid representing plots where PLANTS grow. Each (row col)
;;;;            POSITION in the GARDEN has a PLOT where a PLANT grows.
;;;; PLOT     - a position in the GARDEN where a PLANT can grow.
;;;; PLANT    - plants are represented by a single character.
;;;; POSITION - a (row col) tuple, the coordinates for a PLOT.
;;;; REGION   - a list of PLOTS where the same PLANTS grow, and each PLOT is
;;;;            horizontally or vertically right next to another PLOT with the
;;;;            same PLANT.

(in-package #:day-12)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse INPUT, a list of strings, into solution-friendly format.
   Input represents a 2D grid of plant plot indicators. Return an array with two
   dimensions, where (row col) position (0 0) is 'top-left' in the input, and
   each value in the array is the corresponding character from the input."
  (let ((rows (length lines))
        (cols (length (first lines))))
    (make-array `(,rows ,cols)
     :element-type 'character :adjustable nil :fill-pointer nil :displaced-to nil
     :initial-contents (mapcar (lambda (line)
                                 (coerce line 'list))
                               lines))))

(defun plant-list (garden)
  "Given a GARDEN, return a list of unique plants."
  (let ((plants nil))
    (dotimes (idx (1- (array-total-size garden)))
      (pushnew (row-major-aref garden idx) plants :test #'char=))
    plants))

(defun plant-plots (plant garden)
  "Given a PLANT and a GARDEN, return a list of positions for plots
   growing the PLANT."
  (destructuring-bind (rows cols) (array-dimensions garden)
    (loop :for row :from 0 :below rows
          :nconc (loop :for col :from 0 :below cols
                       :when (char= (aref garden row col) plant)
                       :collect `(,row ,col))) ))

(defun inside-p (plot garden)
  "Return T if PLOT is inside GARDEN, else NIL."
  (destructuring-bind (rows cols) (array-dimensions garden)
    (declare (ignore rows))
    (destructuring-bind (row col) plot
      (and (>= row 0) 
           (>= col 0)
           (> (array-total-size garden) (+ (* row cols) col))))))

(defun is-neighbor-p (a b)
  "Return T if positions A and B are horizontally or vertically right next to each
   other, else NIL. Does not take into account if positions are inside garden."
  (destructuring-bind (row-a col-a) a
    (destructuring-bind (row-b col-b) b
      (or
       (and (= (abs (- row-a row-b)) 1)
            (= (abs (- col-a col-b)) 0))
       (and (= (abs (- row-a row-b)) 0)
            (= (abs (- col-a col-b)) 1))))))

(defun possible-neighbors (plot)
  "Return all possible neighbor plots for given PLOT as a list of plots. No bounds
   checking against a garden."
  (destructuring-bind (plot-row plot-col) plot
    `((,(1+ plot-row) ,plot-col)
      (,(1- plot-row) ,plot-col)
      (,plot-row ,(1+ plot-col))
      (,plot-row ,(1- plot-col)))))

(defun neighbors (plot garden)
  "Return a list of plots that are (valid) neighbors of PLOT in GARDEN."
  (remove-if-not
   (lambda (this-plot)
     (inside-p this-plot garden))
   (possible-neighbors plot)))

(defun partition-one-region (positions)
  "Find one region in POSITIONS, return (IN-REGION NOT-IN-REGION).
   First position in POSITION is the STARTING-POSITION. IN-REGION is a list of
   all positions in POSITION that are direct or indirect neighbors of
   STARTING-POSITION. NOT-IN-REGION is a list of all positions in POSITION that
   are *not* direct or indirect neighbors of STARTING-POSITION. It's basically a
   kind of flood fill. This is *not* the most efficient implementation."
  (let ((in-region nil)
        (stack `(,(first positions))))
    (loop
      :when (null stack)
        :return `(,in-region ,(set-difference positions in-region))
      :do (progn
            (let ((current-position (pop stack)))
              (pushnew current-position in-region :test #'equal)
              (dolist (neighbor
                       (remove-if-not
                        (lambda (test-position)
                          (and
                           (null (member test-position in-region :test #'equal))
                           (is-neighbor-p test-position current-position)))
                        positions))
                (pushnew neighbor stack :test #'equal)
                (pushnew neighbor in-region :test #'equal)))))))

;;   0 1 2 3 4 5 6 7 8 9
;;  +-------------------+
;; 0|R R R R I I C C F F| 0
;; 1|R R R R I I C C C F| 1
;; 2|V V R R R C C F F F| 2
;; 3|V V R C C C J F F F| 3
;; 4|V V V V C J J C F E| 4
;; 5|V V I V C C J J E E| 5
;; 6|V V I I I C J J E E| 6
;; 7|M I I I I I J J E E| 7
;; 8|M I I I S I J E E E| 8
;; 9|M M M I S S J E E E| 9
;;  +-------------------+
;;   0 1 2 3 4 5 6 7 8 9

(defun plant-regions (plots)
  "Given PLOTS, a list of plots in the garden where a certain plant grows,
   return a list of regions."
  (let ((remaining-positions (copy-list plots))
        (regions nil))
    (loop
      :when (null remaining-positions)
        :return regions
      :do (destructuring-bind (in-region not-in-region)
              (partition-one-region remaining-positions)
            (push in-region regions)
            (setf remaining-positions not-in-region)))))

(defun perimeter-part-2 (region)
  "Perimeter cost for part 2.
   Given a REGION, calculate perimiter according to perimeter formula in part 2.

   This one is a bit more complicated. Idea is to
   1) Figure out the non-region neighbors,
   2) Sort non-region neighbors on row value, then column value, ascending
   3) Run a 'slice' through each row, starting at leftmost column, going right across columns,
   4) Collect all column values for the slice into a list,

   5) Find the 'segments' in the column value list, where a segment is a
      sequence of integers with no whole number gaps.
   6) Return number of segments as the perimeter value for that row,
   7) Take next row, repeat from 3 until all rows are checked.

   When the rows are done, do the same but for columns.

   Phew. Let's try it."
  )

(defun perimeter-part-1 (region)
  "Perimeter cost for part 1.
   Given a REGION, calculate perimiter according to perimeter formula in part 1."
  (loop :for plot :in region
        :for candidates = (possible-neighbors plot)
        :for non-region-neighbors = (set-difference candidates region :test #'equal)
        :summing (length non-region-neighbors)))

(defun region-cost (plant garden perimeter-fn)
  "Given a PLANT and a GARDEN, return region cost, which is a sum of area *
   perimeter for each region for the PLANT in the GARDEN."
  (let* ((plots (plant-plots plant garden))
         (regions (plant-regions plots)))
    (loop :for region :in regions
          :for region-area = (length region)
          :for region-perimeter = (funcall perimeter-fn region)
          :summing (* region-area region-perimeter))))

(defun solve-part-1 (garden)
  "Solve part 1 of puzzle."
  (loop :for plant :in (plant-list garden)
        :sum (region-cost plant garden #'perimeter-part-1)))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
    (loop :for plant :in (plant-list garden)
        :sum (region-cost plant garden #'perimeter-part-2)))

(defun main (&optional (mode :full))
  "AoC 2024 day 12 solution.
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
