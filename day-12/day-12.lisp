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
                       :collect `(,row ,col)))))

(defun plot-value (plot garden)
  "Return plot plant at PLOT in GARDEN or NIL if outside of GARDEN."
  (if (inside-p plot garden)
      (destructuring-bind (row col) plot
        (aref garden row col))
      nil))
   
(defun inside-p (plot garden)
  "Return T if PLOT is inside GARDEN, else NIL."
  (destructuring-bind (rows cols) (array-dimensions garden)
    (destructuring-bind (row col) plot
      (and (>= row 0) 
           (>= col 0)
           (> rows row)
           (> cols col)))))

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
   STARTING-POSITION. NOT-IN-REGION is a list of all positions in POSITIONS that
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

;;   0 1 2 3 4 5 6 7 8 9
;;  +-------------------+
;; 0|R R R R I I C C F F| 0
;; 1|R R R R I I C C C F| 1
;; 2|V V R R R C C.F.F F| 2
;; 3|V V R C C C J.F.F F| 3
;; 4|V V V V C J J.C.F E| 4
;; 5|V V I V C C J J E E| 5
;; 6|% V I I I C J J E E| 6
;; 7%M % I I . I J J E E| 7
;; 8%M % % . S . J E E E| 8
;; 9%M M M % S S . E E E| 9
;;  +%-%-%---. .--------+
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

(defun sort-plots-by-row (plots)
  "Sort PLOTS by row and then columns, ascending.
   From CLHS on sort function
   '... sort and stable-sort destructively sort sequences according to the order
    determined by the predicate function.'
   and
   '... The first argument to the predicate function is the part of one element
    of sequence extracted by the key function (if supplied); the second argument
    is the part of another element of sequence extracted by the key function (if
    supplied).  Predicate should return true if and only if the first argument
    is strictly less than the second (in some appropriate sense). If the first
    argument is greater than or equal to the second (in the appropriate sense),
    then the predicate should return false. ...'."

  (flet ((compare (plot-a plot-b)
           "Compare PLOTS PLOT-A and PLOT-B by row and then by column."
           (destructuring-bind (plot-a-row plot-a-col) plot-a
             (destructuring-bind (plot-b-row plot-b-col) plot-b
               (cond ((> plot-b-row plot-a-row) t)
                     ((< plot-b-row plot-a-row) nil)
                     (t (cond ((> plot-b-col plot-a-col) t)
                              ((< plot-b-col plot-a-col) nil)
                              (t nil))))))))
    ;; Duplicates should be removed.
    (remove-duplicates (sort (copy-list plots) #'compare) :test #'equal)))

(defun sort-plots-by-col (plots)
  "Sort PLOTS by column and then row, ascending.
   From CLHS on sort function
   '... sort and stable-sort destructively sort sequences according to the order
    determined by the predicate function.'
   and
   '... The first argument to the predicate function is the part of one element
    of sequence extracted by the key function (if supplied); the second argument
    is the part of another element of sequence extracted by the key function (if
    supplied).  Predicate should return true if and only if the first argument
    is strictly less than the second (in some appropriate sense). If the first
    argument is greater than or equal to the second (in the appropriate sense),
    then the predicate should return false. ...'."

  (flet ((compare (plot-a plot-b)
           "Compare PLOTS PLOT-A and PLOT-B by row and then by column."
           (destructuring-bind (plot-a-row plot-a-col) plot-a
             (destructuring-bind (plot-b-row plot-b-col) plot-b
               (cond ((> plot-b-col plot-a-col) t)
                     ((< plot-b-col plot-a-col) nil)
                     (t (cond ((> plot-b-row plot-a-row) t)
                              ((< plot-b-row plot-a-row) nil)
                              (t nil))))))))
    ;; Duplicates should be removed.
    (remove-duplicates (sort (copy-list plots) #'compare) :test #'equal)))

(defun perimeter-part-2 (plant region garden)
  "Perimeter cost for part 2.
   Given a REGION, calculate perimiter according to perimeter formula in part 2.

   I'm not very proud of this function. I'm certain there must be a much more
   efficient way.

   This one is a bit more complicated. Idea is to
   1) Figure out the non-region neighbors,
   2) Sort non-region neighbors on row value, then column value, ascending
   3) Run a 'slice' through each row, starting at leftmost column, going right
      across columns,
   4) Collect all column values for the slice into a list,
   5) Find the 'segments' in the column value list, where a segment is a
      sequence of integers with no whole number gaps.
   6) Return number of segments as the perimeter value for that row,
   7) Take next row, repeat from 3 until all rows are checked.

   When the rows are done, do the same but for columns.

   So, step 4, how is it done?

   The idea is to detect where possible segments are, determined by the
   surroundings. Example: plant is S, other plants are represented by other
   letters, and plots outside garden are represented by a 0.

   In this case, scan is done between max and min row and col coordinates of all
   plots containing an S.

   Top-to-bottom scan
   ==================
       col   0 1 2 3 4 5 6 7 8 9
           +---------------------+
   row ->  | M I I I I I J J E E | 
   row + 1 | M I I I S I J E E E |
   row + 2 | M M M I S S J E E E | 
   row + 3 | 0 0 0 0 0 0 0 0 0 0 | 
   
   The scan looks at row,and row + 1. The only plots that should be fenced are
   the S:es that are bordering other plots or the edge of the garden.

   - Starting at row, start at column 4 and move right. Record the column value
     for each plot where plot value is different from region plant (or nil) and
     plot value directly below is region plant, which is only at plot (row 4).
   - At row+1, there is one plot where plot value is not region plant and plot
     value directly below is region plant, at (row+1 5).
   - At row+2, there are no plots where plot value not region plant and plot value directly
     below is rgon plant.
   - Row+3 is not scanned for top-to-bottom scan.

   This means the collected plots are (row 4) and (row+1 5).

   Next steps; 1) scan bottom to top, then 2) scan left to right and finally 3)
   left to right. Each scan will result in a list of columns or rows, (per row
   or column), which are then passed to the segments function to detect
   continous series of numbers."
  (let* ((non-region-neighbors
           (loop :for plot :in region
                 :for candidates = (possible-neighbors plot)
                 :for non-region-neighbor-set = (set-difference candidates region :test #'equal)
                 :nconcing non-region-neighbor-set))
         (neighbors-sorted-by-row (sort-plots-by-row non-region-neighbors))
         (neighbors-sorted-by-col (sort-plots-by-col non-region-neighbors))
         (min-row (first (first neighbors-sorted-by-row)))
         (max-row (first (first (reverse neighbors-sorted-by-row))))
         (min-col (second (first neighbors-sorted-by-col)))
         (max-col (second (first (reverse neighbors-sorted-by-col))))
         (region-segments nil))
    
    ;; This is most likely *far* from the optimal solution. But, it works.
    
    ;; By rows, top to bottom, left to right.      
    ;; Collect each col value where plot is a direct neighbor to region at (row+1, col)
    (loop :for cur-row :from min-row :below max-row
          :for segment-parts = (loop :for cur-col :from min-col :to max-col
                                     :for plot-plant = (plot-value `(,cur-row ,cur-col) garden)
                                     :for plot-neighbor = `(,(1+ cur-row) ,cur-col)
                                     :when (and
                                            (or
                                             (null plot-plant)
                                             (not (equal plot-plant plant)))
                                            (equal
                                             (plot-value plot-neighbor garden)
                                             plant)
                                            (member plot-neighbor region :test #'equal))
                                       :collect cur-col) ;; For rows, collect the column only.
          :when segment-parts
            :do (setf region-segments (nconc region-segments (segments segment-parts))))

    ;; By rows, bottom to top, left to right.
    ;; Collect each col value where plot is a direct neighbor to region at (row-1, col)
    (loop :for cur-row :from max-row :above min-row
          :for segment-parts = (loop :for cur-col :from min-col :to max-col
                                     :for plot-plant = (plot-value `(,cur-row ,cur-col) garden)
                                     :for plot-neighbor = `(,(1- cur-row) ,cur-col)
                                     :when (and
                                            (or
                                             (null plot-plant)
                                             (not (equal plot-plant plant)))
                                            (equal (plot-value plot-neighbor garden)
                                                   plant)
                                            (member plot-neighbor region :test #'equal))
                                       :collect cur-col) ;; For rows, collect the column only.
          :when segment-parts
            :do (setf region-segments (nconc region-segments (segments segment-parts))))

    ;; By cols, left to right, top to bottom.
    ;; Collect each row value where plot is a direct neighbor to region at (row, col+1)
    (loop :for cur-col :from min-col :below max-col
          :for segment-parts = (loop :for cur-row :from min-row :to max-row
                                     :for plot-plant = (plot-value `(,cur-row ,cur-col) garden)
                                     :for plot-neighbor = `(,cur-row ,(1+ cur-col))
                                     :when (and
                                            (or
                                             (null plot-plant)
                                             (not (equal plot-plant plant)))
                                            (equal (plot-value plot-neighbor garden)
                                                   plant)
                                            (member plot-neighbor region :test #'equal))
                                       :collect cur-row) ;; For cols, collect the row only.
          :when segment-parts
            :do (setf region-segments (nconc region-segments (segments segment-parts))))

    ;; By cols, right to left, top to bottom.
    ;; Collect each row value where plot is a direct neighbor to region at (row, col-1)
    (loop :for cur-col :from max-col :above min-col
          :for segment-parts = (loop :for cur-row :from min-row :to max-row
                                     :for plot-plant = (plot-value `(,cur-row ,cur-col) garden)
                                     :for plot-neighbor = `(,cur-row ,(1- cur-col))
                                     :when (and
                                            (or
                                             (null plot-plant)
                                             (not (equal plot-plant plant)))
                                            (equal (plot-value plot-neighbor garden)
                                                   plant)
                                            (member plot-neighbor region :test #'equal))
                                       :collect cur-row) ;; For cols, collect the row only.
          :when segment-parts
            :do (setf region-segments (nconc region-segments (segments segment-parts))))
    (length region-segments)))

(defun segments (numbers)
  "Given a list of whole numbers, return a list of segments.
   A segment is a list of numbers where the numbers are consecutive."
  (let ((found-segments nil))
    (loop
      :with current-segment = nil
      :for n :in numbers
      :if (or
           (null current-segment)
           (= (1+ (first current-segment)) n))
        :do (push n current-segment)
      :else
        :do (progn
              (push current-segment found-segments)
              (setf current-segment `(,n)))
      :finally (progn
                 (when current-segment
                   (push current-segment found-segments))))
    found-segments))

(defun perimeter-part-1 (plant region garden)
  "Perimeter cost for part 1.
   Given a REGION, calculate perimiter according to perimeter formula in part 1."
  (declare (ignorable plant garden))
  (loop :for plot :in region
        :for candidates = (possible-neighbors plot)
        :for non-region-neighbors = (set-difference candidates region :test #'equal)
        :summing (length non-region-neighbors)))

(defun region-cost (plant garden perimeter-fn)
  "Given a PLANT, a GARDEN, and a perimeter length function PERMIMETER-FN. return
   region cost, which is a sum of area * perimeter for each region for the PLANT
   in the GARDEN."
  (let* ((plots (plant-plots plant garden))
         (regions (plant-regions plots)))
    (loop :for region :in regions
          :for region-area = (length region)
          :for region-perimeter = (funcall perimeter-fn plant region garden)
          :summing (* region-area region-perimeter))))

(defun solve-part-1 (garden)
  "Solve part 1 of puzzle."
  (loop :for plant :in (plant-list garden)
        :sum (region-cost plant garden #'perimeter-part-1)))

(defun solve-part-2 (garden)
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
