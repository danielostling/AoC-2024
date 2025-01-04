;;;; day-8.lisp

(in-package #:day-8)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.

   Convert lines of text into array of characters."
  (let* ((cols (length (first lines)))
         (rows (length lines))
         (arr (make-array `(,rows ,cols) :element-type 'character)))
    (loop :for line :in lines
          :for row = 0 :then (1+ row)
          :do (loop :for c :across line
                    :for col = 0 :then (1+ col)
                    :do (setf (aref arr row col) c)))
    arr))

(defun tuple-less-p (tpl1 tpl2)
  "Predicate should return true if and only if the first argument is strictly less
   than the second (in some appropriate sense). If the first argument is greater
   than or equal to the second (in the appropriate sense), then the predicate
   should return false."
  (cond ((< (first tpl1) (first tpl2)) t)
        ((> (first tpl1) (first tpl2)) nil)
        ((< (second tpl1) (second tpl2)) t)
        (t nil)))

(defun frequencies (mapping)
  "Given a mapping, return a list of all frequencies in mapping."
  (let ((rows (array-dimension mapping 0))
        (cols (array-dimension mapping 1))
        (freqs ()))
    (loop :for idx :from 0 :below (* rows cols)
          :for val = (row-major-aref mapping idx)
          :when (not (or (char= val #\.) (char= val #\#)))
            :do (pushnew val freqs))
    freqs))

(defun antenna-pos-per-freq (freq mapping)
  "Given a frequency and a mapping, return a list of (row col) positions in
   mapping for that frequency."
  (let ((rows (array-dimension mapping 0))
        (cols (array-dimension mapping 1)))
    (loop :for row :from 0 :below rows
          :nconcing (loop :for col :from 0 :below cols
                          :for val = (aref mapping row col)
                          :when (char= val freq)
                            :collect (list row col)))))

(defun in-bounds-p (row col mapping)
  "Return t if (row col) is inside dimensions of mapping, else nil."
  (let ((rows (array-dimension mapping 0))
        (cols (array-dimension mapping 1)))
    (not
     (or (minusp row)
         (minusp col)
         (>= row rows)
         (>= col cols)))))

(defun combine-antenna-positions (antenna-positions)
  "Given a list of (row col) pairs for antennas, return a list of all pairwise
   position combinations as a list of ((r1 c1) (r2 c2)) lists."
  (loop :for cur-pos-idx :from 0 :below (1- (length antenna-positions))
        :for cur-pos = (nth cur-pos-idx antenna-positions)
        :for r1 = (first cur-pos)
        :for c1 = (second cur-pos)
        :nconcing (loop :for combo-pos-idx :from (1+ cur-pos-idx) :below (length antenna-positions)
                        :for combo-pos = (nth combo-pos-idx antenna-positions)
                        :for r2 = (first combo-pos)
                        :for c2 = (second combo-pos)
                        :for pairs = (sort
                                      (list
                                       (list r1 c1)
                                       (list r2 c2))
                                      #'tuple-less-p)
                        :collect pairs)))

;; (defun antinode-positions (antenna-pos1 antenna-pos2 mapping)
;;   "Calculate positions of antinodes given antenna pair position and a mapping.

;;    antenna-pos1 and antenna-pos2 are (row col) tuples representing antenna
;;    positions.

;;    An antinode is described in the puzzle as:
;;      '... , an antinode occurs at any point that is perfectly in line with two
;;      antennas of the same frequency - but only when one of the antennas is twice
;;      as far away as the other. This means that for any pair of antennas with the
;;      same frequency, there are two antinodes, one on either side of them.'

;;    Tuples antenna-pos1 and antenna-pos2 are sorted, so that lowest row comes
;;    first. If rows are equal, lowest col comes first. This eliminates some
;;    positioning combinations.

;;    The following combinations are possible
;;    Combination 1, p1 is above and left compared to p2.
;;    +--------+
;;    | p1     |
;;    |        |
;;    |     p2 |
;;    +--------+

;;    Combination 2, p1 is left of p2 and horizontally aligned to p2 (as row is
;;    equal, col sorting puts p1 to the left).
;;    +--------+
;;    |        |
;;    | p1  p2 |
;;    |        |
;;    +--------+

;;    Combination 3, p1 is above and right compared to p2.
;;    +--------+
;;    |     p1 |
;;    |        |
;;    | p2     |
;;    +--------+
 
;;    Combination 4, p1 is above p2 and vertically aligned to p2.
;;    +--------+
;;    |   p1   |
;;    |        |
;;    |   p2   |
;;    +--------+"
;;   (let* ((r1 (first antenna-pos1))
;;          (c1 (second antenna-pos1))
;;          (r2 (first antenna-pos2))
;;          (c2 (second antenna-pos2))
;;          (r-delta (abs (- r1 r2)))
;;          (c-delta (abs (- c1 c2)))
;;          (a1-r nil)
;;          (a1-c nil)
;;          (a2-r nil)
;;          (a2-c nil)
;;          (antinodes ()))

;;     ;; Combination 1, p1 is above and left compared to p2.
;;     (when (and (> c2 c1)
;;                (> r2 r1))
;;       (setf a1-c (- c1 c-delta)
;;               a1-r (- r1 r-delta)
;;               a2-c (+ c2 c-delta)
;;               a2-r (+ r2 r-delta)))
    
;;     ;; Combination 2, p1 is left of p2 and horizontally aligned to p2.
;;     (when (and (> c2 c1)
;;                (= r2 r1))
;;       (setf a1-c (- c1 c-delta)
;;               a1-r r1
;;               a2-c (+ c2 c-delta)
;;               a2-r r1))

;;     ;; Combination 3, p1 is above and right compared to p2.
;;     (when (and (< c2 c1)
;;                (> r2 r1))
;;       (setf a1-c (+ c1 c-delta)
;;               a1-r (- r1 r-delta)
;;               a2-c (- c2 c-delta)
;;               a2-r (+ r2 r-delta)))

;;     ;; Combination 4, p1 is above p2 and vertically aligned to p2.
;;     (when (and (= c2 c1)
;;                (> r2 r1))
;;       (setf a1-c c1
;;               a1-r (- r1 r-delta)
;;               a2-c c2
;;               a2-r (+ r2 r-delta)))
    
;;     (when (in-bounds-p a1-r a1-c mapping)
;;       (push (list a1-r a1-c) antinodes))
;;     (when (in-bounds-p a2-r a2-c mapping)
;;       (push (list a2-r a2-c) antinodes))    
;;     antinodes))


(defun get-vector (p1 p2)
  "Return a vector v such that p1 + v = p2.

   p1 and p2 are (row col) tuples, row and col are integers."
  (mapcar #'- p2 p1))

(defun antinode-positions (antenna-pos1 antenna-pos2 mapping)
  (let* ((rows (array-dimension 0 mapping))
         (cols (array-dimension 1 mapping))
         (vector-p1-to-p2 (get-vector antenna-pos2 antenna-pos1))
         (vector-p2-to-p1 (get-vector antenna-pos1 antenna-pos2))
         )

    )
  )

(defun antinodes (freq mapping)
  "Return a list of (row col) pairs for each antinode for given frequency and
   mapping."
  (let* ((antenna-positions (antenna-pos-per-freq freq mapping))
         (antenna-pairs (combine-antenna-positions antenna-positions)))
    (loop :for antenna-pair :in antenna-pairs
          :for antenna-pos1 = (first antenna-pair)
          :for antenna-pos2 = (second antenna-pair)
          :nconcing (antinode-positions antenna-pos1 antenna-pos2 mapping))
    ))

;; Examples from puzzle description.
;;
;; 0 1 2 3 4 5 6 7 8 9 
;; . . . . . . . . . . 0
;; . . . # . . . . . . 1
;; # . . . . . . . . . 2
;; . . . . a . . . . . 3
;; . . . . . . . . a . 4
;; . . . . . a . . . . 5
;; . . # . . . . . . . 6
;; . . . . . . # . . . 7
;; . . . . . . . . . . 8
;; . . . . . . . . . . 9

;;                     1 1 
;; 0 1 2 3 4 5 6 7 8 9 0 1 
;; . . . . . . # . . . . # 0
;; . . . # . . . . 0 . . . 1
;; . . . . # 0 . . . . # . 2
;; . . # . . . . 0 . . . . 3
;; . . . . 0 . . . . # . . 4
;; . # . . . . A . . . . . 5
;; . . . # . . . . . . . . 6
;; # . . . . . . # . . . . 7
;; . . . . . . . . A . . . 8
;; . . . . . . . . . A . . 9
;; . . . . . . . . . . # . 10
;; . . . . . . . . . . # . 11

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (let* ((freqs (frequencies input))
         (antinode-positions
           (sort (loop :for freq :in freqs
                       :nconcing (antinodes freq input))
            #'tuple-less-p))
         (unique-antinode-positions
           (remove-duplicates antinode-positions :test #'equal)))
    (length unique-antinode-positions)))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  )

(defun main (&optional (mode :full))
  "AoC 2024 day 8 solution.
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
