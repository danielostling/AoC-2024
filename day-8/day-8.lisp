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

(defun get-vector (v1 v2)
  "Return a vector v' such that v1 + v' = v2.

   v1 and v2 are (row col) tuples, row and col are integers."
  (mapcar #'- v2 v1))

(defun add-vectors (v1 v2)
  "Return a vector v' such that v' = v1 + v2.

   v1 and v2 are (row col) tuples, row and col are integers."
  (mapcar #'+ v1 v2))

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

(defun get-antinode-positions (antenna-pos1 antenna-pos2 mapping &optional (harmonics nil))
  "Return list of antinode positions going from antenna-pos1 towards antenna-pos 2.

   mapping is the puzzle input map.  If harmonics is not nil, count each
   harmonic on the way as well. Else, just count one antinode."
  (let* ((directional-vec (get-vector antenna-pos1 antenna-pos2))
         (positions ()))
    (loop
      :for counter = 1 :then (1+ counter)
      :for cur-pos = (if harmonics
                         antenna-pos1
                         (add-vectors antenna-pos2 directional-vec))
        :then (add-vectors cur-pos directional-vec)
      :when (or
             (null (in-bounds-p (first cur-pos) (second cur-pos) mapping))
             (and
              (not harmonics)
              (> counter 1)))
        :return positions
      :do (pushnew cur-pos positions :test #'equal))
    positions))

(defun antinodes (freq mapping &optional (harmonics nil))
  "Return a list of (row col) pairs for each antinode for given frequency and
   mapping."
  (let* ((antenna-positions (antenna-pos-per-freq freq mapping))
         (antenna-pairs (combine-antenna-positions antenna-positions)))
    (loop :for antenna-pair :in antenna-pairs
          :for antenna-pos1 = (first antenna-pair)
          :for antenna-pos2 = (second antenna-pair)
          :for antinodes-1-to-2 = (get-antinode-positions antenna-pos1 antenna-pos2 mapping harmonics)
          :for antinodes-2-to-1 = (get-antinode-positions antenna-pos2 antenna-pos1 mapping harmonics)
          :for both-directions = (append antinodes-1-to-2 antinodes-2-to-1)
          :nconcing both-directions)))

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

(defun solve-part-1 (input &optional (harmonics nil))
  "Solve part 1 of puzzle."
  (let* ((freqs (frequencies input))
         (antinode-positions
           (sort (loop :for freq :in freqs
                       :nconcing (antinodes freq input harmonics))
                 #'tuple-less-p))
         (unique-antinode-positions
           (remove-duplicates antinode-positions :test #'equal)))
    (length unique-antinode-positions)))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (solve-part-1 input t))

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
