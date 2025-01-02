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
   position combinations as a list of (r1 c1 r2 c2) lists."
  (loop :for cur-pos-idx :from 0 :below (1- (length antenna-positions))
        :for cur-pos = (nth cur-pos-idx antenna-positions)
        :for r1 = (first cur-pos)
        :for c1 = (second cur-pos)
        :nconcing (loop :for combo-pos-idx :from (1+ cur-pos-idx) :below (length antenna-positions)
                        :for combo-pos = (nth combo-pos-idx antenna-positions)
                        :for r2 = (first combo-pos)
                        :for c2 = (second combo-pos)
                        :collect (list r1 c1 r2 c2))))

(defun antinode-positions (antenna-pos-pair mapping)
  "Calculate positions of antinodes given antenna pair position and a mapping."
  (let* ((r1 (first antenna-pos-pair))
         (c1 (second antenna-pos-pair))
         (r2 (third antenna-pos-pair))
         (c2 (fourth antenna-pos-pair))
         (r-delta (abs (- r1 r2)))
         (c-delta (abs (- c1 c2)))
         (a1-r -1)
         (a1-c -1)
         (a2-r -1)
         (a2-c -1)
         (antinodes ()))

         ;; vertical, three cases
         ;; v1) c1 is above c2
         ;; v2) c1 is horizontal to c2
         ;; v3) c1 is below c2
         ;;
         ;; horizontal, three cases
         ;; h1) c1 is left of c2
         ;; h2) c1 is vertical to c2
         ;; h3) c1 is right of c2

    (when (>= c2 c1)
      (setf a1-c (- c1 c-delta)
            a2-c (+ c2 c-delta))) ;; v1 & v2
    (when (> c1 c2)
      (setf a1-c (+ c1 c-delta)
            a2-c (- c2 c-delta))) ;; v3
    (when (>= r2 r1)
      (setf a1-r (- r1 r-delta)
            a2-r (+ r2 r-delta))) ;; h1 & h2
    (when (> r1 r2)
      (setf a1-r (+ r1 r-delta)
            a2-r (- r2 r-delta))) ;; h3

    (when (in-bounds-p a1-r a1-c mapping)
      (push (list a1-r a1-c) antinodes))
    (when (in-bounds-p a2-r a2-c mapping)
      (push (list a2-r a2-c) antinodes))    
    antinodes))

(defun antinodes (freq mapping)
  "Return a list of (row col) pairs for each antinode for given frequency and
   mapping."
  (let* ((antenna-positions (antenna-pos-per-freq freq mapping))
         (antenna-pairs (combine-antenna-positions antenna-positions)))
    (loop :for antenna-pair :in antenna-pairs
          :nconcing (antinode-positions antenna-pair mapping))
    )
  
  
  )

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

(defun tuple-less-p (tpl1 tpl2)
  "Predicate should return true if and only if the first argument is strictly less
   than the second (in some appropriate sense). If the first argument is greater
   than or equal to the second (in the appropriate sense), then the predicate
   should return false."

  This sorter predicate does not work.
  
  (cond ((< (first tpl1) (first tpl2)) t)
        ((< (second tpl1) (second tpl2)) t)
        (t nil)))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (let* ((freqs (frequencies input))
         (antenna-positions (loop :for freq
                                    :in freqs
                                  :nconcing (antenna-pos-per-freq freq input)))
         (antinode-positions (loop :for freq
                                     :in freqs
                                   :nconcing (antinodes freq input)))
         )
    (intersection (sort antenna-positions :test #'equal)
                  (sort antinode-positions :test #'equal))
    
    )

  )

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
