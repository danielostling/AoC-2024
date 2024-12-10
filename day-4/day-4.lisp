;;;; day-4.lisp

(in-package #:day-4)

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

(defun get-row (arr row)
  "Return given row from given array."
  (let ((cols (array-dimension arr 1)))
    (make-array cols
                :element-type 'character
                :displaced-to arr 
                :displaced-index-offset (* row cols))))

(defun get-diagonal (arr row col)
  "Build a 45 degree diagonal 'slice' from arr, starting at given row and
   col, going up and left in equal amounts.

   If array is
   0 1 2 3
   4 5 6 7
   8 9 a b
   c d e f

   Then, a call to get-diagonal with array and
   - row = 2, col = 0 would return (8 5 2)
   - row = 0, col = 0 would return (0)
   - row = 3, col = 0 would return (c 9 6 3)
   - row = 3, col = 1 would return (d a 7)
   ... and so on.

   NOTE: No bounds checking is done. Keep given row and col within array
         dimensions."
  (let ((cols (array-dimension arr 1)))
    (loop
      :for cur-row = row :then (1- cur-row)
      :for cur-col = col :then (1+ cur-col)
      :until (or (minusp cur-row) (= cur-col cols))
      :collect (aref arr cur-row cur-col))))

(defun get-diagonals (arr)
  "Iterate across all 45 degree diagonals from left and bottom edges in array,
   starting from top-left corner, then moving down left side (as starting point)
   until lower-left corner.  Then switch direction, moving starting point from
   lower-left corner to the right until lower-right corner.

   If array is
   0 1 2 3
   4 5 6 7
   8 9 a b
   c d e f

   First starting point is (0, 0), second is (1, 0), continuing to (3, 0).
   Next, direction changes to right, (3, 1), then (3, 2) and finally (3, 3).

   Return diagonals as a list of lists."
  (let ((rows (array-dimension arr 0))
        (cols (array-dimension arr 1))
        (diagonals nil))
    ;; First go down left edge.
    (loop :for row :from 0 :below rows
          :for diag = (get-diagonal arr row 0)
          :do (push
               (coerce (get-diagonal arr row 0) 'string)
               diagonals))
    
    ;; Next go right along bottom edge.
    (loop :for col :from 0 :below cols
          :do (push
               (coerce (get-diagonal arr (1- rows) col) 'string)
               diagonals))
    diagonals))

(defun scan-left-to-right (arr)
  "Search array from left to right, row by row, for XMAS. Return number of
   hits as integer."
  (let* ((xmas "XMAS")
         (rows (array-dimension arr 0)))
    (loop :for row :from 0 :below rows
          :summing (cl-ppcre:count-matches xmas (get-row arr row)))))

(defun rotate-right (arr)
  "Rotate array right by 90 degrees."
  (let* ((rows (array-dimension arr 0))
         (cols (array-dimension arr 1))
         (new-arr (make-array `(,rows ,cols) :element-type 'character)))
    (loop :for row :from 0 :below rows
          :do (loop :for col :from 0 :below cols
                    :do (setf (aref new-arr col (- cols 1 row))
                              (aref arr row col))))
    new-arr))

(defun scan-horiz-and-vert (arr)
  "Scan left to right and then rotate the array and scan again. Doing this four
   times at rotation 0 degrees, 90 degrees, 180 degrees and 270 degrees, will
   cover horizontal and vertical cases, as well as backwards occurrances of
   XMAS. Return number of found cases of the string."
  (let ((cur-arr (alexandria:copy-array arr)))
    (loop :for angle :from 90 :to 360 :by 90
          :do (progn
                (setf cur-arr (rotate-right cur-arr))
                (format t "horiz/vert: rotated to ~a degrees~%" angle))
          :sum (scan-left-to-right cur-arr))))

(defun scan-string-list (lst)
  "Scan string list for XMAS. Return number of hits."
  (let ((xmas "XMAS"))
    (loop :for elt :in lst
          :summing (cl-ppcre:count-matches xmas elt))))

(defun scan-diagonals (arr)
  "Get all diagonals of arr and scan them left to right. Then rotate the array,
   get the new diagonals, and scan again. Doing this four times at rotation 0
   degrees, 90 degrees, 180 degrees and 270 degrees, will cover all directional
   cases of XMAS. Return number of found cases of the string."

  (let ((cur-arr (alexandria:copy-array arr)))
    ;; Start at 90 degrees because of how loop directive order sets aggregation last.
    (loop :for angle :from 90 :to 360 :by 90
          :do (progn
                (setf cur-arr (rotate-right cur-arr))
                (format t "diag: rotated once to ~a degrees~%" angle))
          :sum (scan-string-list (get-diagonals cur-arr)))))



(defun test-arr ()
  (make-array '(3 3)
              :element-type 'character
              :initial-contents '((#\A #\B #\C)
                                  (#\D #\E #\F)
                                  (#\G #\H #\I))))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (+ (scan-horiz-and-vert input)
     (scan-diagonals input)))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  )

(defun main (&optional (mode :full))
  "AoC 2024 day 2 solution.
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
