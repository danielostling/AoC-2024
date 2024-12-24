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

   and array is accessed by (row, col) pairs,
   then, a call to get-diagonal with array and
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

   (and array is accessed by (row, col) pairs)
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
    ;; NOTE: Start with col 1. Col 0 was added from previous loop.
    (loop :for col :from 1 :below cols
          :do (push
               (coerce (get-diagonal arr (1- rows) col) 'string)
               diagonals))
    diagonals))

(defun scan-left-to-right (arr &optional (re "XMAS"))
  "Search array from left to right, row by row, for XMAS. Return number of
   hits as integer."
  (let ((rows (array-dimension arr 0)))
    (loop :for row :from 0 :below rows
          :for matches = (cl-ppcre:count-matches re (get-row arr row))
          :summing matches)))

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

(defun scan-horiz-and-vert (arr &optional (re "XMAS"))
  "Scan left to right and then rotate the array and scan again. Doing this four
   times at rotation 0 degrees, 90 degrees, 180 degrees and 270 degrees, will
   cover horizontal and vertical cases, as well as backwards occurrances of
   XMAS. Return number of found cases of the string."
  (loop :for angle :from 0 :to 270 :by 90
          :for cur-arr = (alexandria:copy-array arr) then (rotate-right cur-arr)
          :sum (scan-left-to-right cur-arr re)))

(defun scan-string-list (lst &optional (re "XMAS"))
  "Scan string list for XMAS. Return number of hits."
  (loop :for elt :in lst
        ;; :do (format t "scan-string-list part ~a for re ~a~%" elt re)
        :summing (cl-ppcre:count-matches re elt)))

(defun scan-diagonals (arr &optional (re "XMAS") (scan-function #'scan-string-list))
  "Get all diagonals of arr and scan them left to right. Then rotate the array,
   get the new diagonals, and scan again. Doing this four times at rotation 0
   degrees, 90 degrees, 180 degrees and 270 degrees, will cover all directional
   cases of XMAS. Return number of found cases of the string."
  (loop :for angle :from 0 :to 270 :by 90
          :for cur-arr = (alexandria:copy-array arr) then (rotate-right cur-arr)
          :for diagonals = (get-diagonals cur-arr)
          :for matches = (funcall scan-function diagonals re)
          :sum (funcall scan-function (get-diagonals cur-arr) re)))

(defun populate-window (arr window-rows window-cols row-start col-start)
  "Copy a part of given array into a new array.
   Start the copy at position (row-start col-start).
   Note: no bounds checking is done."
  (let ((window (make-array (list window-rows window-cols)
                            :element-type (array-element-type arr))))
    ;; This loops rows and cols in the coordinate system of the given array.
    (loop :for arr-row :from row-start :below (+ row-start window-rows)
          :for window-row = 0 :then (1+ window-row)
          :do (loop :for arr-col :from col-start :below (+ col-start window-cols)
                    :for window-col = 0 :then (1+ window-col)
                    :do (setf (aref window window-row window-col)
                                (aref arr arr-row arr-col))))
    window))

(defun make-windows (bigger-arr)
  "Walk a (smaller) 3x3 square 'windows' across a bigger square, from left to
   right, starting in top left corner. When right side of smaller square reaches
   the right edge of the bigger square, move smaller square back to left edge of
   bigger square, but one row down.  Keep going until smaller square reaches
   lower right corner of the bigger square.

   Return a list of all the smaller window squares, which each smaller square is
   a list of lists; The 3x3 square (where . represents a blank space)
   M . S
   . A .
   M . S

   is represented by
   ((#\M #\  #\S)
    (#\  #\A #\ )
    (#\M #\  #\S))

   Top left corner of the bigger square is at row 0 and column 0. Columns grow
   to the right, rows grow down."

  ;; The top left corner of the small square will move inside the bigger square,
  ;; from first col to two cols less than the col count of the larger square.
  ;;
  ;; For rows, the same holds; small square moves from first row to row two less
  ;; than the row count of the larger square.
  ;;
  ;; Note that it may look strange to the whole width and height of the smaller
  ;; square to get stopping indices.  This works due to loop macro :below
  ;; preposition. If bigger square is 10x10, and smaller square is 3x3, and lisp
  ;; uses zero based array indices, then bigger square has columns 0 through 9,
  ;; and rows 0 through 9.  The final column the smaller square top left corner
  ;; should be at, is 7. Otherwise the smaller square would "stick out" of the
  ;; right side of the bigger square. Same logic applies to rows.
  ;;
  ;; The :below preposition stops *before* 7, so at 6. And then it works.
  (let ((bigger-rows (array-dimension bigger-arr 0))
        (bigger-cols (array-dimension bigger-arr 1))
        (smaller-square-rows 3)
        (smaller-square-cols 3))
    (loop
      :for bigger-starting-row
        :from 0 :below (- bigger-rows (1- smaller-square-rows))
      :nconcing (loop
                  :for bigger-starting-col
                    :from 0 :below (- bigger-cols (1- smaller-square-cols))
                  :collect (populate-window
                            bigger-arr
                            smaller-square-rows smaller-square-cols
                            bigger-starting-row bigger-starting-col)))))

(defun scan-x-mas (arr)
  "Scan for MAS in arr.

   Produce a list of 3x3 windows by sliding it across arr, left to right, and
   top to bottom. For each window, scan diagonals in all directions for the
   string 'MAS'. If 'MAS' is present as a cross (see example in make-windows
   docstring), then the window is to be counted, else not.

   Return number of such 'MAS' crosses found in arr, across all windows."
  (let ((windows (make-windows arr)))
    (loop :for window :in windows
          :for mas-diagonals = (scan-diagonals window "MAS")
          :counting (= mas-diagonals 2))))

(defun test-arr()
  (make-array '(10 10)
              :element-type 'character
              :initial-contents '((#\. #\. #\. #\. #\X #\X #\M #\A #\S #\.)
                                  (#\. #\S #\A #\M #\X #\M #\S #\. #\. #\.)
                                  (#\. #\. #\. #\S #\. #\. #\A #\. #\. #\.)
                                  (#\. #\. #\A #\. #\A #\. #\M #\S #\. #\X)
                                  (#\X #\M #\A #\S #\A #\M #\X #\. #\M #\M)
                                  (#\X #\. #\. #\. #\. #\. #\X #\A #\. #\A)
                                  (#\S #\. #\S #\. #\S #\. #\S #\. #\S #\S)
                                  (#\. #\A #\. #\A #\. #\A #\. #\A #\. #\A)
                                  (#\. #\. #\M #\. #\M #\. #\M #\. #\M #\M)
                                  (#\. #\X #\. #\X #\. #\X #\M #\A #\S #\X))))

(defun test-arr2()
  (make-array '(10 10)
              :element-type 'character
              :initial-contents '((#\. #\M #\. #\S #\. #\. #\. #\. #\. #\.)
                                  (#\. #\. #\A #\. #\. #\M #\S #\M #\S #\.)
                                  (#\. #\M #\. #\S #\. #\M #\A #\A #\. #\.)
                                  (#\. #\. #\A #\. #\A #\S #\M #\S #\M #\.)
                                  (#\. #\M #\. #\S #\. #\M #\. #\. #\. #\.)
                                  (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)
                                  (#\S #\. #\S #\. #\S #\. #\S #\. #\S #\.)
                                  (#\. #\A #\. #\A #\. #\A #\. #\A #\. #\.)
                                  (#\M #\. #\M #\. #\M #\. #\M #\. #\M #\.)
                                  (#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.))))

(defun test-arr3()
  (make-array '(3 3)
              :element-type 'character
              :initial-contents '((#\M #\A #\S)
                                  (#\. #\A #\A)
                                  (#\M #\M #\S))))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (+ (scan-horiz-and-vert input)
     (scan-diagonals input)))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (scan-x-mas input))

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
