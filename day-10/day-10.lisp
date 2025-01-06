;;;; day-10.lisp

(in-package #:day-10)

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
  "Return a list of (row col) tuples where height is 0 in topo-map.

   A trailhead is, according to the puzzle:

   'A trailhead is any position that starts one or more hiking trails - here, these positions will
   always have height 0.'"
  (destructuring-bind
      (rows cols)
      (array-dimensions topo-map)
    (loop :for row :from 0 :below rows
          :nconcing (loop :for col :from 0 :below cols
                          :when (zerop (aref topo-map row col))
                            :collect `(,row ,col)))))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
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
