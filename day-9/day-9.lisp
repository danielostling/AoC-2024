;;;; day-9.lisp

(in-package #:day-9)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.

   Puzzle input is a block layout format for a disk. Numbers alternate between file block count and
   free block count.

   '... a disk map like 12345 would represent a one-block file, two blocks of free space, a
   three-block file, four blocks of free space, and then a five-block file. A disk map like 90909
   would represent three nine-block files in a row (with no free space between them).'

   'Each file on disk also has an ID number based on the order of the files as they appear before
   they are rearranged, starting with ID 0. So, the disk map 12345 has three files: a one-block file
   with ID 0, a three-block file with ID 1, and a five-block file with ID 2. Using one character for
   each block where digits are the file ID and . is free space, the disk map 12345 represents these
   individual blocks:

   0..111....22222

   The first example above, 2333133121414131402, represents these individual blocks:

   00...111...2...333.44.5555.6666.777.888899'

   Read input, which is a string of integers, and return a vector where positive numbers are file
   IDs and -1 is free space. First element in vector is leftmost number in input."
  (flet ((pad-input (input)
           "If input string has odd length, add a 0 to pad zero free space."
           (let* ((blocks-definition (coerce (first input) 'list))
                  (size (length blocks-definition)))
             (when (oddp size)
               (setf blocks-definition (append blocks-definition '(#\0)))
               (incf size))
             (list (coerce blocks-definition 'vector) size))))
    (let* ((blockmap (make-array 0 :element-type 'integer :fill-pointer t :adjustable t))
           (input-blocks-and-size (pad-input lines))
           (input-blocks (first input-blocks-and-size))
           (input-size (second input-blocks-and-size)))
      (loop :for idx :from 0 :to (- input-size 2) :by 2
            :for file-id = 0 :then (1+ file-id)
            :for file-blocks = (digit-char-p (aref input-blocks idx))
            :for free-blocks = (digit-char-p (aref input-blocks (1+ idx)))
            :do (progn
                  (loop :repeat file-blocks
                        :do (vector-push-extend file-id blockmap))
                  (loop :repeat free-blocks
                        :do (vector-push-extend -1 blockmap))))
    blockmap)))

(defun print-blockmap (blockmap)
  (loop :for file-id :across blockmap
        :do (format t "~a" (if (minusp file-id) "." file-id)))
  (format t "~%"))

(defun defrag (blockmap)
  "Perform a defragmentation on blockmap and return its result.

   Move one block at a time in blockmap from right-to-left to first available
   left-to-right free slot. Return defragmented blockmap."
  (flet ((get-next-free-idx (idx blockmap)
           "Return next free slot in blockmap.

            Start at idx and move 'right' in blockmap. If no free slots remain,
            return nil."
           (let ((size (length blockmap)))
             (loop :for free-idx :from idx :below size
                   :when (minusp (aref blockmap free-idx))
                     :return free-idx)))
         (get-next-file-id-idx (idx blockmap)
           "Return next idx of a file-id in blockmap.

            Start at idx and move 'left' in blockmap until an idx for a file-id
            is found. If no such idx remains, return nil."
           (loop
             :for file-id-idx :from idx :downto 0
             :for file-id = (aref blockmap file-id-idx)
             :when (plusp file-id)
               :return file-id-idx)
           ))
    (let* ((blockmap-size (1- (length blockmap)))
           (left-idx (get-next-free-idx 0 blockmap)))
    (loop
      :for right-idx = blockmap-size :then (get-next-file-id-idx right-idx blockmap)
      :when (>= left-idx right-idx)
        :return blockmap
      :do
         (setf (aref blockmap left-idx) (aref blockmap right-idx)
               (aref blockmap right-idx) -1
               left-idx (get-next-free-idx left-idx blockmap))))))

(defun checksum (blockmap)
  "Calculate blockmap checksum.

   Sum file ID multiplied by block index, left to right. Free space is ignored."
  (let ((size (length blockmap)))
    (loop :for idx :from 0 :below size
          :for file-id = (aref blockmap idx)
          :when (not (minusp file-id))
            :sum (* idx file-id))))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (checksum (defrag input)))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  )

(defun main (&optional (mode :full))
  "AoC 2024 day 9 solution.
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
