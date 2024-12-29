;;;; day-5.lisp

(in-package #:day-5)

(defun read-input (path)
  "Read and return puzzle input."
  (uiop:read-file-lines path))

(defun parse-input (lines)
  "Parse input lines into solution-friendly format.

   Input is in two sections, 1) page ordering rules, and 2) page numbers per
   update. Sections are separated by a single blank line.

   Page rules
   ----------
   Lines are strings of two integers separated by a bar; X|Y. Read these into a
   list of pairs ((X1 Y1) (X2 Y2) ... (XN YN)).

   Updates
   -------
   Lines are strings of page integers per update; X, Y, ..., Z. Read these into
   a list of integers ((X Y Z) ... (A B C D))

   Return a list of the parsed input sections (page-rules updates)."
  (loop :for line :in lines
        :for delimiter = (if (find #\| line :test #'equal) #\| #\,)
        :for parts = (uiop:split-string line :separator `(,delimiter))
        :for int-parts = (mapcar
                          (lambda (s) (parse-integer s :junk-allowed t))
                          parts)
        :when (> (length parts) 0)
          :if (char= delimiter #\|)
            :collect int-parts :into page-rules
          :else
            :collect int-parts :into updates
          :end
        :finally (return (list page-rules updates))))

(defun has-elem (elem set &key (test #'equal))
  "Utility to test if elem is present in set."
  (not (null (intersection (list elem) set :test test))))

(defun ok-update (update page-rules)
  "Return the update page sequence if it follows the page order rules given in
   page-rules hash, else nil.

   page-rules hash has page as key and value is a list of all pages that must be
   printed after the key page. Key and elements in value list are integers."
  (loop :for remaining-pages :on update
        :for cur-page = (first remaining-pages)
        :for cur-page-rules = (gethash cur-page page-rules)
        :for next-pages = (rest remaining-pages)
        :if (loop :for next-page :in next-pages
                  :always (has-elem next-page cur-page-rules))
          :collect cur-page
        :else
          :return nil))

(defun make-rules-hash (page-rules)
  "Convert a list of page rules into a hash where page is key and value is a list
   of all pages that must be after the key page.

   Key and all elements in value list are integers."
  (let ((page-rules-hash (make-hash-table)))
    (loop :for (page later-page) :in page-rules
          :for later-pages = (nconc
                              (gethash page page-rules-hash)
                              (list later-page))
          :do (setf (gethash page page-rules-hash) later-pages))
    page-rules-hash))

(defun strictly-less-p-not-used (sought-page page-tree &optional (to-check nil))
  "Perform a recursive tree-like search for sought-page in page-tree.

   page-tree structure is a hash with integers as keys, and lists of integers as
   values. If sought-page is not found in value list of a page-tree key,
   recursively search for the sought-page by looking up each element in value
   list in the page-tree.

   Return true if sought-page is found, else nil."

        ;; No more elements to check => nil
  (cond ((null to-check) nil) 

        ;; Element found => t
        ((member sought-page to-check :test #'=) t)

        ;; If the intersection of to-check and expanded to-check is nil, then
        ;; abort too.
        ((null (set-difference to-check (remove-duplicates
                                         (copy-list
                                          (alexandria:flatten
                                           (loop :for page :in to-check
                                                 :collecting (gethash page page-tree)))))))
         nil)
 
        ;; Expand all key-to-value mappings and try again.
        (t (strictly-less-p sought-page page-tree
                            (remove-duplicates
                             (copy-list
                               (alexandria:flatten
                                (loop :for page :in to-check
                                      :collecting (gethash page page-tree)))))))))

(defun strictly-less-p (page-a page-b page-rules)
  "Check if page-b is a member of page-a values in page-rules.

   page-tree structure is a hash with integers as keys, and lists of integers as
   values. If page-b is not found in value list of page-rules key page-a,
   return nil. Else return rest of list from search hit.

   This was not my first intuition. My first was the sort predicate seen in
   strictly-less-p-not-used function."
  (member page-b (gethash page-a page-rules)))

(defun make-page-sort-predicate (page-rules)
  "Generate a predicate function for the CL sort function based on page sorting
   rules.

   From CLHS on sort: `Predicate should return true if and only if the first
   argument is strictly less than the second (in some appropriate sense). If the
   first argument is greater than or equal to the second (in the appropriate
   sense), then the predicate should return false.`

   Given knowledge about the sort rules, return a function that takes arguments
   page-a and page-b, return true or false according to page sort rules and CL
   sort predicate function."

  ;; For strictly-less-p-not-used function.
  ;; (lambda (page-a page-b) (strictly-less-p
  ;;                          page-b
  ;;                          page-rules
  ;;                          (gethash page-a page-rules)))

  (lambda (page-a page-b) (strictly-less-p
                           page-a
                           page-b
                           page-rules)))

(defun solve-part-1 (input)
  "Solve part 1 of puzzle."
  (let* ((page-rules-list (first input))
         (page-rules (make-rules-hash page-rules-list))
         (updates (second input)))
    (loop :for update :in updates
          :for in-order = (ok-update update page-rules)
          :if in-order
            ;; If list is odd in length, which it is per problem statement or
            ;; implications, then index of middle page is length of pages minus
            ;; one, then divided by two. This works because Common Lisp lists
            ;; are zero-indexed. If list is (1 2 3 4 5), length is 5, length - 1
            ;; is 4, and 4/2 is 2, which is the index of the middle value in the
            ;; list.
            :sum (nth (/ (1- (length in-order)) 2) in-order))))

(defun solve-part-2 (input)
  "Solve part 2 of puzzle."
  (let* ((page-rules-list (first input))
         (page-rules (make-rules-hash page-rules-list))
         (updates (second input))
         (sortp (make-page-sort-predicate page-rules)))
    (loop :for update :in updates
          :unless (ok-update update page-rules)
            :sum (nth (/ (1- (length update)) 2)
                      (sort (copy-seq update) sortp)))))

(defun main (&optional (mode :full))
  "AoC 2024 day 5 solution.
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
