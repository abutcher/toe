;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This file is part of AIslash.
;
; AIslash is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; AIslash is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with AIslash.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;<h1><join>Hash Functions</join></h1>
;<p>You can run over lists with dolist:
;<pre>
;(dolist (i 1000) 
;    (print i)
;</pre>
;Why can't you run over hash tables the same way?
;<h2>Macro Magic: dohash</h2>
;<p> E.g. print every key value in a hash table
;<pre>
;(dohash (k v h)
;   (format t "~a = ~a~%" k v))
;</pre>
;<p>Code:
;<pre>
(defmacro dohash ((key value hash &optional end) &body body)
  `(progn (maphash #'(lambda (,key ,value) ,@body) ,hash)
         ,end))
;</pre><p>Short cut: just access the kyes or values:<pre>
(defmacro dovalues ((value hash &optional end) &body body)
  (let ((key (gensym)))
    `(progn (maphash #'(lambda (,key ,value) ,@body) ,hash)
            ,end)))

(defmacro dokeys ((key hash &optional end) &body body)
  (let ((value (gensym)))
    `(progn (maphash #'(lambda (,key ,value) ,@body) ,hash)
            ,end)))
;</pre><p>Q: why <tt>gensym</tt>? Hint: name collision within <tt>@body</tt>.
;<h2>Pretty print a Hash Table</h2>
;<p> <tt>(dohash (k v h) (format t "~a = ~a~%" k v))</tt> is all very well
; but what does a real pretty print for a hash table look like?
;<pre>
(defun showh (h &key
              (indent 0) (stream t) (before "") (after "")
              (if-empty "empty")
              (show #'(lambda (x)
                        (format stream "~a~a = ~a~%"
                           (nchars indent) (first x) (rest x))))
              (lt #'lt))
  (if (zerop (hash-table-count h))
      (format stream "~a~a~a" before if-empty after)
      (let (l)
        (format stream "~a" before) 
        (maphash #'(lambda (k v) (push (cons k v) l)) h)
        (mapc show 
              (sort l #'(lambda (a b)
                          (funcall lt (car a) (car b)))))
        (format stream "~a" after)
        h)))
;</pre>
;<P>If that is too complex, then lets look at an example:
;<pre>
(deftest test-showh ()
  (let ((h (make-hash-table)))
    (dolist (one '(apple pear banana))
      (setf (gethash (length (string one)) h) one))
   (check
     (samep (with-output-to-string (s) (showh h  :stream s))
            "4 = PEAR
             5 = APPLE
             6 = BANANA"))))
;</pre>
;<p>(By the same, <tt>samep</tt> is a very relaxed string comparison
; operator. It ignores all white space- which means that a one space
; typo in the test case does not wreck the test.
;<p>And here's an example for <tt>do-values</tt>
;<pre>
(deftest test-dovalues ()
  (let (all
        (h (make-hash-table)))
    (dolist (one '(apple pear banana))
      (setf (gethash (length (string one)) h) one))
    (dovalues (value h)
      (push value all))
    (check
      (equal '(banana pear apple) all))))
;</pre>
;<h2>Other Misc Stuff</h2>
;<pre>
(defun keys2sorted-alist (h ranker)
  (labels ((car-string-lessp (x y) 
             (string-lessp (rest x) (rest y))))
    (let (all (n -1))
      (dohash (key value h)
        (push (cons key
                    (format nil (if (numberp key) ranker "~a") key))
              all))
      (mapcar #'(lambda (one) `(,(first one) . ,(incf n)))
              (sort all #'car-string-lessp)))))

(defmethod print-object ((object hash-table) stream) 
  (format stream "#<HT ~a>" (hash-table-count object)))   
;</pre>