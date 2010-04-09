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

;<h1><join>Debugging Tricks</join><h1>
;<h2>Print name of variable, and its bindings</h2><pre>
(defmacro o (&rest l) 
  (let ((last (gensym)))
    `(let (,last)
       ,@(mapcar #'(lambda(x) `(setf ,last (oprim ,x))) l)
       (terpri)
       ,last)))

(defmacro oprim (x)  
  `(progn (format t "[~a]=[~a] " (quote ,x) ,x) ,x))
;</pre>
;<p>E.g.<pre>
(deftest test-o ()
  (let* ((a 1)
         (b 2)
         (c (+ a b)))
    (o a b c))) ; prints [a]=[1] [b]=[2] [c]=[3]
;</pre>
;<h2>Profile code</h2><pre>
(defmacro watch (code)
  `(progn
    (sb-profile:unprofile)
    (sb-profile:reset)
    (sb-profile:profile ,@(my-funs))
    (eval (progn ,code t))
    (sb-profile:report)
    (sb-profile:unprofile)))

(defun my-funs ()
  (let ((out '()))
    (do-symbols  (s)
      (if (and (fboundp s)
	       (find-symbol  (format nil "~a" s) *package*)
	       (not (member s *lisp-funs*)))
	  (push s out)))
    out))
;</pre>
;<h2>Timing of executions</h2><p>Returns the mean runtime
;of each <t>n</t> repeats.<pre>
(defmacro time-it (n &body body) 
  (let ((n1 (gensym))
        (i  (gensym))
        (t1 (gensym)))
    `(let ((,n1 ,n)
           (,t1 (get-internal-run-time)))
       (dotimes (,i ,n1) ,@body)
       (float (/ (- (get-internal-run-time) ,t1)
                 (* ,n1 internal-time-units-per-second))))))
;</pre>
