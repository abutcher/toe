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

;<h1><join>Random Numbers in LISP</join></h1>
;<h2>Pseudo-random Number Generation</h2>
;<p>The bald guy says <em>when debugging code containing random
;selections, it is useful to 
;recreate a 
;prior run that lead to an error</em>. Hence, most languages 
;offer a pseudo-random number generator which generates a sequence of 
;random numbers from some 
;seed. Exactly the same sequence of "random" numbers can be recreated by 
;resetting the seed and re-running the random sequence. 
;<p>Sadly, I can't make LISP's built in random number generator work that 
;way. Yes, I 've tried reseting *random-state* and that did not work. 
;Also, I began to worry 
;that what ever I did would not be portable to other LISPs.
;<p>So the following  implements a
;simple  pseudo-random number generator.
;<pre>
(defparameter *seed0* 10013)
(defparameter *seed*  *seed0*)

(defun reset-seed () (setf *seed* *seed0*))
;</pre>
;<p>This is the Park-Miller multiplicative congruential randomizer
;  (CACM, October 88, Page 1195). Creates pseudo random floating
;  point numbers in the range 0.0 &lt; x &le; 1.0.
;<pre>  
(defun park-miller-randomizer ()
   (let ((multiplier 
	 16807.0d0);16807 is (expt 7 5)
	(modulus 
	 2147483647.0d0)) ;2147483647 is (- (expt 2 31) 1)
    (let ((temp (* multiplier *seed*)))
      (setf *seed* (mod temp modulus))
      (/ *seed* modulus))))
;</pre>
;<h2>My-random</h2> <p> Returns a pseudo random floating-point number
;  in range 0.0 &le; number &lt; n.
;Nogte that we  subtract the randomly generated number from 1.0
; before scaling so that we end up in the range
; 0.0 &le; x &lt; 1.0, not 0.0 &lt; x &le; 1.0.<pre>
 (defun my-random (n)
  (let ((random-number (park-miller-randomizer)))
    (* n (- 1.0d0 random-number))))
;</pre>
;<h2>My-random-int</h2><p>  Returns a pseudo random integer 
;  in range 0 &le; number &lt; n-1.<pre>
(defun my-random-int (n)
  (let ((random-number (/ (my-random 1000.0) 1000)))
    (floor (* n random-number))))
;</pre><H2>Demo</h2> If we've done the above right, then if we
;generate
; two sequences of random numbers, then if we reset the seed between
; sequence one and sequence two, then the two sequences should be the
; same.
;<pre>
(deftest test-random ()
  (check
    (equalp (random-demo) (random-demo))))

(defun random-demo (&optional (resetp t))
  (let (counts out)
    (labels 
	((sorter (x y) (< (car x) (car y)))
         (zap    ()    (setf out nil)
                       (if resetp (reset-seed)) 
                       (setf counts (make-hash-table)))
	 (inc    (n)   (setf (gethash n counts) 
			     (1+  (gethash n counts 0)))) 
	 (cache  (k v) (push (list k v) out)))
      (zap)
      (dotimes (i 10000)              ; 10000 times do
            (inc  (my-random-int 5))) ; generate a num 0..4
      (maphash #'cache counts)        ; hash key/buckets ==> lists 
      (sort out #'sorter))))          ; sort and print  list
;</pre>
