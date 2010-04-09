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

;<h1><join>Unit Tests</join></h1>
; <p>(From Peter Seiblel's excellent text: 
; <a href="http://gigamonkeys.com/book">http://gigamonkeys.com/book</a>).
;<p>

;   <h2>Q: What is the same across all languages?</h2> 
;<p>A: all programs in all languages need testing.
; <p><em>I should be able to write a test in any language. 
; --J. Random Hacker</em>
; <p>Every programming has to be debugged:
; <a href="http://unbox.org/wisp/var/timm/09/310/share/img/bug.jpg">
; <img src="http://unbox.org/wisp/var/timm/09/310/share/img/bug.jpg" 
; width=300 align=right></a>
; <ul><em>      Debugging had to be discovered. I can remember the exact 
; instant when I realized that a large part of my life from then on was 
; going to be spent in finding mistakes in my own programs.<br>
;       - Maurice Wilkes 1949 
; </em></ul>
; <br clear=all>
; <p>
; Wilkes is pointing out that the time required to debug a system is 
; surprisingly large- over half the effort of building. 
; According to Brooks (The Mythical Man Month, 1995), the time 
; required to build software divides as follows:
; <ul>
; <li>
;      1/3 in management and planning
;     <li> 1/6 in development
;     <li> 1/4 in unit test (testing all parts in isolation)
;     <li> 1/4 in system testing (testing all parts, in combination) 

; </ul>
; <p>Decades later, the same distribution persists. It looks like this:
; <center>
; <img src="http://unbox.org/wisp/var/timm/09/310/share/img/vdiagram.png">
; </center>
; <P>What this diagram is saying is that if you want to drastically change 
; the economics of software development, do something about
; <em>testability</em>.
; So dealing with a new language,
; always start with the test engine.
;<p>Peter Seiblel's <tt>deftest</tt> test engine is both darn useful 
;and a good beginner's
;test for LISP and LISP design. He took a test engine, found common
;patterns, and wrote macros to implement those common features.
;<p>Macros are a little beyond most LISP begineers so, if the following,
;do not try to understand it all- just test yourself to see how much
; you do understand.

;<h2>Globals</h2>
;<p>Place to store (1) a list of active  tests and (2) a list of all test names.
;<pre>
(defparameter *test-name* nil)  
(defparameter *all-tests* nil)
;</pre>
;<p>Q: what is the difference between <tt>defparameter</tt>,
; <tt>defvar</tt> and <tt>defconstant</tt>? Hint: usually use
;<tt>defparameter</tt>

;<h2>Running Test(s)</h2>
;<p>Every test does the following..
;<pre>
(defmacro run-tests (&body body)
  `(progn 
     (make)            ; check you have the system updated 
     (tests-reset)     ; reset some counters to zero
     ,@body            ; run the code
     (tests-report)))  ; report the state of the counters
;</pre>
;<p>Q: in the above, what does "`" and ",@" do? Hint1: this is
; really tricky. Hint2: <tt>run-tests</tt>
; does not exectute; rather it does some compile-time rewrites.
;<p>Usually, we just run <tt>tests</tt> which
; runs over all all the <tt>*all-tests*</tt> and runs one?
;<pre>
(defun tests ()
  (run-tests
    (dolist (one (reverse *all-tests*))
      (funcall one))))
;</pre>
;<p>Q: Why do we call <tt>reverse</tt> in the above? Hint:
;the name of new tests is pushed onto <tt>*all-tests*</tt> in
; the order of their arrival.
;<p>Q: What does <tt>funcall</tt> do? Hint: pointer to functions.
;<h2>Defining One Test</h2>
;<p>When we define tests, we have to store its name in <tt>*all-tests*</tt>
; then write a special <tt>defun</tt> using that test name.
; Inside this <tt>defun</tt>, before we run a test, we push the name
; to the end of a list of active tests.
; <p>(Note that the explanation actually takes more characters
; than the actual code. Tee hee.).
;<pre>
(defmacro deftest (name parameters &body body)
  `(progn
    (setf *all-tests* (remove-duplicates (cons ',name *all-tests*)))
    (defun ,name ,parameters
      (let ((*test-name* (append *test-name* (list ',name))))
        ,@body))))
;</pre>
;<h2>Support Tools</h2>
;<p>A little complex. Exercise for the reader.
;<pre>
(defmacro check (&body forms)
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect
              `(,n (make-symbol ,(string n))))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect 
             `(unless ,f (setf ,result nil)))
      ,result)))
;</pre>
;<h2>Handling the Counters</h2>
;<p>Object-oriented encapsulation can be implemented in LISP using
;<em>closures</em>. In the following, only these functions can access
; the number of <tt>passes</tt> and <tt>fails</tt>.
;<pre>
(let ((passes 0) (fails 0))  
  (defun tests-reset()
    (setf passes 0)
    (setf fails 0))
  (defun tests-report ()
    (if (zerop (+ passes fails))
        (format t "no tests~%")
        (format t "~%PASSES: ~a (~a %)~%FAILS : ~a~%"
            passes (* 100 (/ passes (+ passes fails)))
            fails))
    (>= passes fails))
 (defun report-result (result form)
    (if result
        (and (incf passes) 
             (format t "% ~a~%" *test-name*))
        (and (incf fails) 
             (format t "~%fail ... ~a: ~a~%"  *test-name* form)))
    result)
)
;</pre>
;<p>Q: what do these three functions do?
;<h2>Examples</h2>
;<p>For examples of <tt>deftest</tt> in action, see
;<a href="http://menzies.us/ai/?lib/lisp101/random.lisp">random.lisp</a> and
;<a href="http://menzies.us/ai/?lib/lisp101/hash.lisp">hash.lisp</a>.
;The <a href="http://menzies.us/ai/?lib/lisp101/hash.lisp">hash.lisp</a>
; example shows one interesting feature of <tt>deftest</tt>: it can be
; used to document the expected behaviour.
