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

(let ((goal 10))
  (defun assess (x)
    (let (msg status)
      (cond ((= x goal) (setf msg "just right!"
                              status t))
            ((> x goal) (setf msg "too big"))
            ((< x goal) (setf msg "too small")))
      (format t "[~a] is ~a~%" x msg)
      status)))

(defun looping (&key
                (fn       #'assess)
                (eof      :end)
                (prompt   "Guess")
                (stream   *standard-input*)
                (patience 10))
  (decf patience)
  (unless (< patience 0)
    (if prompt (format t "~a) ~a> " patience prompt))
    (let ((guess (read stream nil eof)))
      (cond ((eq guess eof)     nil)
            ((funcall fn guess) guess)
            (t                 
             (looping 
                :fn fn :eof eof :stream   stream 
                :prompt prompt  :patience patience ))))))

(defun looping-from-string (string)
  (with-input-from-string (stream string)
    (looping :stream stream)))

(defun looping-from-prompt ()
    (looping))

(defun looping-from-file (f)
  (with-open-file (stream f)
    (looping :stream stream)))

(defun test-loop1 ()  (looping))

(defun test-loop2 ()  
  (looping-from-string 
     "1 2 3 11 23 12 22 10 1 4 100 55 33 66 33 111 44 66"))

(defun test-loop3 ()  
  (looping-from-file "lisp101/eg/1"))
