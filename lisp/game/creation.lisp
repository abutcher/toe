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

(defun init-board (l) 
  (let ((board  (make-board))
        (height (length l))
        (width  (length (first l)))
        (i -1) (j -1))
    (labels  
        ((init-cell (char)
           (let ((cell (char->piece char))
                 (pos  (make-point :x i :y j)))
             (incf i)
             (if (gold-p cell)
               (setf (board-gold board) pos))
             (if(player-p cell)
               (setf (board-at        board) pos
                     (player-direction cell) char))
             cell))
         (init-row (row)
           (incf j)
           (setf i 0)
           (if (= width  (length row))
               (mapcar #'init-cell row)
               (error "~a not of length ~a" row width))))
     (let* ((rows      (mapcar #'init-row l))
            (contents  (make-array (list height width) 
                                   :initial-contents rows)))
       (setf (board-contents board)  contents
             (board-width    board)  (1- width)
             (board-height   board)  (1- height))
       (if (board-ok-p board)
           board)))))

(defmethod board-ok-p ((x board))
  (unless (board-gold x)
    (error "missing the gold"))
  (unless (board-at x)
    (error "missing a player"))
  (unless (= (board-height x) (1- (fixed-height *f*) ))
     (error "board height is ~a, not ~a"
            (board-height x)
           (1- (fixed-height *f*))))
  (unless (= (board-width x) (1- (fixed-width *f*) ))
    (error "board width is ~a, not ~a"
           (board-width x)
           (1- (fixed-width *f*))))
  (walk (i j cell endp x)
        (unless (typep cell 'thing)
          (error "~a is not a piece" cell)))
  t)

(deftest read1 ()
  (let ((f "game/eg/1"))
    (init-board (file->linesOfChar f))))

(deftest show1 ()
  (check (string-equal (onstring (read1))
"
~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~
~~     B     B   k ~~
~~   ***     ***   ~~
~~*-*     v     *-*~~
~~  **         **  ~~
~~ g **   d   ** a ~~
~~    BB     BB    ~~
~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~
" ))) 
