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


(defstruct point 
  "why isn't this a listp build in?"
  (x 0) (y 0))


(defstruct board
  (contents (make-grid))
  g 
  h 
  width
  height
  gold
  at)

(defstruct thing)

(defstruct (player (:include thing))
  direction
  holding)

(defstruct (piece    (:include thing))  (cost 0))
(defstruct (blank    (:include piece)))
(defstruct (blocks   (:include piece  (cost (fixed-inf *f*))))  fixedp)
(defstruct (tool     (:include piece)))

(defstruct (axe      (:include tool)))
(defstruct (key      (:include tool)))
(defstruct (dynamite (:include tool)))
(defstruct (gold     (:include tool)))
 
(defstruct (bush     (:include blocks)))
(defstruct (door     (:include blocks)))
(defstruct (wall     (:include blocks)))
(defstruct (water    (:include blocks (fixedp t))))
