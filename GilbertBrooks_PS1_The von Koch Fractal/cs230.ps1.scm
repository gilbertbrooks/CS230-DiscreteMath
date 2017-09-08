;;; Go to Language, Choose Language, Other Languages, Swindle, Full Swindle
;;; This may have to be done in cs230-graphics.scm as well
;;; cs230.ps1.scm


(require racket/base) ;;This allows the type system to work.
(require (file "cs230-graphics.scm")) ;;Pull in the definitions for the drawing window and stuff. Assumes the file is in the same directory. 

;; Here are the procedures you will modify in the problem set
(define side
  (lambda ((length <real>) (heading <real>) (level <integer>))
    (if (zero? level)
        
        (drawto heading length)

        (let ((len/3 (/ length 3))
              (lvl-1 (- level 1)))
          (side len/3 heading lvl-1)
          (side len/3 (- heading PI/3) lvl-1)
          (side len/3 (+ heading PI/3) lvl-1)
          (side len/3 heading lvl-1)))))

(define snowflake:0
  (lambda ((length <real>) (level <integer>))
    (side length 0.0 level)
    (side length (* 2 PI/3) level) ;; (* 2 PI/3) = 60 degrees
    (side length (- (* 2 PI/3)) level)))

;;Problem 1
(define flip-side
  (lambda ((length <real>) (heading <real>) (level <integer>))
    (if (zero? level)
        (drawto heading length)
        (let ((len/root2 (/ length (* 2 (sqrt 2))))
              (lvl-1 ( - level 1)))
          (flip-side len/root2 (- heading PI/4) lvl-1)         
          (flip-side (* 2 len/root2) (+ heading PI/4) lvl-1)
          (flip-side len/root2 (- heading PI/4) lvl-1)))))

(define square-snowflake:1
  (lambda ((length <real>) (level <integer>))
    (flip-side length 0.0 level)
    (flip-side length PI/2 level)
    (flip-side length (- PI) level)
    (flip-side length (- PI/2) level)))


;;Problem 2
;;(square-snowflake:2 150 3 flip-side)
(define snowflake:2
  (lambda ((length <real>) (level <integer>) (funcName <function>))
    (funcName length 0.0 level)
    (funcName length (* 2 PI/3) level)
    (funcName length (- (* 2 PI/3)) level)))

(define square-snowflake:2
  (lambda ((length <real>) (level <integer>) (aFunc <function>) (inverter <integer>))
    (aFunc length 0.0 level)
    (aFunc length PI/2 level) 
    (aFunc length (- PI) level)
    (aFunc length (- PI/2) level)))



;;Problem 3
(define snowflake-inv
  (lambda ((length <real>) (level <integer>) (funcName <function>) (inverter <function>))
    (funcName length 0.0 level inverter)
    (funcName length (* 2 PI/3) level inverter)  
    (funcName length (- (* 2 PI/3)) level inverter)))

(define side-inv
  (lambda ((length <real>) (heading <real>) (level <integer>) (inverter <function>))

    (if (zero? level)
        
        (drawto heading length)
        
        (let ((len/3 (/ length 3))
              (lvl-1 (- level 1)))
          
          (side-inv len/3 heading lvl-1 inverter)
          (side-inv len/3 (- heading (* PI/3 (inverter level))) lvl-1 inverter)
          (side-inv len/3 (+ heading (* PI/3 (inverter level))) lvl-1 inverter)
          (side-inv len/3 heading lvl-1 inverter)))))



;;Problem 4 -- compute the total length of all line segments that would be drawn

(define side-length
  (lambda ((length <real>) (heading <real>) (level <integer>) (inverter <function>))

    (if (zero? level) length
       
        (let ((len/3 (/ length 3))
              (lvl-1 (- level 1)))
 
         (+ (side-length len/3 heading lvl-1 inverter)
         (side-length len/3 (- heading (* PI/3 (inverter level))) lvl-1 inverter)
         (side-length len/3 (+ heading (* PI/3 (inverter level))) lvl-1 inverter)
         (side-length len/3 heading lvl-1 inverter))))))


(define snowflake-length
  (lambda ((length <real>) (level <integer>) (funcName <function>) (inverter <function>))
    
    (+ (funcName length 0.0 level inverter)
    (funcName length (* 2 PI/3) level inverter)  
    (funcName length (- (* 2 PI/3)) level inverter))))
        
;; Make the graphics window visible, and put the pen somewhere useful
(init-graphics 640 480)
(clear)
(moveto 100 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
