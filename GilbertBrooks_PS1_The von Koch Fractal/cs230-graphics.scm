; swindle does not need to be separately downloaded
; swindle comes with the racket package(provide drawto)

(module newcs230 swindle
  (require graphics/graphics)
  (require racket/class)
  (require racket/math) ;Required to use pi
  (open-graphics);Required initialization of viewport library
  (provide drawto PI PI/2 PI/3 PI/4 PI/6 init-graphics clear moveto)
  (defclass <viewport> ()
    (port :initvalue #f :accessor port) ; <obj>
    (color :initvalue "black" :accessor color) ; <obj>
    (xpos :initvalue 0. :accessor xpos) ; <real>
    (ypos :initvalue 0. :accessor ypos)) ; <real>
  
  (define *graphics* (make <viewport>))
  
  (define PI (* 4.0 (atan 1.0)))
  (define PI/2 (/ PI 2))
  (define PI/3 (/ PI 3))
  (define PI/4 (/ PI 4))
  (define PI/6 (/ PI 6))
  
  (define moveto
    (lambda (x y)
      (set-xpos! *graphics* x)
      (set-ypos! *graphics* y)
      ))
  
  (define rmoveto
    (lambda (dx dy)
      (set-xpos! *graphics*
                 (+ (xpos *graphics*) dx))
      (set-ypos! *graphics*
                 (+ (ypos *graphics*) dy))
      ))
  
  (define lineto
    (lambda (x y)
      (let ((tx (round x))
            (ty (round y)))
        ((draw-line (port *graphics*))
         (make-posn (round (xpos *graphics*))
                    (round (ypos *graphics*)))
         (make-posn tx ty)
         (color *graphics*))
        (set-xpos! *graphics* x)
        (set-ypos! *graphics* y)
        )))
  
  (define rlineto
    (lambda (dx  dy)
      (let* ((new-x (+ (xpos *graphics*) dx))
             (new-y (+ (ypos *graphics*) dy))
             (tx (round new-x))
             (ty (round new-y)))
        ((draw-line (port *graphics*))
         (make-posn (round (xpos *graphics*)) 
                    (round (ypos *graphics*)))
         (make-posn tx ty)
         (color *graphics*))
        (set-xpos! *graphics* new-x)
        (set-ypos! *graphics* new-y)
        )))
  
  
  (define drawto
    (lambda (angle length)
      (let ((dx (* (cos angle) length))
            (dy (* (sin angle) length)))
        (rlineto dx dy)
        )))
  (define skipto
    (lambda (angle length)
      (let ((dx (* (cos angle) length))
            (dy (* (sin angle) length)))
        (rmoveto dx dy)
        )))
  
  (define clear
    (lambda ()
      ((clear-viewport (port *graphics*)))
      ))
  
  (define penrgb
    (lambda (r g b)
      (let ((true-red (/ r 100.0))
            (true-grn (/ g 100.0))
            (true-blu (/ b 100.0)))
        (set-color! *graphics* (make-rgb true-red true-grn true-blu))
        )))
  
  (define pencolor
    (lambda (clr)
      (set-color! *graphics* clr)  ;; Should probably check this...
      ))
  
  (define center
    (lambda ()
      (let-values (((width height)
                    (send (viewport-dc (port *graphics*)) get-size)))
        (set-xpos! *graphics* (/ width 2))
        (set-ypos! *graphics* (/ height 2))
        )))
  
  (define get-viewport-dim
    (lambda (port dim)
      
      (let-values (((width height)
                    (send (viewport-dc port) get-size)))
        (case dim
          ((:w :wid :width) width)
          ((:h :hgt :height) height)
          (else
           (error 'get-viewport-dim "unknown dimension" dim)
           )))))
  
  (define width
    (lambda ()
      (get-viewport-dim (port *graphics*) :width)
      ))
  
  (define height
    (lambda ()
      (get-viewport-dim (port *graphics*) :height)
      ))
  
  (define init-graphics
    (lambda (width height)
      (if (port *graphics*)
          (close-viewport (port *graphics*)))
      (set-port! *graphics* (open-viewport "Graphics" width height))
      ))
  
  ;; Convert degrees to radians
  (define deg->rad
    (lambda (deg)
      (* deg (/ pi 180.0))))
  
  ;; Convert radians to degrees
  (define rad->deg
    (lambda (rad)
      (* rad (/ 180.0 pi)))))




;; Here there be dragons