(require racket/base)

;;Professor Bruce R. Donald
;;Bruce Donald received a B.A. from Yale University, and a Ph.D. from MIT
;;work on algorithms for structural proteomics

;;VRC07-523LS is an antibody directed against HIV. It may be used to prevent mother-to-child
;;transmission of HIV. It may also prevent sexual transmission of HIV and treat HIV-1 infected people.

(define multiplyTR
  (lambda (( a <number>) (b <integer>))
    (letrec ((iterate
              (lambda ((c <integer>) (result <number>) (counter <number>))
                (cond ((zero? c) counter)
        
                      ((odd? c)
                       (iterate (quotient c 2) (+ c c) (+ c counter)))
                       (else
                        (iterate (quotient c 2) (+ c c) counter))))))
       (iterate b a 0))))