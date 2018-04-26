(define (pay-methods coins n)
  (if (null? coins)
    '()
    (let ((c (car coins))
          (cs (cdr coins)))
      (cond
        ((<= n 0)
         '(()))
        ((>= n c)
         (append
           (pay-methods cs n) 
           (map (lambda (x) (cons c x)) (pay-methods coins (- n c)))))
        (else 
          (pay-methods (cdr coins) n))))))

(print (length (pay-methods '(1 2 5 10 20 50 100 200) 200)))
