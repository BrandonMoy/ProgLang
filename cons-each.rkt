(define cons-each
  (lambda (x lst)
    (if (null? lst)
        '()
        (cons (cons x (car lst)) (cons-each x (cdr lst))))))

(cons-each 1 '(() (2) (3)))