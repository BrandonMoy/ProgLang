(define keep-first-n
  (lambda (n l)
    (if (< n 1)
    '()
    (if (null? l)
        '()
        (cons (car l) (keep-first-n (- n 1) (cdr l)))))))

(define sum
  (lambda l
    (if (null? l)
        0
        (+ (car l) (sum (cdr l))))))