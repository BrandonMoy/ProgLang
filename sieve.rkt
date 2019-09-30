(define seq
 (lambda (start stop)
    (if (> start stop)
        #f
        (cons start
            (lambda () (seq (+ start 1) stop))))))

(define inf-seq
 (lambda (start)
   (cons start (lambda() (inf-seq (+ start 1))))))

(define first-n
  (lambda (lazy-list n)
    (if (or (= n 0) (not lazy-list))
        '()
        (cons (car lazy-list) (first-n ((cdr lazy-list)) (- n 1))))))
;;(first-n (inf-seq 1) 5) ; test case
;(first-n (seq 1 2) 5)

(define n-th
  (lambda (lazy-list n)
    (if (= n 1)
        (car lazy-list)
        (n-th ((cdr lazy-list)) (- n 1)))))
;(n-th (inf-seq 1) 5)

(define filter-multiples ; should return a lazy list
  (lambda (lazy-list n)
    (if (not lazy-list)
        '()
        (if  (= (remainder (car lazy-list) n) 0);(integer? (/ ((car lazy-list) n))) ;is the first item divisible by n
             (filter-multiples ((cdr lazy-list)) n)
             (cons (car lazy-list) (lambda () (filter-multiples ((cdr lazy-list)) n)))))))
;(filter-multiples (seq 2 6) 2)  ;---> (3 5)
;((cdr (filter-multiples (seq 2 6) 2)))
;(filter-multiples (seq 3 8) 3) ;---> (4 5 7 8)

(define sieve
  (lambda (lazy-list)  
    (cons (car lazy-list) (lambda () (sieve (filter-multiples ((cdr lazy-list)) (car lazy-list)))))))

(define primes
  (lambda () 
    (sieve (inf-seq 2))))

;(first-n (primes) 10) ; (2 3 5 7 11 13 17 19 23 29)
;(n-th (primes) 20) ; 71

(define count-helper
  (lambda (lst n)
    (if (< (car lst) n)
        (+ 1 (count-helper (cdr lst) n))
        0)))

(define count-smaller-primes
  (lambda (n)
    (count-helper (first-n (primes) n) n)))

(define twin-helper
  (lambda (lazy-list)
    (if (= (+ (car lazy-list) 2) (car ((cdr lazy-list)))) ; if the 1st # = the 2nd # + 2
        (cons (cons (car lazy-list) (car ((cdr lazy-list)))) (lambda () (twin-helper ((cdr lazy-list))))) ; cons the first number iwth teh second number
        (twin-helper ((cdr lazy-list))))))

(define twin-primes
  (lambda ()
    (twin-helper (primes)))) 


(first-n (twin-primes) 5)
