(define gen-list
  (lambda (start stop)
    (if (> start stop)
        '()
        (cons start (gen-list (+ 1 start) stop)))))

(define pair-sum?
  (lambda (lst val)
    (cond
      ((null? (cdr lst)) #f)
      ((= (+ (car lst) (car (cdr lst))) val) #t)
      (else (pair-sum? (cdr lst) val)))))

(define gen-lazy-list
  (lambda (start stop)
    (if (> start stop)
        #f
        (cons start
            (lambda () (gen-lazy-list (+ start 1) stop))))))

(define pair-sum-lazy?
  (lambda (lazy-list val)
    (if (equal? (cdr lazy-list) #f)
        #f
        (if (= (+ (car lazy-list) (car (cdr lazy-list))) val)
            #t
            (pair-sum-lazy? (cdr lazy-list) val)))))