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
    (if (equal? ((cdr lazy-list)) #f)
        #f ; if the list is over return false
        (if (= (+ (car lazy-list) (car ((cdr lazy-list)) )) val)
            #t ; if the first two items sum to val return true
            (pair-sum-lazy? ((cdr lazy-list)) val))))) ;otherwise try again with next number

;; takes traditional list as parameter, returns lazy version
(define make-lazy
  (lambda (lst)
    (if (null? (cdr lst))
        (cons (car lst) (lambda () #f)); if this is the last item in the list return it
        (cons (car lst)
              (lambda () (make-lazy (cdr lst)))))))

(define any-sum-helper
  (lambda (lst val)
    (cond
      ((= (car lst) val) #t)
      ((equal? ((cdr lst)) #f) #f)
     (else (any-sum-helper ((cdr lst)) val)))))
 
(define any-sum-lazy?
  (lambda (lst val)
    (if (equal? ((cdr((cdr lst)))) #f) ; test to see if you're at the end of the list
        (if (= (+ (car ((cdr lst))) (car lst)) val) ; test last two numbers w/out helper function
            #t
            #f)
        (if (any-sum-helper ((cdr lst)) (- val (car lst))) ; use helper fn to check through rest of list
            #t
            (if (equal? ((cdr lst)) #f)
                #f
                (any-sum-lazy? ((cdr lst)) val))))))
