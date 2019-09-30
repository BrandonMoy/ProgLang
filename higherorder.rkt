(define curry3
  (lambda (f)
    (lambda (x)
      (lambda (y)
        (lambda (z)
          (f (f x y) z))))))


(define uncurry3
  (lambda (f)
     (lambda (x y z)
       (((f x)y)z))))





(define my-filter
  (lambda(f lst)
    (if (null? lst)
      '()
      (if (f (car lst))
          (cons (car lst) (my-filter f (cdr lst)))
          (my-filter f (cdr lst))))))

(define (member? x list)
     (if (null? list) #f                               
         (if (equal? x (car list)) #t                   
              (member? x (cdr list)))))

(define =?
  (lambda(x)
    (lambda(y)
      (= x y))))

(define intersection
  (lambda (lst1 lst2)
    (if (null? lst1)
        '()
         (if (null? (my-filter (=? (car lst1)) lst2))
             (intersection (cdr lst1) lst2)
             (cons (car (my-filter (=? (car lst1)) lst2)) (intersection (cdr lst1) lst2))))))

(define union
  (lambda (lst1 lst2)
    (if (null? lst2)
        lst1
        (if (null? (my-filter (=? (car lst2)) lst1))
            (union (cons (car lst2) lst1) (cdr lst2))
            (union lst1 (cdr lst2))))))
            

(define exists
  (lambda (f lst)
    (if (null? (my-filter f lst))
        #f
        #t)))

;i wrote this without my partner
(define flatmap
  (lambda (f lst)
    (flatten (map f lst))))


;i wrote this without my partner
(define flatten
  (lambda (lst)
    (if (null? (cdr lst)) ; if the cdr is null; will be triggered by last item in any sub list and last item in the entire list ; base case 
        (car lst)
        (append (car lst) (flatten (cdr lst))))))


; for curry look at book by divig and look up lambda - there are other versions for
;without partner
; i think i am very close - just was not able to ask for help with syntax due to short office hours
; can i get partial credit? 
(define uncurry
  (lambda x
    (uncurry-helper (car x) (cdr x))))

(define uncurry-helper
  (lambda (f x)
    (if (null? (cdr (list x)))
        (f x)
        (uncurry-helper (f (car x)) (cdr x)))))



(define doubleup (lambda (x) (list x x)))
(flatmap doubleup '(1 2 3))

           

;((uncurry (curry3 +)) 1 2 3)
