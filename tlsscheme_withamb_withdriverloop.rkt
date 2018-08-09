
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; extend The Little Schemer interpreter with nondeterminism

; add dynamic driver loop to enable dynamic specification

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define first car)

(define second cadr)

(define third caddr)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lookup-in-table
  (lambda (name table table-f)
    (cond 
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))))))

(define extend-table cons)


(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (names entry)
                          (vals entry)
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names vals entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car vals))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr vals)
                                  entry-f)))))


(define new-entry build)

(define names
  (lambda (entry) (car entry)))

(define vals
  (lambda (entry) (cadr entry)))

;;;top layer
(define value
  (lambda (e)
    (meaning e (quote ()) init-succeed init-fail)))

(define init-succeed
  (lambda (val fail)
    val))

(define init-fail
  (lambda ()
    'failed))
  

(define meaning
  (lambda (e table succeed fail)
    ((expression-to-action e) e table succeed fail)))





(define expression-to-action
  (lambda (e)
    (cond 
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      ((eq? e (quote not)) *const)
      ((eq? e (quote or)) *const)
      ((eq? e (quote list)) *const)
      ((eq? e (quote <)) *const)
      ((eq? e (quote >)) *const)
      ((eq? e (quote =)) *const)
      ((eq? e (quote distinct?)) *const)
      ((eq? e (quote abs)) *const)
      ((eq? e (quote +)) *const)
      ((eq? e (quote -)) *const)
      ((eq? e (quote *)) *const)
      ((eq? e (quote /)) *const)
      (else *identifier))))


(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond 
         ((eq? (car e) (quote quote))
          *quote)
         ((eq? (car e) (quote lambda))
          *lambda)
         ((eq? (car e) (quote let))
          *let)
         ((eq? (car e) (quote amb))
          *amb)                        
         ((eq? (car e) (quote require))
          *require)
         ((eq? (car e) (quote cond))
          *cond)
         ((eq? (car e) (quote begin))
          *sequence)
         (else *application)))
      (else *application))))


(define *const
  (lambda (e table succeed fail)
    (cond 
      ((number? e) (succeed e fail))
      ((eq? e #t) (succeed #t fail))
      ((eq? e #f) (succeed #f fail))
      (else (succeed (build (quote primitive) e) fail)))))


(define *quote
  (lambda (e table succeed fail)
    (succeed (text-of e) fail)))

(define text-of second)


(define *identifier
  (lambda (e table succeed fail)
    (succeed (lookup-in-table e table initial-table)
             fail)))


(define initial-table
  (lambda (name)
    (car (quote ()))))


(define *lambda
  (lambda (e table succeed fail)
    (succeed (build (quote non-primitive)
                    (cons table (cdr e)))
             fail)))

(define table-of first)

(define formals-of second)

(define body-of third)


(define *sequence
  (lambda (e table succeed fail)
    (define actions (cdr e))
    (define (sequentially first-proc succeed fail rest-proc)
      (if (null? rest-proc)
          (meaning first-proc
                   table
                   succeed
                   fail)
          (meaning first-proc
                   table
                   (lambda (a-value fail2)
                            (sequentially (car rest-proc) succeed fail2 (cdr rest-proc)))
                   fail)))
    (sequentially (car actions) succeed fail (cdr actions))))  
             
    


(define *let
  (lambda (e table succeed fail)
    (let ((associated-lambda-exp 
           (lambda-from-let e)))
      (meaning associated-lambda-exp table succeed fail))))


(define let-vars
  (lambda (e) 
    (map car (cadr e))))


(define let-vals
  (lambda (e)
    (map cadr (cadr e))))


(define let-body
  (lambda (e)
    (caddr e)))

(define lambda-from-let
  (lambda (e)
    (cons
     (list
      'lambda
      (let-vars e)
      (let-body e))
     (let-vals e))))




(define evcon
  (lambda (lines table succeed fail)
    (cond ((else? (question-of (car lines)))
           (meaning (answer-of (car lines))
                             table
                             succeed
                             fail))            
          (else
           (meaning (question-of (car lines))
                    table
                    (lambda (pred-value fail2)
                      (cond (pred-value
                             (meaning (answer-of (car lines))
                                      table
                                      succeed
                                      fail2))
                            (else (evcon (cdr lines) table succeed fail2))))
                    fail)))))
         

(define else?
  (lambda (x)
    (cond 
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)

(define answer-of second)



(define *cond 
  (lambda (e table succeed fail)
    (evcon (cond-lines-of e) table succeed fail)))

(define cond-lines-of cdr)


(define amb-choices cdr)

(define *amb
  (lambda (e table succeed fail)
    (define (try-next choices)
      (if (null? choices)
          (fail)
          (meaning (car choices)
                   table
                   succeed
                   (lambda() (try-next (cdr choices))))))
    (try-next (amb-choices e))))
    

(define *require
  (lambda (e table succeed fail)
    (meaning (require-to-cond (second e)) table succeed fail)))

(define (require-to-cond e)
  (list 'cond
        (list
         (list 'not
               e)
         (cons 'amb '()))
        (list 'else '(quote success))))


(define evlis
  (lambda (args table succeed fail)
    (cond 
      ((null? args) (succeed (quote ()) fail))
      (else
       (meaning (car args)
                table
                (lambda (arg fail2)
                  (evlis (cdr args)
                         table
                         (lambda (args fail3)
                           (succeed (cons arg args)
                                    fail3))
                         fail2))
                fail)))))
    

(define *application
  (lambda (e table succeed fail)
    (meaning (function-of e)
             table
             (lambda (proc fail2)
               (evlis (arguments-of e)
                      table
                      (lambda (args fail3)
                        (myapply proc
                                 args
                                 succeed
                                 fail3))
                      fail2))
             fail)))


(define function-of car)

(define arguments-of cdr)


(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))



(define myapply
  (lambda (fun vals succeed fail)
    (cond
      ((primitive? fun)
       (apply-primitive
        (second fun) vals succeed fail))
      ((non-primitive? fun)
       (apply-closure
        (second fun) vals succeed fail)))))


(define apply-primitive
  (lambda (name vals succeed fail)
    (cond
      ((eq? name (quote cons))
       (succeed (cons (first vals) (second vals))
                fail))
      ((eq? name (quote car))
       (succeed (car (first vals))
                fail))
      ((eq? name (quote cdr))
       (succeed (cdr (first vals))
                fail))
      ((eq? name (quote null?))
       (succeed (null? (first vals))
                fail))
      ((eq? name (quote eq?))
       (succeed (eq? (first vals) (second vals))
                fail))
      ((eq? name (quote atom?))
       (succeed (:atom? (first vals))
                fail))
      ((eq? name (quote zero?))
       (succeed (zero? (first vals))
                fail))
      ((eq? name (quote add1))
       (succeed (+ (first vals) 1)
                fail))
      ((eq? name (quote sub1))
       (succeed (sub1 (first vals))
                fail))
      ((eq? name (quote number?))
       (succeed (number? (first vals))
                fail))
      ((eq? name (quote not))
       (succeed (not (first vals))
                fail))
      ((eq? name (quote or))
       (succeed (eval-or vals)
                fail))
      ((eq? name (quote list))
       (succeed vals
                fail))
      ((eq? name (quote <))
       (succeed (< (first vals) (second vals))
                fail))
      ((eq? name (quote >))
       (succeed (> (first vals) (second vals))
                fail))
      ((eq? name (quote =))
       (succeed (= (first vals) (second vals))
                fail))
      ((eq? name (quote distinct?))
       (succeed (distinct? (first vals))
                fail))
      ((eq? name (quote abs))
       (succeed (if (>= (first vals) 0)
                    (first vals)
                    (- (first vals)))
                fail))
      ((eq? name (quote +))
       (succeed (+ (first vals) (second vals))
                fail))
      ((eq? name (quote -))
       (succeed (- (first vals) (second vals))
                fail))
      ((eq? name (quote *))
       (succeed (* (first vals) (second vals))
                fail))
      ((eq? name (quote /))
       (if (= 0 (second vals))
           (fail)
           (succeed (/ (first vals) (second vals))
                    fail)))      
      )))

(define (member? e s)
  (cond ((null? s) #f)
        ((eq? e (car s)) #t)
        (else (member? e (cdr s)))))

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member? (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

(define (eval-or l)
  (cond ((null? l) #f)
        (else (or (car l) (eval-or (cdr l))))))

(define :atom?
  (lambda (x)
    (cond 
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive))
       #t)
      ((eq? (car x) (quote non-primitive))
       #t)
      (else #f))))

(define apply-closure
  (lambda (closure vals succeed fail)
    (meaning (body-of closure)
               (extend-table
                (new-entry
                 (formals-of closure)
                 vals)
                (table-of closure))
               succeed
               fail)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; dynamic driver loop


(define (announce string)
  (newline) (display string) (newline))

(define (user-print val)
  (display val)
  (newline))

(define (prompt-for-input)
         (newline)
         (display ";;; Input problem in the form of (let ((<v1> <e1>) ...) (begin <exp1> ...))")
         (newline)
         (display ";;; Or input try-again for other solutions, ac to add constraint, dc to delete constraint, exit to end")
         (newline))

(define (remove-subclause sub original)
  (cond ((null? sub) original)
        ((null? original) original)
        ((equal? sub (car original))
         (remove-subclause sub (cdr original)))
        (else (cons (car original) (remove-subclause sub (cdr original))))))


(define (dynamic-driver-loop)
  (call-with-current-continuation
   (lambda (end)
     
     (define (dynamic)
       
       (define (construct-let bindings body)
         (list 'let
               bindings
               (cons 'begin body)))
       
       (define internal-loop
         (let ((bindings '())
               (body '()))
           (lambda (try-again)
             (prompt-for-input)
              (let ((input (read)))
                (cond
                  ((eq? input 'exit)
                   (end 'done))
                  ((eq? input 'try-again)
                   (try-again))
                  ((eq? input 'ac) ;abbreviation for add_constraint
                   (cond ((null? body)
                            (announce ";;; There is no current problem.")
                            (dynamic))
                         (else (announce ";;; Input new constraint:")
                               (let ((input2 (read)))                    
                                 (set! body (cons input2 body))                   
                                 (meaning (construct-let bindings body)
                                          '()
                                          (lambda (val fail)
                                            (announce ";;; Results:")
                                            (user-print val)
                                            (internal-loop fail))
                                          (lambda ()
                                            (announce
                                             ";;; There are no more solutions.")
                                            (internal-loop
                                             (lambda ()                                  
                                               (announce ";;; There are no more solutions. Problem deleted.")
                                               (dynamic)))))))))
                  ((eq? input 'dc) ;abbreviation for delete_constraint
                   (cond ((null? body)
                          (announce ";;; There is no current problem.")
                          (dynamic))
                         (else
                          (announce ";;; Constraint to be deleted:")
                          (let* ((input3 (read))
                                 (new-body (remove-subclause input3 body)))
                            (cond ((or (null? new-body) (equal? body new-body))
                                   (announce ";;; Problem not changed"))
                                  (else (set! body new-body)))                     
                            (meaning (construct-let bindings body)
                                     '()
                                     (lambda (val fail)
                                       (announce ";;; Results:")
                                       (user-print val)
                                       (internal-loop fail))
                                     (lambda ()
                                       (announce
                                        ";;; There are no more solutions.")
                                       (internal-loop
                                        (lambda ()                                  
                                          (announce ";;; There are no more solutions. Problem deleted.")
                                          (dynamic)))))))))
                                          
                  (else (begin
                          (newline)
                          (display ";;; Starting a new problem ")
                          (set! bindings (second input))
                          (set! body (cdr (third input)))
                          (meaning input
                                   '()                                  
                                   (lambda (val fail)
                                     (announce ";;; Results:")
                                     (user-print val)
                                     (internal-loop fail))                                  
                                   (lambda ()
                                     (announce
                                      ";;; There are no more solutions.")
                                     (internal-loop
                                      (lambda ()
                                        (announce ";;; There are no more solutions. Problem deleted.")
                                        (dynamic))))))))))))
       
       (internal-loop
        (lambda ()
          (announce ";;; There is no current problem.")
          (dynamic))))
     
     (dynamic))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; tests


; (value '7)

; (value '(add1 6))

; (value '(cons (quote x) (quote ())))

 
; (value '((lambda (x) (add1 x)) 3))

; (value '((lambda (x) (add1 x)) 
;          ((lambda (x) (add1 x)) 4)))

;(value '(((lambda (y)
;            (lambda (x) (cons x y)))
;          3)
;         4))

;(value '(let ((x (amb 1 3 4))
;              (y (amb 3 4)))
;          (begin
;            (require (= x y))
;            (list x y))))



;(dynamic-driver-loop)

#|
(value '(let ((baker (amb 1 2 3 4 5))
              (cooper (amb 1 2 3 4 5))
              (fletcher (amb 1 2 3 4 5))
              (miller (amb 1 2 3 4 5))
              (smith (amb 1 2 3 4 5)))
          (begin    
            (require (not (= baker 5)))
            (require (not (= cooper 1)))
            (require (not (= fletcher 5)))
            (require (not (= fletcher 1)))
            (require (> miller cooper))
            (require (not (= (abs (- smith fletcher)) 1)))
            (require (not (= (abs (- fletcher cooper)) 1)))
            (require (distinct? (list baker cooper fletcher miller smith)))          
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith)))))
|#

#|
(value
 '(let ((a (amb 1 4 7 9))
        (b (amb 1 4 7 9))
        (c (amb 1 4 7 9))
        (d (amb 1 4 7 9))
        (f1 (amb + - * /))
        (f2 (amb + - * /))
        (f3 (amb + - * /)))
    (begin
      (require (distinct? (list a b c d)))
      (require (or (= 24 (f1 a (f2 b (f3 c d))))  ; (a (b (c d)))
                   (= 24 (f2 (f1 a b) (f3 c d)))  ; ((a b) (c d))
                   (= 24 (f3 (f1 a (f2 b c)) d))  ; ((a (b c)) d)
                   (= 24 (f1 a (f3 (f2 b c) d)))  ; (a ((b c) d))
                   (= 24 (f3 (f2 (f1 a b) c) d)))); (((a b) c) d)
      (cond ((= 24 (f1 a (f2 b (f3 c d))))
             (list a (list f2 b (list f3 c d))))
            ((= 24 (f2 (f1 a b) (f3 c d)))
             (list f2 (list f1 a b) (list f3 c d)))
            ((= 24 (f3 (f1 a (f2 b c)) d))
             (list f3 (list f1 a (list f2 b c)) d))
            ((= 24 (f1 a (f3 (f2 b c) d)))
             (list f1 a (list f3 (list f2 b c) d)))
            ((= 24 (f3 (f2 (f1 a b) c) d))
             (list f3 (list f2 (list f1 a b) c) d))))))
 |# 