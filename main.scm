; Authors: Christopher Castel, Martin D'Hoedt

; Multiple inheritance
; A class such as color-point can now inherit from both "color" and "point".
; The method lookup has been slightly modified to allow for a list of parents to be used.
; The order of the list matters as it is the first method that is matched that will be returned.
; The "set-self!" procedure is used to update the "self" of all parents defined in a class.

; Final classes
; A final class is a class that cannot be extended by a subclass.
; We cannot, unfortunately, stop the execution of the program when a violation is detected.
; A message is still emitted to the console, warning the user that they attempt to extend a class marked as "final".
; In the case of a violation, the class is not extended but an instance is still created.
; By default, all classes are not final (inherited from "object"),
; the user has to define a "final?" procedure and set its return value to #t.

(define (object)
  (define super (list 'nil))
  (define (set-self! new-self)
    (set! self new-self)
  )
  (define (final?) #f)
  (define (type) 'object)
  (define (self m)
    (case m
      ('set-self! set-self!)
      ('final? final?)
      ('type type)
    )
  )
  self
)

(define (point x y)
  (define supers (get-supers (list (object))))
  (define (getx) x)
  (define (gety) y)
  (define (type) 'point)
  (define (info) (list ((self 'type)) ((self 'getx)) ((self 'gety))))
  (define (add p)
    (point
      (+ ((self 'getx)) ((p 'getx)))
      (+ ((self 'gety)) ((p 'gety)))
    )
  )
  (define (setx! new-x) (set! x new-x))
  ;(define (final?) #t)
  (define (set-self! new-self)
    (set! self new-self)
    (send-supers supers 'set-self! new-self)
  )
  (define (self m)
    (case m
      ('set-self! set-self!)
      ;('final? final?)
      ('type type)
      ('info info)
      ('getx getx)
      ('setx! setx!)
      ('gety gety)
      ('add add)
      (else (method-lookup supers m))
    )
  )
  self
)

(define (color col)
  (define supers (get-supers (list (object))))
  (define (get-color) col)
  (define (type) 'color)
  (define (info) (list ((self 'type)) ((self 'get-color))))
  ;(define (final?) #t)
  (define (set-self! new-self)
    (set! self new-self)
    (send-supers supers 'set-self! new-self)
  )
  (define (self m)
    (case m
      ('set-self! set-self!)
      ('type type)
      ('info info)
      ('get-color get-color)
      ;('final? final?)
      (else (method-lookup supers m))
    )
  )
  self
)

(define (color-point x y col)
  (define supers (get-supers (list (point x y) (color col))))
  (define (add cp)
    (color-point
      (+ ((self 'getx)) ((cp 'getx)))
      (+ ((self 'gety)) ((cp 'gety)))
      ((self 'get-color))
    )
  )
  (define (type) 'color-point)
  (define (set-self! new-self)
    (set! self new-self)
    (send-supers supers 'set-self! new-self)
  )
  (define (self m)
    (case m
      ('set-self! set-self!)
      ('type type)
      ('add add)
      (else (method-lookup supers m))
    )
  )
  self
)

; Returns the first procedure corresponding to the <message> if any of the <receivers> is a procedure,
; 'null otherwise.
(define (method-lookup receivers message)
  (if (null? receivers)
    'null
    ; https://docs.racket-lang.org/r5rs/r5rs-std/r5rs-Z-H-9.html?q=error#%25_idx_576
    (if (procedure? ((car receivers) message))
      ((car receivers) message)
      (method-lookup (cdr receivers) message)
    )
  )
)

; Finds the "method" corresponding to <message> defined in <receiver> and executes it.
; Outputs error messages in the case of an invalid <receiver> procedure
; or if the <message> is not defined in the <receiver>.
(define (send receiver message . args)
  (define method (method-lookup (list receiver) message))
  (if (not (procedure? receiver))
    (display "[error] Inappropriate receiver object\n")
    (if (procedure? method)
      (apply method args)
      (display "[error] Message not understood\n")
    )
  )
)

; Send <message> to all <supers>.
(define (send-supers supers message . args)
  (if (not (null? supers))
    (begin
      (apply send (car supers) message args)
      (send-supers (cdr supers) message args)
    )
  )
)

; Created an <instance> of the given super class.
; If the class is declared as "final", an warning message is displayed and 'nil is returned.
(define (get-super instance)
  (define final (eqv? #t (send instance 'final?)))
  (if final
    (begin
      (display (list "[warning] Cannot extend class :" (send instance 'info)))
      (display " !\n")
      'nil
    )
    instance
  )
)

; Creates a list of instances from the list of <super> classes.
; It uses "get-super" which checks for any "final" violation.
(define (get-supers supers)
  (if (null? supers)
    '()
    (let
      ((
        instance
        (get-super (car supers))
      ))
      (if (procedure? instance)
        (cons instance (get-supers (cdr supers)))
        (get-supers (cdr supers))
      )
    )
  )
)

(define (new class . params)
  ; could check if instance is extending final class
  ; wil get error calling method on 'nil super class
  ; cf. get-super
  (define instance (apply class params))
  (send instance 'set-self! instance)
  instance
)

(define o (object))
(send o 'type) ; object
(send o 'foo) ; should display "Message not understood"
(define p1 (point 1 2))
(define p2 (point 3 4))
(send p1 'getx) ; 1
(send p1 'gety) ; 2
(send p2 'getx) ; 3
(send p2 'gety) ; 4
(define p (send p1 'add p2))
(send p 'info) ; (point 4 6)
(define cp (color-point 5 6 'red))
(send cp 'type) ; color-point
(send cp 'getx) ; 5
(send cp 'gety) ; 6
(send cp 'get-color) ; red
(send cp 'info) ; (point 5 6)
(define cp-1 (send cp 'add (color-point 1 2 'green)))
(send cp-1 'type) ; color-point
(send cp-1 'getx) ; 6
(send cp-1 'gety) ; 8
(send cp-1 'get-color) ; red
