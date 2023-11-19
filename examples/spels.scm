(define *objects* '(whiskey-bottle bucket frog chain))

(define *map*
  '((living-room
     (you are in the living-room of a wizards house. there is a wizard snoring loudly on the couch.)
     (west door garden)
     (upstairs stairway attic))
    (garden (you are in a beautiful garden. there is a well in front of you.) (east door living-room))
    (attic (you are in the attic of the wizards house. there is a giant welding torch in the corner.)
           (downstairs stairway living-room))))

(define *object-locations*
  '((whiskey-bottle living-room) (bucket living-room) (chain garden) (frog garden)))

(define *location* 'living-room)

(define (string-join l)
  (apply to-string (map symbol->string l)))

(define (describe-location location map)
  (string-join (second (assoc location map))))

(define (describe-path path)
  `(there is a ,(second path) going ,(first path) from here.))

; (displayln (describe-location `living-room *map*))

(define (describe-paths location m)
  (apply to-string (map string-join (map describe-path (cddr (assoc location m))))))

; (displayln (describe-paths 'living-room *map*))

(define (is-at obj loc obj-loc)
  (equal? (second (assoc obj obj-loc)) loc))

; (displayln (is-at 'whiskey-bottle 'living-room *object-locations*))

(define (describe-floor loc objs obj-loc)
  (apply to-string
         (map string-join
              (map (lambda (x) `(you see a ,x on the floor.))
                   (filter (lambda (x) (is-at x loc obj-loc)) objs)))))

; (displayln (describe-floor 'living-room *objects* *object-locations*))

(define (look)
  (to-string (describe-location *location* *map*)
             (describe-paths *location* *map*)
             (describe-floor *location* *objects* *object-locations*)))

(displayln (look))

(define (walk-direction direction)
  (let ([next (assoc direction (cddr (assoc *location* *map*)))])
    (cond
      [next
       (set! *location* (third next))
       (look)]
      [else "you cant go that way."])))

; (walk-direction 'west)
; (displayln (look))

(define-syntax walk
  (syntax-rules ()
    [(walk direction) (walk-direction 'direction)]))

(displayln (walk west))
(displayln (walk west))
; (displayln (look))
