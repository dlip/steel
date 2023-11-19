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

(define (describe-location location map)
  (second (assoc location map)))

(define (describe-path path)
  `(there is a ,(second path) going ,(first path) from here.))

; (displayln (describe-location `living-room *map*))

(define (describe-paths location m)
  (apply append (map describe-path (cddr (assoc location m)))))

(displayln (describe-paths 'living-room *map*))

(define (is-at obj loc obj-loc)
  (equal? (second (assoc obj obj-loc)) loc))

; (displayln (is-at 'whiskey-bottle 'living-room *object-locations*))

(define (describe-floor loc objs obj-loc)
  (apply append
         (map (lambda (x) `(you see a ,x on the floor.))
              (filter (lambda (x) (is-at x loc obj-loc)) objs))))
; (displayln (describe-floor 'living-room *objects* *object-locations*))

(define (look)
  (describe-location *location* *map*)
  (describe-paths *location* *map*)
  (describe-floor *location* *objects* *object-locations*))

(displayln (look))

(define (walk-direction direction)
  (let ([next (assoc direction (cddr (assoc *location* *map*)))])
    (cond
      [next
       (set! *location* (third next))
       (look)]
      [else '(you cant go that way.)]))) ; TODO escape backqoutes

; (walk-direction 'west)
; (displayln (look))

(define-syntax walk
  (syntax-rules ()
    [(walk direction) (walk-direction 'direction)]))

(define (pickup-object object)
  (cond
    [(is-at object *location* *object-locations*)
     (set! *object-locations* (cons (list object 'body) *object-locations*))
     `(you are now carrying the ,object)]
    [else '(you cannot get that.)]))

(define-syntax pickup
  (syntax-rules ()
    [(pickup direction) (pickup-object 'direction)]))

(define (inventory)
  (filter (lambda (x) (is-at x 'body *object-locations*)) *objects*))

(define (have object)
  (member object (inventory)))

(define *chain-welded* #f)

; (define (weld subject object)
;   (cond
;     [(and (eq? *location* 'attic)
;           (eq? subject 'chain)
;           (eq? object 'bucket)
;           (have 'chain)
;           (have 'bucket)
;           (not *chain-welded*))
;      (set! *chain-welded* #t)
;      '(the chain is now securely welded to the bucket.)]
;     [else '(you cannot weld like that.)]))

(define *bucket-filled* #f)

; (define (dunk subject object)
;   (cond
;     [(and (eq? *location* 'garden)
;           (eq? subject 'bucket)
;           (eq? object 'well)
;           (have 'bucket)
;           *chain-welded*)
;      (set! *bucket-filled* #t)
;      '(the bucket is now full of water)]
;     [else '(you cannot dunk like that.)]))

; Error: Not yet implemented
(define-syntax game-action
  (syntax-rules ()
    [(game-action command subj obj place rest...)
     (define-syntax command
       (syntax-rules ()
         [(command subject object)
          (cond
            [(and (eq? *location* 'place) (eq? 'subject 'subj) (eq? 'object 'obj) (have 'subj)) rest]
            [else '(i cant command like that.)])]))]))

(game-action
 weld
 chain
 bucket
 attic
 (cond
   [(and (have 'bucket) (set! *chain-welded* #t)) '(the chain is now securely welded to the bucket.)]
   [else '(you do not have a bucket.)]))

(displayln (pickup whiskey-bottle))
; (displayln (walk west))
; (displayln (walk west))

(displayln (weld 'chain 'bucket))
