; não conseguimos perceber diferença entre foldr e foldl, 
; se a função é comutativa ou associativa

(define list '(1 2 3 4 5))
(define sum (foldr + 0 list))
(display sum)

; ---------------------------------------------------------

; já a função a seguir (string-append) é comutativa:

(define list '("A" "B" "C"))
(foldl string-append "" list)
(foldr string-append "" list)

; ---------------------------------------------------------

; uso de map

(define list '(1 2 3 4 5))
(define (square x) (* x x))
(map square list)

; ---------------------------------------------------------

; uso de filter

(define (even? x) (= (remainder x 2) 0))
(filter even? list)