#lang scheme

(define (insert-end lista elem)
  (append lista (list elem)))

(define (insert-end2 lista elem)
  (if (null? lista)
      (list elem)
      (cons (car lista) (insert-end2 (cdr lista) elem))))