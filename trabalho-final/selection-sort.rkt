#lang racket

;;Função: selection-sort Lista-Números, ((Número, Número) -> Boolean) -> Lista-Números
;;Objetivo: dado uma lista de números e uma função de comparação, retorna uma lista com os números ordenados
(define (selection-sort lst comparator)
    (cond
    [(empty? lst) empty]
    [else (local ((define to-compare (compare lst comparator)))
	(cons to-compare (selection-sort (retira to-compare lst) comparator)))]
    )
  )

;;Função compare: Lista-Números, ((Número, Número) -> Boolean) -> Número
;;Objetivo: Dado uma lista de números e uma função de comparação, retorna o primeiro número da lista que satisfizer a comparação com todos os outros elementos
(define (compare lst comparator)
  (cond
    [(true-to-all (first lst) (rest lst) comparator) (first lst)]
    [else (compare (rest lst) comparator)]
    )
  )

;;Função true-to-all: Número, Lista-Números, ((Número, Número) -> Boolean) -> Boolean
;;Objetivo: Verifica se o número dado satisfaz a comparação com todos os outros elementos da lista
(define (true-to-all element lst comparator)
  (cond
    [(empty? lst) true]
    [(comparator element (first lst)) (true-to-all element (rest lst) comparator)]
    [else false]
    )
  )

;;Função retira: Número, Lista-Números -> Lista-Números
;;Objetivo: retorna a lista com elemento removido
(define (retira element lst)
  (cond
    [(empty? lst) empty]
    [(= element (first lst)) (rest lst)]
    [else (cons (first lst) (retira element (rest lst)))]
    )
  )

(equal? (list 1 2 3 4) (selection-sort (list 1 2 3 4) <))
(equal? (list 4 3 2 1) (selection-sort (list 1 2 3 4) >))

(equal? (list 1 2 3 4) (selection-sort (list 4 3 2 1) <))
(equal? (list 4 3 2 1) (selection-sort (list 4 3 2 1) >))

(equal? empty (selection-sort empty >))
