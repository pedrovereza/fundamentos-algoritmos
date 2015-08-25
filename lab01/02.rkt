#lang racket

;; Função medias: Caracter, Número, Número, Número -> Número
;; Objetivo: Dado um caracter e tres números, retorna a médias dos valores informados de acordo com a regra: 
;; caractere 'a' retorna média aritmética; caractere 'g' retorna a média geométrica; caractere 'h' retorna a média harmonica
;; Exemplos:
;; (medias #\a 1 2 3) -> 2
;; (medias #\g 1 2 4) -> 2.0
;; (medias #\h 2 6 8) -> 72/19
(define (medias tipo valor1 valor2 valor3)
  (cond 
	[(char=? #\a tipo) (media-aritmetica valor1 valor2 valor3)]
	[(char=? #\g tipo) (media-geometrica valor1 valor2 valor3)]
	[(char=? #\h tipo) (media-harmonica valor1 valor2 valor3)]
	)
  )

;; Função media-aritmetica: Número, Número, Número -> Número
;; Objetivo: Dado 3 valores numéricos, retorna a média artimética destes valores
;; Exemplo: (media-aritmetica 1 2 3) -> 2
(define (media-aritmetica valor1 valor2 valor3)
  (/ (+ valor1 valor2 valor3) 3)
)

;; Função media-geometrica: Número, Número, Número -> Número
;; Objetivo: Dado 3 valores numéricos, retorna a média geométrica destes valores
;; Exemplo: (media-geometrica 1 2 4) -> 2
(define (media-geometrica valor1 valor2 valor3)
  (expt (* valor1 valor2 valor3) 1/3)
)

;; Função media-harmonica: Número, Número, Número -> Número
;; Objetivo: Dado 3 valores numéricos, retorna a média harmônica destes valores
;; Exemplo: (media-geometrica 2 6 8) -> 72/19
(define (media-harmonica valor1 valor2 valor3)
  (/ 3 (+ (/ 1 valor1) (/ 1 valor2) (/ 1 valor3)))
)

(medias #\a 1 2 3)
(medias #\g 1 2 4)
(medias #\h 2 6 8)
