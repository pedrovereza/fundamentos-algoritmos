#lang racket
(provide fahrenheit-celsius)

;; Função fahrenheit-celsius: Número -> Número
;; Objetivo: Dada uma temperatura na escala Fahrenheit, retorna a temperatura em escala Celsius
;; Exemplo: (fahrenheit-celsius 60) -> 15.5
(define (fahrenheit-celsius temperatura)
  (/ (- temperatura 32) 1.8)
)
