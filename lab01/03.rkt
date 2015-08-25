#lang racket

;;Função move-robo: Simbolo, Numero, Numero -> Número
;; Objetivo: Calcular nova posição do robo dado o sentido da movimentação, a posição atual e o número de posiçoes que devem ser movidas,
;; respeitando as posiçoes limites 1 e 50.
;; Exemplos
;; (move-robo 50 'D 1) -> 50
;; (move-robo 48 'D 1 ) -> 49
;; (move-robo 50 'E 2) -> 48
;; (move-robo 1  'E 1) -> 1
(define (move-robo posicao-atual sentido posicoes)
  (cond
	[(and (symbol=? 'D sentido) (<= (+ posicao-atual posicoes) 50)) (+ posicao-atual posicoes)]
	[(and (symbol=? 'E sentido) (>= (- posicao-atual posicoes) 1)) (- posicao-atual posicoes)]
	[else posicao-atual]
	)
  )

(move-robo 50 'D 1)
(move-robo 48 'D 1 )
(move-robo 50 'E 2)
(move-robo 1  'E 1)
