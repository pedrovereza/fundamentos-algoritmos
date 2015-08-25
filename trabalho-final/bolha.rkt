#lang racket

(require (lib "draw.ss" "teachpack" "htdp"))
(require lang/htdp-beginner)

(define-struct movel (altura largura posicao))
;;Um móvel do conjunto Movel é uma estrutura:
;; (make-movel altura largura posicao)
;;onde:
;; altura: Número -> altura do movel
;; largura: Número ->largura do movel
;; posicao: Posn -> posição do canto superior esquerdo do móvel

(define um-movel (make-movel 100 100 (make-posn 5 5)))

(define-struct bolha (x y delta-x delta-y cor))
;;uma bolha é uma estrutura 
;;(make-bolha numero numero numero numero simbolo)
;;onde:
;;x: Número -> posiçao x do centro da bolha
;;y: Número -> posição y do centro da bolha
;;delta-x: Número -> velocidade com que a bolha se movimenta no eixo x
;;delta-y: Número -> velocidade com que a bolha se movimenta no eixo y
;;cor: Símbolo -> cor da bolha

(define uma-bolha (make-bolha 500 500 +20 -17 'blue))
(define outra-bolha (make-bolha 600 600 -20 -17 'blue))

;;Bolhas é:
;; empty ou
;; (cons b lstb)
;; onde: b: Bolhas
;; lstb: Bolhas
(define bolhas (cons uma-bolha (cons outra-bolha empty)))

;;desenha-e-apaga: uma-bolha  ->  true
;;desenha e apaga o disco na área de desenho (canvas), considerando um tempo ESPERA entre as duas ações 
(define (desenha-e-apaga uma-bolha)
  (and
    (draw-circle (make-posn (bolha-x uma-bolha) (bolha-y uma-bolha)) 5 (bolha-cor uma-bolha))
    (sleep-for-a-while ESPERA)
    (clear-circle (make-posn (bolha-x uma-bolha) (bolha-y uma-bolha)) 5 (bolha-cor uma-bolha))))

;;move-bolha bolha  ->  bolha
;;move a bolha, redesenhando ela na nova posição
(define (move-bolha uma-bolha) 
  (make-bolha (+ (bolha-x uma-bolha) (bolha-delta-x uma-bolha))
		  (+ (bolha-y uma-bolha) (bolha-delta-y uma-bolha))
		  (bolha-delta-x uma-bolha)
		  (bolha-delta-y uma-bolha)
		  (bolha-cor uma-bolha)))

;;Canvas 
(define LARGURA 700)
(define ALTURA 700)
(define ESPERA .05)

;; Função: colisao? Bolha -> Boolean
;;Objetivo: dado uma bolha, verifica se houve colisão com um móvel pré-posicionado  ou com o canvas
(define (colisao? uma-bolha)
  (or (colidiu-movel? uma-bolha)(fora-canvas? uma-bolha)) 
  )

;;Função:colidiu-movel? Bolha -> Boolean
;;Objetivo: dado uma bolha, verifica se colidiu com um móvel pré-posicionado
(define (colidiu-movel? uma-bolha)
  (and
    (<= 0 (bolha-x uma-bolha) (+ (posn-x (movel-posicao um-movel)) (movel-largura um-movel)))
    (<= 0 (bolha-y uma-bolha) (+ (posn-y (movel-posicao um-movel)) (movel-altura um-movel)))))

;;Função:fora-canvas?: Bolha ->  Boolean
;;Objetivo:determina se a bolha está fora do canvas
(define (fora-canvas? uma-bolha)
  (not
    (and
	(<= 0 (bolha-x uma-bolha) LARGURA)
	(<= 0 (bolha-y uma-bolha) ALTURA))))


;;Função: move-bolhas Bolhas -> Bolhas
;;Objetivo: move todas as bolhas de uma lista, retornando uma nova lista com todas as bolhas que não colidiram com um móvel pré-posicionado ou com o canvas
(define (move-bolhas lstbolhas)
  (cond
    [(empty? lstbolhas) empty]
    [(colisao? (move-bolha (first lstbolhas))) (move-bolhas (rest lstbolhas))]
    [else (cons (move-bolha (first lstbolhas)) (move-bolhas (rest lstbolhas)))]
    )
  )

;;Função: desenha-todas-bolhas Bolhas -> Boolean
;;Objetivo: Desenha todas as bolhas em uma lista de bolhas
(define (desenha-todas-bolhas lstbolhas)
  (cond
    [(empty? lstbolhas) true]
    [else (and (desenha-e-apaga (first lstbolhas))
		   (desenha-todas-bolhas (rest lstbolhas)))]
    ))

;;Função move-ate-nao-ter-bolhas Bolhas -> Boolean
;;Objetivo: Movimenta bolhas pelo canvas até que não existam mais bolhas (colisão com móvel um canvas)
(define (move-ate-nao-ter-bolhas lstbolhas)
  (cond
    [(empty? lstbolhas) true]
    [else (and (desenha-todas-bolhas lstbolhas)
		   (desenha-movel um-movel)
		   (move-ate-nao-ter-bolhas (move-bolhas lstbolhas))
		   )]
    ))

;;Função desenha-movel Movel -> Boolean
;;Objetivo: desenha um móvel no canvas
(define (desenha-movel umMovel)
  (draw-solid-rect (movel-posicao umMovel)
			 (movel-largura umMovel)
			 (movel-altura umMovel)
			 'brown) 
  )

(start LARGURA ALTURA)
(move-ate-nao-ter-bolhas bolhas)
(stop)
