#lang racket
(require racket/format)

(define-struct veiculo-passeio (cor tipo possui-airbag))
;; Um elemento veiculo-passeio do conjunto Veiculo-Passeio é uma estrutura
;;    (make-veiculo-passeio cor tipo possui-airbag), onde
;; cor : Símbolo, é a cor do veículo,
;; tipo : Símbolo, é o tipo do veículo de passeio,
;; possui-airbag: Boolean, indica se veículo de passeio possui airbag 

(define-struct veiculo-utilitario (cor numero-assentos tracao-quatro-rodas))
;; Um elemento veiculo-utilitario do conjunto Veiculo-Utilitario é uma estrutura
;;    (make-veiculo-utilitario cor numero-assentos tracao-quatro-rodas), onde
;; cor : Símbolo, é a cor do veículo,
;; numero-assentos :Número, é o número de assentos do veículo utilitário, 
;; tracao-quatro-rodas : Boolean, indica se veículo utilitário possui tração nas quatro rodas

(define-struct veiculo-caminhao (cor total-carga volume-carga))
;; Um elemento veiculo-caminhao do conjunto Veiculo-Caminhao é uma estrutura
;;    (make-veiculo-caminhao cor total-carga volume-carga), onde
;; cor : Símbolo, é a cor do veículo,
;; total-carga :Número, é o número em toneladas que representa a capacidade de transporte de carga, 
;; volume-carga: Número, é o volume total de espaço para cargas

(define-struct estoque (numeroPasseio numeroUtilitario numeroCaminhao passeiosVendidos utilitariosVendidos caminhoesVendidos))
;; Um elemento estoque do conjunto Estoque é uma estrutura
;;    (make-estoque numeroPasseio numeroUtilitario numeroCaminhao passeiosVendidos utilitariosVendidos caminhoesVendidos), onde
;; numeroPasseio : Número, é a quantidade de veículos de passeio no estoque,
;; numeroUtilitario : Número, é a quantidade de veículos utilitarios no estoque,
;; numeroCaminhao : Número, é a quantidade de veículos caminhão no estoque,
;; passeiosVendidos : Número, é a quantidade de veículos de passeio vendidos,
;; utilitariosVendidos : Número, é a quantidade de veículos utilitarios vendidos,
;; caminhoesVendidos : Número, é a quantidade de veículos caminhoes vendidos

(define-struct tipo-cor (tipo cor))
;; Um elemento tipo-cor do conjunto Tipo-Cor é uma estrutura
;;    (make-tipo-cor tipo cor), onde
;; cor : Símbolo, é a cor do veículo,
;; tipo: Símbolo, é o tipo do veículo ('Passeio, 'Utilitario ou 'Caminhao)


(define passeio (make-veiculo-passeio 'Azul 'Sedan false))
(define utilitario (make-veiculo-utilitario 'Verde 5 true))
(define caminhao (make-veiculo-caminhao 'Prata 3 300))

(define umEstoque (make-estoque 10 20 30 0 0 0))

;;Função tipo-cor-veiculo: Veiculo -> Tipo-Cor
;;Um veículo é um elemento do conjunto Veiculo-Passeio ou Veiculo-Utilitario ou Veiculo-Caminhao
;;Objetivo: dado um veículo, indicar seu tipo e Cor
;;Exemplos: (tipo-cor-veiculo passeio) -> "Veiculo de passeio Azul"
(define (tipo-cor-veiculo veiculo)
  (cond
	[(veiculo-passeio? veiculo) (make-tipo-cor 'Passeio (veiculo-passeio-cor veiculo))]
	[(veiculo-utilitario? veiculo) (make-tipo-cor 'Utilitario (veiculo-utilitario-cor veiculo))]
	[(veiculo-caminhao? veiculo) (make-tipo-cor 'Caminhao (veiculo-caminhao-cor veiculo))]
  )
)

(tipo-cor-tipo (tipo-cor-veiculo passeio))
(tipo-cor-cor (tipo-cor-veiculo passeio))

(tipo-cor-tipo (tipo-cor-veiculo utilitario))
(tipo-cor-cor (tipo-cor-veiculo utilitario))

(tipo-cor-tipo (tipo-cor-veiculo caminhao))
(tipo-cor-cor (tipo-cor-veiculo caminhao))

;;Função: remove-assento-muda-cor: veiculo-utilitario -> veiculo-utilitario
;;Objetivo: Se o veiculo utilitario possui tração nas quatro rodas, remove um assento e muda a cor para azul
;;Exemplos:(tipo-cor-cor (tipo-cor-veiculo (remove-assento-muda-cor (make-veiculo-utilitario 'Verde 5 true)))) -> 'Azul
;;Exemplos:(veiculo-utilitario-numero-assentos (remove-assento-muda-cor (make-veiculo-utilitario 'Verde 5 true)))) -> 4
(define (remove-assento-muda-cor veiculo)
  (cond
	[(veiculo-utilitario-tracao-quatro-rodas veiculo) (make-veiculo-utilitario
														'Azul
														(- (veiculo-utilitario-numero-assentos veiculo) 1)
														(veiculo-utilitario-tracao-quatro-rodas veiculo)
														)]
	[else veiculo]
	)
)

(tipo-cor-cor (tipo-cor-veiculo (remove-assento-muda-cor utilitario)))
(veiculo-utilitario-numero-assentos (remove-assento-muda-cor utilitario))


;;Função informacao-especifica: Veiculo -> String
;;Um veículo é um elemento do conjunto Veiculo-Passeio ou Veiculo-Utilitario ou Veiculo-Caminhao
;;Objetivo: Se for um veiculo-passeio sedan, retorna se possui airbag
;;Se for um veiculo-utilitario, retorna se possui tração nas quatro rodas
;;Se for um veiculo-caminhao, retorna o volume total de carga
;;Exemplo: (informacao-especifica passeio) -> "Possui airbag: false"
;;Exemplo: (informacao-especifica utilitatio) -> "Possui tração nas quatro rodas: true"
;;Exemplo: (informacao-especifica caminhao) -> "Volume máximo de carga que pode ser transportado: 300"
(define (informacao-especifica veiculo)
  (cond
	[(veiculo-passeio? veiculo) (informacao-especifica-passeio veiculo)]
	[(veiculo-utilitario? veiculo) (informacao-especifica-utilitario veiculo)]
	[(veiculo-caminhao? veiculo) (informacao-especifica-caminhao veiculo)]
	[else "Entrada não é um veiculo"]
  )
)

;;Função informação-especifica-passeio: Veiculo-Passeio -> String
;;Objetivo: Se for um veiculo-passeio sedan, retorna se possui airbag
;;Exemplo: (informacao-especifica passeio) -> "Possui airbag: false"
(define (informacao-especifica-passeio veiculo)
	(cond
	  [(symbol=? (veiculo-passeio-tipo veiculo) 'Sedan) (format "Possui airbag: ~a" (veiculo-passeio-possui-airbag veiculo))]
	  [else ""]
	)  
)

;;Função informação-especifica-utilitario: Veiculo-Utilitario -> String
;;Objetivo: Determina se possui tração nas quatro rodas
;;Exemplo: (informacao-especifica utilitatio) -> "Possui tração nas quatro rodas: true"
(define (informacao-especifica-utilitario veiculo)
	(format "Possui tração nas quatro rodas: ~a" (veiculo-utilitario-tracao-quatro-rodas veiculo))  
)

;;Função informação-especifica-caminhao: Veiculo-Caminhao -> String
;;Objetivo: Determina o volume de carga que pode ser transportado
;;Exemplo: (informacao-especifica caminhao) -> "Volume máximo de carga que pode ser transportado: 300"
(define (informacao-especifica-caminhao veiculo)
	(format "Volume máximo de carga que pode ser transportada: ~a" (veiculo-caminhao-volume-carga veiculo))  
)

(informacao-especifica passeio)
(informacao-especifica utilitario)
(informacao-especifica caminhao)

;;Função atualiza-estoque: Estoque, Número -> Estoque
;;Objetivo: Atualizar o número de veiculos de passeio no estoque
;;Exemplo: (estoque-numeroPasseio (atualiza-estoque (make-estoque 10 20 30 0 0 0) 6)) -> 4
;;Exemplo: (estoque-passeiosVendidos (atualiza-estoque (make-estoque 10 20 30 0 0 0) 6)) -> 6
(define (atualiza-estoque umEstoque numeroPasseiosVendidos)
  (make-estoque
	(- (estoque-numeroPasseio umEstoque) numeroPasseiosVendidos)
	(estoque-numeroUtilitario umEstoque)
	(estoque-numeroCaminhao umEstoque)
	(+ (estoque-passeiosVendidos umEstoque) numeroPasseiosVendidos)
	(estoque-utilitariosVendidos umEstoque)
	(estoque-caminhoesVendidos umEstoque)
	)
)

(= 4 (estoque-numeroPasseio (atualiza-estoque umEstoque 6))) 
(= 6 (estoque-passeiosVendidos (atualiza-estoque umEstoque 6))) 

