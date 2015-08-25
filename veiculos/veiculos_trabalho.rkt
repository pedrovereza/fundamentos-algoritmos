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

(define-struct total-estoque (emEstoque vendidos))
;; Um elemento total-estoque do conjunto Total-Estoque é uma estrutura
;;    (make-total-estoque emEstoque vendidos), onde
;; emEstoque : Número, representa o total de veiculos em estoque 
;; vendidos: Número, representa o total de veiculos vendidos 


(define passeio (make-veiculo-passeio 'Azul 'Sedan false))
(define utilitario (make-veiculo-utilitario 'Verde 5 true))
(define caminhao (make-veiculo-caminhao 'Prata 3 300))

(define estoqueMontadora (make-estoque 10 20 30 0 0 0))

;;Função pode-carregar. Veiculo-Caminhao -> Boolean
;;Objetivo: Dado o peso e volume de uma carga, determina se veiculo-caminhao pode carrega-la
;;Exemplos:
;;(pode-carregar 3 100 caminhao) -> True
;;(pode-carregar 4 100 caminhao) -> False
;;(pode-carregar 2 301 caminhao) -> False
(define (pode-carregar peso-carga volume-carga caminhao)
	  (and (<= peso-carga (veiculo-caminhao-total-carga caminhao))
			(<= volume-carga (veiculo-caminhao-volume-carga caminhao)))
)

;;Função informacao. Veiculo -> Informacao
;;Um elemento Informação é um elemento do conjunto String ou Número
;;Objetivo: Dado um veiculo, retorna informações dependendo do tipo do veículo.
;;Exemplo: (informacao passeio) -> "Veiculo Sedan com cor Azul"
;;(informacao utilitario) -> "Número de assentos: 5"
;;(informacao (make-veiculo-utilitario 'Preta 3 false)) -> ""
;;(informacao caminhao) -> 0
;;(informacao (make-veiculo-caminhao 'Azul 5500 20)) -> 5500
(define (informacao veiculo)
	(cond
	  [(veiculo-passeio? veiculo) (informacao-passeio veiculo)]
	  [(veiculo-utilitario? veiculo) (informacao-utilitario veiculo)]
	  [(veiculo-caminhao? veiculo) (informacao-caminhao veiculo)]
	  [else "Não é um veículo"]
	  )
)

;;Função informacao-passeio: Veiculo-Passeio -> String
;;Obs: Tipo String escolhido pois o enunciado pede que os valores sejam "mostrados"
;;Objetivo: Dado um veiculo-passeio, retorna uma string contendo o seu tipo e cor
;;Exemplo: (informacao-passeio passeio) -> "Veiculo Sedan com cor Azul" 
(define (informacao-passeio veiculo)
	(format "Veiculo ~a com cor ~a" (veiculo-passeio-tipo veiculo) (veiculo-passeio-cor veiculo))
)

;;Função informacao-utilitario: Veiculo-Utilitario -> String
;;Obs: Tipo String escolhido pois o enunciado pede que os valores sejam "apresentados"
;;Objetivo: Dado um veiculo-utilitario, retorna uma String contendo o número de assentos se o veículo
;;possuir tração nas quatro rodas. Senão, uma String vazia
;;Exemplo: (informacao-utilitario utilitario) -> "Número de assentos: 5"
;;(informacao-utilitario (make-veiculo-utilitario 'Preta 3 false)) -> ""
(define (informacao-utilitario veiculo)
  (cond
	[(veiculo-utilitario-tracao-quatro-rodas veiculo) (format "Número de assentos: ~a" (veiculo-utilitario-numero-assentos veiculo))]
	[else ""]
  )
)

;;Função informacao-caminhao: Veiculo-Caminhao -> Número
;;Objetivo: Dado um veiculo-caminhao, retorna quanto de carga o caminhao pode carregar, se este valor for maior que
;;5000. Senão, retorna zero.
;;Exemplo: (informacao-caminhao caminhao) -> 0
;;(informacao-caminhao (make-veiculo-caminhao 'Azul 5500 20)) -> 5500
(define (informacao-caminhao veiculo)
  (cond
	[(> (veiculo-caminhao-total-carga veiculo) 5000) (veiculo-caminhao-total-carga veiculo)]
	[else 0]
	)
)

;;Função programa -> Número, Estoque, Veiculo -> Info-Montadora
;;Um elemento Info-Montadora é um elemento Estoque ou Total-Estoque
;;Objetivo: Dada opção 1, atualiza o Estoque somando uma unidade produzida de Veiculo
;;Dada opção 2, atualiza o Estoque com a venda de Veiculo
;;Dada opção 3, retorna Total-Estoque com totais de veiculos produzidos e vendidos de Estoque
(define (programa opcao umEstoque veiculo)
	(cond
	  [(= 1 opcao) (veiculo-produzido umEstoque veiculo)]
	  [(= 2 opcao) (veiculo-vendido umEstoque veiculo)]
	  [(= 3 opcao) (totais-estoque umEstoque)]
	)  
)

;;Função totais-estoque. Estoque -> Total-Estoque
;;Objetivo: Dado um estoque, retorna os totais de veículos produzidos e vendidos
;;Exemplo:
(define (totais-estoque umEstoque)
	(make-total-estoque 
		  (+ (estoque-numeroPasseio umEstoque)
			 (estoque-numeroUtilitario umEstoque)
			 (estoque-numeroCaminhao umEstoque))
		  (+ (estoque-passeiosVendidos umEstoque)
			 (estoque-utilitariosVendidos umEstoque)
			 (estoque-caminhoesVendidos umEstoque))
	)  
)

;;Função: veiculo-produzido. Estoque, Veiculo -> Estoque
;;Objetivo: Dado um veículo produzido, atualiza a respectiva quantidade no estoque
;;Exemplo:
(define (veiculo-produzido umEstoque veiculo)
  (cond
	[(veiculo-passeio? veiculo) (veiculo-passeio-produzido umEstoque)]
	[(veiculo-utilitario? veiculo) (veiculo-utilitario-produzido umEstoque)]
	[(veiculo-caminhao? veiculo) (veiculo-caminhao-produzido umEstoque)]
	)
  )

;;Função veiculo-vendido: Estoque, Veiculo -> Estoque
;;Objetivo: atualizar o estoque de acordo com o tipo de veiculo vendido
;;Exemplo:
(define (veiculo-vendido umEstoque veiculo)
  (cond
	[(veiculo-passeio? veiculo) (veiculo-passeio-vendido umEstoque)]
	[(veiculo-utilitario? veiculo) (veiculo-utilitario-vendido umEstoque)]
	[(veiculo-caminhao? veiculo) (veiculo-caminhao-vendido umEstoque)]
	)
  )

;;Função veiculo-passeio-vendido: Estoque -> Estoque
;;Objetivo: atualizar a quantidade de estoque considerando a venda de um Veiculo-Passeio
;;Exemplo: (estoque-numeroPasseio (veiculo-passeio-vendido (make-estoque 1 0 0 0 0 0)) -> 0
;;Exemplo: (estoque-passeiosVendidos (veiculo-passeio-vendido (make-estoque 1 0 0 0 0 0)) -> 1
(define (veiculo-passeio-vendido umEstoque) 
  (make-estoque (- (estoque-numeroPasseio umEstoque) 1)
				(estoque-numeroUtilitario umEstoque)
				(estoque-numeroCaminhao umEstoque)
				(+ 1 (estoque-passeiosVendidos umEstoque))
				(estoque-utilitariosVendidos umEstoque)
				(estoque-caminhoesVendidos umEstoque)
  ) 
)

;;Função veiculo-utilitario-vendido: Estoque -> Estoque
;;Objetivo: atualizar a quantidade de estoque considerando a venda de um Veiculo-Utilitario
;;Exemplo: (estoque-numeroUtilitario (veiculo-utilitario-vendido (make-estoque 0 1 0 0 0 0)) -> 0
;;Exemplo: (estoque-utilitariosVendidos (veiculo-utilitario-vendido (make-estoque 0 0 1 0 0 0)) -> 1
(define (veiculo-utilitario-vendido umEstoque) 
  (make-estoque (estoque-numeroPasseio umEstoque)
				(- (estoque-numeroUtilitario umEstoque) 1)
				(estoque-numeroCaminhao umEstoque)
				(estoque-passeiosVendidos umEstoque)
				(+ 1 (estoque-utilitariosVendidos umEstoque))
				(estoque-caminhoesVendidos umEstoque)
  ) 
)

;;Função veiculo-caminhao-vendido: Estoque -> Estoque
;;Objetivo: atualizar a quantidade de estoque considerando a venda de um Veiculo-Caminhao
;;Exemplo: (estoque-numeroCaminhao (veiculo-caminhao-vendido (make-estoque 0 0 1 0 0 0)) -> 0
;;Exemplo: (estoque-caminhoesVendidos (veiculo-caminhao-vendido (make-estoque 0 0 1 0 0 0)) -> 1
(define (veiculo-caminhao-vendido umEstoque) 
  (make-estoque (estoque-numeroPasseio umEstoque)
				(estoque-numeroUtilitario umEstoque)
				(- (estoque-numeroCaminhao umEstoque) 1)
				(estoque-passeiosVendidos umEstoque)
				(estoque-utilitariosVendidos umEstoque)
				(+ 1 (estoque-caminhoesVendidos umEstoque))
  ) 
)

;;Função: veiculo-passeio-produzido. Estoque, Veiculo-Passeio -> Estoque
;;Objetivo: Dado um veículo produzido, atualiza a respectiva quantidade de veiculos de passeio em estoque
;;Exemplo: (estoque-numeroPasseio (veiculo-passeio-produzido (make-estoque 1 0 0 0 0 0)) -> 2
(define (veiculo-passeio-produzido umEstoque) 
  (make-estoque (+ 1 (estoque-numeroPasseio umEstoque))
				(estoque-numeroUtilitario umEstoque)
				(estoque-numeroCaminhao umEstoque)
				(estoque-passeiosVendidos umEstoque)
				(estoque-utilitariosVendidos umEstoque)
				(estoque-caminhoesVendidos umEstoque)
  ) 
)

;;Função: veiculo-utilitario-produzido. Estoque, Veiculo-Utilitario -> Estoque
;;Objetivo: Dado um veículo produzido, atualiza a quantidade de veiculos utilitarios  em estoque
;;Exemplo: (estoque-numeroUtilitario (veiculo-utilitario-produzido (make-estoque 0 1 0 0 0 0)) -> 2
(define (veiculo-utilitario-produzido umEstoque) 
  (make-estoque (estoque-numeroPasseio umEstoque)
				(+ 1 (estoque-numeroUtilitario umEstoque))
				(estoque-numeroCaminhao umEstoque)
				(estoque-passeiosVendidos umEstoque)
				(estoque-utilitariosVendidos umEstoque)
				(estoque-caminhoesVendidos umEstoque)
  ) 
)

;;Função: veiculo-caminhao-produzido. Estoque, Veiculo-Caminhao -> Estoque
;;Objetivo: Dado um veículo produzido, atualiza a quantidade de veiculos caminhão  em estoque
;;Exemplo: (estoque-numeroCaminhao (veiculo-caminhao-produzido (make-estoque 0 0 1 0 0 0)) -> 2
(define (veiculo-caminhao-produzido umEstoque) 
  (make-estoque (estoque-numeroPasseio umEstoque)
				(estoque-numeroUtilitario umEstoque)
				(+ 1 (estoque-numeroCaminhao umEstoque))
				(estoque-passeiosVendidos umEstoque)
				(estoque-utilitariosVendidos umEstoque)
				(estoque-caminhoesVendidos umEstoque)
  ) 
)

;;Testes

(pode-carregar 3 100 caminhao) 
(not (pode-carregar 4 100 caminhao)) 
(not (pode-carregar 2 301 caminhao))

(string=? "Veiculo Sedan com cor Azul" (informacao passeio))
(string=? "Número de assentos: 5" (informacao utilitario)) 
(= 0 (informacao caminhao))
(= 5500 (informacao (make-veiculo-caminhao 'Azul 5500 20)))

(= 11 (estoque-numeroPasseio (programa 1 estoqueMontadora passeio)))
(= 21 (estoque-numeroUtilitario (programa 1 estoqueMontadora utilitario)))
(= 31 (estoque-numeroCaminhao (programa 1 estoqueMontadora caminhao)))

(= 9 (estoque-numeroPasseio (programa 2 estoqueMontadora passeio)))
(= 1 (estoque-passeiosVendidos (programa 2 estoqueMontadora passeio)))
(= 19 (estoque-numeroUtilitario (programa 2 estoqueMontadora utilitario)))
(= 1 (estoque-utilitariosVendidos (programa 2 estoqueMontadora utilitario)))
(= 29 (estoque-numeroCaminhao (programa 2 estoqueMontadora caminhao)))
(= 1 (estoque-caminhoesVendidos (programa 2 estoqueMontadora caminhao)))

(= 60 (total-estoque-emEstoque (programa 3 estoqueMontadora passeio)))
(= 5 (total-estoque-emEstoque (programa 3 (make-estoque 1 2 2 3 4 5) passeio)))
(= 12 (total-estoque-vendidos (programa 3 (make-estoque 1 2 2 3 4 5) passeio)))
