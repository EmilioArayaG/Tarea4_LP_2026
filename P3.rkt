#lang scheme
;; ejecutor-casacada
;; Esta funcion navega un arbol n-ario sacando de la estructura X y mutando los nodos numericos mediante procedimientos
;; Parametros:
;; 1. mapa: lista n-aria estructurada con numeros y simbolos X
;; 2. daemons: lista de procedimientos a ejecutar sobre los numeros
;; Retorno: Nueva lista conservando clusters, sin simbolos X y con los nodos actualizados
(define (ejecutor-cascada mapa daemons)
  (define (aplicar-daemons valor lista-d)
    (if (null? lista-d)
        valor
        (aplicar-daemons ((car lista-d) valor) (cdr lista-d))))

  (cond
    ((null? mapa) '())
    ((list? (car mapa))
     (cons (ejecutor-cascada (car mapa) daemons)
           (ejecutor-cascada (cdr mapa) daemons)))
    ((eq? (car mapa) 'X)
     (ejecutor-cascada (cdr mapa) daemons))
    (else
     (cons (aplicar-daemons (car mapa) daemons)
           (ejecutor-cascada (cdr mapa) daemons)))))