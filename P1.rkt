#lang scheme
;; 1. Recursion simple
;; Esta funcion que descifra los puertos del ICE aplicando una funcion matematica a cada uno mediante recursion simple
;; Parametros:
;; 1.daemon: funcion de un argumento con el que desencriptaremos un valor numerico
;; 2.puerots: lista simple de numeros encriptados
;; Retorno: Una lista que mantiene el orden, con los puertos desencriptados
(define (icebreaker-simple daemon puertos)
  (if(null? puertos)
     '()
     (cons (daemon(car puertos))
           (icebreaker-simple daemon (cdr puertos)))))
;; 2. Recursion de cola
;; invertir-lista
;; Esta funcion auxiliar se encarga de invertir los elementos de una lista usando recursion de cola
;; Parametros:
;; 1. lista: lista que sera invertida
;; 2. i: lista acumuladora que construye el resultado
;; Retorno: lista con los elementos en orden inverso
(define (invertir-lista lista i)
  (if(null? lista)
     i
     (invertir-lista (cdr lista)(cons (car lista) i))))
;; auxiliar-cola
;; Esta funcion auxiliar  aplica un procedimiento a una lista iterando con recursion de cola
;; Parametros:
;; 1. p: procedimiento a evaluar sobre cada elemento
;; 2. l: lista de elementos restantes por procesar
;; 3. it: lista con los resultados parciales acomulados
;; Retorno: lista con los resultados evaluados y en el orden original
(define (auxiliar-cola p l it)
  (if (null? l)
      (invertir-lista it '())
      (auxiliar-cola p (cdr l)(cons (p(car l)))it)))
;; icebreaker-cola
;; Esta funcion mapea un procedimiento sobre los elementos de una lista usando recursion de cola
;; Parametros:
;; 1. daemon: procedimiento a aplicar
;; 2. puertos: lista de numeros a procesar
;; Retorno: lista con los resultados finales tras el procedimiento
(define(icebreaker-cola daemon puertos)
  (auxiliar-cola daemon puertos '()))
