#lang scheme
;; hash-deconexion
;; Esta funcion se encarga de procesar un token para extraer su valor numerico con funciones de orden superior para filtrar, mapear y reducir los datos
;; Parametros:
;; 1. token: cadena de texto que representa la clave interceptada
;; Retorno: Numero entero correspondiente a la suma de valores ASCII de las tareas.
(define (hash-desconexion token)
  (foldl + 0 
         (map char->integer 
              (filter char-alphabetic? 
                      (string->list token)))))