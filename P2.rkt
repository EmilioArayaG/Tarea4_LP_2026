#lang scheme
;; crear-gusano
;; Esta funcion genera una clausura, retorna un procedimiento hecgo para mapear una estructura de un arbol n-ario y reemplazar los nodos que no cumplan con X
;; Parametro:
;; 1. vulnerabilidad: procedimiento que retorna #t o #f
;; Retorno: un procedimiento lambda que acepta un argumento
(define(crear-gusano vulnerabilidad)
  (lambda (memoria)
    (define (recorrer-arbol nodo)
      (cond
       ((null? nodo)'())
       ((not(pair? nodo))
        (if (vulnerabilidad nodo)
            nodo
            'X))
       (else
        (cons (recorrer-arbol(car nodo))
              (recorrer-arbol(cdr nodo))))))
    (recorrer-arbol memoria)))
