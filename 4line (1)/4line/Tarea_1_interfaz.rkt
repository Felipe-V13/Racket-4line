#lang racket
(require racket/include)
(require racket/gui/base)
(require (prefix-in htdp: 2htdp/image))
(require pdf-snip)
         



(require "Logica.rkt")


; ||============= Variables necesarias para el juego y sus respectivos set! ==========||


;Condición de que el juego comenzó
(define gameStart #f)

; Función que dice si el juego comenzó o no

(define (asignar_gameStart bool)
    (set! gameStart (cons bool gameStart)))

; Matriz del juego actual
(define matrix null)

; Función que asigna la matriz del juego en el momento
(define (asignar_matriz matriz)
    (set! matrix (cons matriz matrix)))

; ||============= Creación de widwets interfaz  ==========||

; Widget frame que represente la ventana
(define frame (new frame%
                   [label "4 en Línea"]
                   [width 350]
                   [height 300]))

; Mensaje para indicar al jugador que se le pide en los text fields
(new message% [parent frame][label "Por favor indique el número de filas y columnas deseado"])

; Textfield para las filas
(define filas_TF(new text-field% [parent frame] [label "          Filas:"]))

; Textfield para las columnas
(define columnas_TF(new text-field% [parent frame] [label "Columnas:"]))


(new button% [parent frame]
             [label "Presionar"]
             [callback (lambda (button event)                         (displayln "Botón presionado")                        )])




(new button% [parent frame]
             [label "Escribir"]
             [callback (lambda (button event)
                         ; Abre el archivo PDF
                         (define pdf-file (open-input-file "C:/Users/pipev/Downloads/4line(1)/4line/docu.pdf"))

                         ; Crea una nueva ventana
                         (define new-frame (new frame% [label "PDF"]
                                                      [width 500]
                                                      [height 500]))

                         ; Muestra el PDF en la ventana
                         (define pdf-canvas (new pdf% [parent new-frame]
                                                      [data pdf-file]
                                                      [show-controls #f]))
                         (send pdf-canvas fit-page)

                         ; Cierra el archivo PDF
                         (close-input-port pdf-file)

                         ; Muestra la nueva ventana
                         (send new-frame show #t)
                        )])

; Widget frame para la selección de ficha
;(define ficha_frame (new frame% [label "Selección de Ficha"]))

;(define ficha_panel (new horizontal-panel% [parent ficha_frame]))

; Textfield para los colores a escoger
;(define ficha_TF (new choice-field% [parent ficha_panel]
                   ; [label "Ficha a escoger:"]
                   ; [choices '("rosa.jpg" "cobre.jpg")]))
                    
; Botón para guardar la ficha seleccionada
;(new button% [parent ficha_panel]
  ;   [label "Guardar"]
     ; Callback procedure for a button click:
   ;  [callback (lambda (button event)
     ;            (preguntar_ficha))])



;Botón para crear el tablero e iniciar el juego
(new button% [parent frame]
             [label "Inicar"]
             [callback (lambda (button event)
                         (cond
                           [(< (string->number (send columnas_TF get-value)) 8) (new message% [parent frame][label "La cantidad de columnas debe ser mínimo 8"])]
                           [(< (string->number (send filas_TF get-value)) 8) (new message% [parent frame][label "La cantidad de filas debe ser mínimo 8"])]
                           [(> (string->number (send columnas_TF get-value)) 16) (new message% [parent frame][label "La cantidad de columnas debe ser máximo 16"])]
                           [(> (string->number (send filas_TF get-value)) 16) (new message% [parent frame][label "La cantidad de filas debe ser máximo 16"])]
                           ((and (not (false? (generateMatrx (string->number (send columnas_TF get-value)) (string->number (send filas_TF get-value))))) (not gameStart))
                            (crear_botones_columnas (string->number (send columnas_TF get-value)) 0)
                            (asignar_matriz (generateMatrx (string->number (send filas_TF get-value)) (string->number (send columnas_TF get-value))))
                            (crear_matriz_interfaz (car matrix))
                            (asignar_gameStart #t))
                           (else
                            (writeln #f))))])


;Definir el área de los botones de interfaz
(define panel_botones(new horizontal-panel%	 
   	 	[parent frame]	 
                [style '(border)]
                [alignment '(left top)]
   	 ))


;Definir el área de juego de interfaz
(define panel(new vertical-panel%	 
   	 	[parent frame]	 
                [style '(border)]
                [alignment '(left top)]
   	 ))




;Dialog box cuando el jugador gana
(define wonWindow (new dialog% [label "Ganaste"]
                       [parent frame]
                       [width 300]	 

       )
)

; Función para mostrar la ventana que ganó
(define (showWonWindow)
  
   (send wonWindow show #t)
  
 )

;Dialog box cuando el jugador gana
(define lossWindow (new dialog% [label "Perdiste"]
                       [parent frame]
                       [width 300]	 

       )
)

; Función para mostrar la ventana que ganó
(define (showLossWindow)
  
   (send lossWindow show #t)
  
 )

;Dialog box cuando el jugador gana
(define tieWindow (new dialog% [label "Empate"]
                       [parent frame]
                       [width 300]	 

       )
)

; Función para mostrar la ventana que ganó
(define (showTieWindow)
  
   (send tieWindow show #t)
  
 )

;Funcion que realiza la conexion con el algoritmo codicioso. Retorna un numero de columna donde colocar la ficha
(define(colPC)
   (solucion
    (objetivo
    (viabilidad
     (buscar
      (selec (traspuesta (car matrix) ) 0 '() null)
      (maximo (selec (traspuesta (car matrix) ) 0 '() null)) '()) (car matrix) '()) (car matrix)  )))


;Funcion que usa el retorno de colPC para mostrar la jugada de la PC
(define(playPC)
  (cond
    ((equal? (play 2 (colPC) (car matrix)) 1) (showWonWindow))
    ((equal? (play 2 (colPC) (car matrix)) 2) (showLossWindow))
    ((equal? (play 2 (colPC) (car matrix)) 0) (showTieWindow))
    (else
     (printf "turno pc\n")
     (define candidatos (candi  matrix))
     (asignar_matriz (play 2 (colPC) (car matrix)))
     (refrescar_tablero)
     (crear_matriz_interfaz (car matrix))
)))


; Hace el espacio que representa fichas dependiendo del numero que toque (0 es vacío, 1 es ficha jugador, 2 ficha máquina)

(define (crear_espacio num panelx)
  (if(= num 0)
     (new message% [parent panelx]
            [label (read-bitmap "plata1.jpg" )])
     #f)
  (if(= num 1)
     (new message% [parent panelx]
            [label (read-bitmap "rubi1.jpg" )])
     #f)
  (if(= num 2)
     (new message% [parent panelx]
            [label (read-bitmap "rosa.jpg" )])
     #f))
  



;Crea un widget que representa una fila de la matriz en la interfaz
(define (crear_fila_mat_widget)
   (new horizontal-panel%	 
   	 	[parent panel]	 
                [style '(border)]
                [alignment '(left top)]))

;Logica crear fila matriz interfaz
(define (crear_fila_matriz_interfaz fila widget)
  (cond
    ((null? fila ) 1)
    (else
     (crear_espacio (car fila) widget)
     (crear_fila_matriz_interfaz (cdr fila) widget))))

; Esta función se encarga de crear toda la matriz en la interfaz
;Logica crear matriz interfaz

(define(crear_matriz_interfaz matriz)
  (cond
    ((null? matriz ) 1)
    ((not(list? matriz)) #t) 
    (else
     (crear_fila_matriz_interfaz (car matriz) (crear_fila_mat_widget))
     (crear_matriz_interfaz (cdr matriz)))))

;Función que crea n botones para las columnas del juego en la interfaz

(define (crear_botones_columnas num cont)
  (cond
    ((number? num)
     (cond
       ((= num 0) 1)
       (else
        (crear_boton cont )
        (crear_botones_columnas (- num 1) (+ cont 1)))))))


;Función que reinicia el tablero de juego en la interfaz, cada vez que se hace un movimiento

(define (refrescar_tablero)
    (send frame delete-child panel)
     (set! panel (new vertical-panel%	 
   	 	[parent frame]	 
                [style '(border)]
                [alignment '(left top)]
   	 )

     )
)

; ||============= Funciones que afectan la interfaz  ==========||

; Hace un botón para tirar una ficha en el juego
(define (crear_boton col)
  (new button% [parent panel_botones]
             [label (number->string col)]
             [horiz-margin 1]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (printf "turno jug\n")
                         (writeln (play 1 col (car matrix)))
                         (cond
                           ;Si la función jugar devuelve un string es que el jugador ya ganó
                           ((equal? (play 1 col (car matrix)) 1) (showWonWindow))
                           ((equal? (play 1 col (car matrix)) 2) (showLossWindow))
                           ((equal? (play 1 col (car matrix)) 0) (showTieWindow))
                           ((equal? (play 1 col (car matrix)) (car matrix)) (send button enable #f))
                           (else
                            (asignar_matriz (play 1 col (car matrix)))
                            (refrescar_tablero)
                            (crear_matriz_interfaz (car matrix))
                            (playPC)
                            (writeln (play 1 col (car matrix)))
                            )))]))
         

(send frame show #t)