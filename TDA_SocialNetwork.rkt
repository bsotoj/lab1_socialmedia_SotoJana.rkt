#lang racket
;
;Descrip:
;Dom:
;Rec:
;Recursion:

;TDA Date
;date(dia,mes,año)

;constructor
;Descr: Funcion que genera una fecha
;Dom: Dia <int> X mes <int> X año <int>
;Rec: date
;Recursion: NA
(define date(lambda (dd mm yyyy) (list dd mm yyyy)))

;Pertenencia
;Descr: Funcion que verifica si los argumentos cumplen el formato fecha
;Dom: date
;Rec: Bool
;Recursion: NA
(define date? (lambda(fecha)
              (if(and(number? (getDay fecha))(getMonth fecha)(getYear fecha))
                 #t
                 #f
                 )))

;Selectores
(define getDay car)
(define getMonth cadr)
(define getYear caddr)




;Funcion encryptFn
;Descr: que permite encriptar/desencriptar un mensaje
;Dom: String
;Rec: String
;Recursion: NA 
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))
(define decryptFn (lambda (s) (list->string (reverse (string->list s)))))


;TDA SocialNetwork
;(name, date, encryptFn decryptFn,usuarios, publicaciones,gente_que_participo_en_post)


;Constructor
;Dom: String X Date X EncryptFunction X DecryptFunction
;Rec: SocialNetwork

(define socialnetwork(lambda(name date encryptFn decryptFn)
                       (if(and(esString? name)(date? date)(funcion? encryptFn)(funcion? decryptFn))
                          (list name date encryptFn decryptFn '() '() '())
                          '()
                          )
                       )
  )

;----------------------------------PERTENENCIA-----------------------
(define esString?(lambda(palabra)
                  (if (string? palabra) #t
                      #f)
                  )
  )

(define funcion? (lambda (funcion)
                   (if (procedure? funcion) #t
                       #f
                       )
                   )
  )

(define esNumero?(lambda(numero)
                  (if (number? numero) #t
                      #f)
                  )
  )

(define esRedSocial? (lambda(socialN)
                       (if(and
                           (esString? (get_snName socialN))
                           (date? (get_snDate socialN))
                           (funcion? (get_encryptFn socialN))
                           (funcion? (get_decryptFn socialN))
                           (son_UsuariosValidos? (get_snUsuarios socialN))
                           (sonPublicaciones? (get_snPublicaciones socialN))
                           ) #t
                             #f)
 
                       )

  )

(define son_UsuariosValidos?(lambda(lista_usuarios)
                              (if (null? lista_usuarios)#t
                                  (and (es_UserValido? (car lista_usuarios)) (son_UsuariosValidos? (cdr lista_usuarios)))

                                  )))

(define sonPublicaciones? (lambda (x)#t)); pendiente


;-------------------------------SELECTORES-------------------------------
(define get_snName car)
(define get_snDate cadr)
(define get_encryptFn caddr)
(define get_decryptFn cadddr)
(define get_snUsuarios (lambda(sn)(car(cdr(cdr(cdr(cdr sn)))))))
(define get_snPublicaciones(lambda(sn)(car(cdr(cdr(cdr(cdr(cdr sn))))))))
(define get_snGente_que_participo_en_post(lambda(sn)(car(cdr(cdr(cdr(cdr(cdr(cdr sn)))))))))

;-----------------------------OTRAS FUNCIONES-----------------------------


;descr:funcion que agrega un elemento en la cabeza de la lista
;dom: list x number/string/list
;rec: list
;recursion: NA
;referencia: funcion vista en clase del profesor Gonzalo Martinez

(define agregar_cabeza
  (lambda (lista elemento)
    (cons elemento lista)))


;descr:funcion que agrega un elemento en la cola de la lista
;dom: list x number/string/list
;rec: list
;recursion: NA
;referencia: funcion vista en clase del profesor Gonzalo Martinez

(define agregar_cola
  (lambda (lista elemento)
    (reverse (cons elemento (reverse lista)))))

;descr:funcion que agrega uno a uno los elementos de una lista a otra
;dom: list x list
;rec: list
;recursion: natural
(define agregar_uno_a_uno (lambda(lista lista_a_agregar)

                            (if(empty? lista)
                               lista_a_agregar
                               (cons (car lista) (agregar_uno_a_uno (cdr lista) lista_a_agregar))
                               )
                            ))


;descr: funcion que permite hacer un filtrado a partir de un predicado
;dom: predicado X list
;rec: list
;;Referencia: funcion vista en clase del profesor Gonzalo Martinez

(define (my-filter pred lst)
  (cond
    [(empty? lst) '()] ;null? null
    [(pred (car lst))  ;; si predicado es verdero, ejecuto la siguiente linea, caso contrario, ejecuto else
     (cons (car lst) (my-filter pred (cdr lst)))]
    [else (my-filter pred (cdr lst))]))


;-----------------------------TDA USER-------------------------------------

;user(sesionActiva,idUser,username,password,date,amigos)
;donde amigos = (user1,user2,....,userN)
;----------------------------------CONSTRUCTORES--------------------------------
;
;Descrip:funcion que crea un user
;Dom: sesionActiva <bool> X userID <number> X username <string> X password <string> X date X amigos
;Rec:user
;Recursion: NA

(define user(lambda(sesionActiva userID username password date amigos)
              (if (and (boolean? sesionActiva)
                       (esNumero? userID)
                       (esString? username)
                       (esString? password)
                       (date? date)
                       ;(son_AmigosValidos? amigos)
                        ) 
                       (list sesionActiva userID username password date amigos)
                  '()
                  )
              )
 
  )

(define user_inicializado(lambda(username password date)
              (if (and  (esString? username) (esString? password) (date? date)) 
                  (list #f 0 username password date '())
                  '()
                  )
              )
 
  )

;-------------------------------------PERTENENCIA-----------------------------
(define es_UserValido? (lambda(usuario)
                  (if
                    (and(boolean? (getUser_sesionActiva usuario))
                        (esNumero? (getUser_idUser usuario))
                        (esString?(getUser_username usuario))
                        (esString? (getUser_password usuario))
                        (date? (getUser_date usuario))) #t
                         #f)))
                   


(define son_AmigosValidos? (lambda(lista_amigos)
                             (if (null? lista_amigos)#t
                                 (and (esString? (car lista_amigos)) (son_AmigosValidos? (cdr lista_amigos)))
                                 )
                             )
  )
#|
(define publicaciones_que_participa_Validas?(lambda (lista_PParticipa)
                                              (if (null? lista_PParticipa)#t
                                                  (and (esNumero? (car lista_PParticipa))
                                                       (publicaciones_que_participa_Validas? (cdr lista_PParticipa)))
                                                  )
                                              )
  )
|#                                              
;-----------------------------------MODIFICADORES-------------------------
(define set_sesionActiva_True (lambda(username lista_usuarios)
                                (if (eqv? username (getUser_username(car lista_usuarios)))
                                    ;caso base
                                    (cons (user
                                           #t
                                          (getUser_idUser (car lista_usuarios))
                                          (getUser_username(car lista_usuarios))
                                          (getUser_password(car lista_usuarios))
                                          (getUser_date(car lista_usuarios))
                                          (getUser_amigos(car lista_usuarios))
                                          )
                                          (cdr lista_usuarios))
                                    ;caso recursivo
                                    (cons (car lista_usuarios)(set_sesionActiva_True username (cdr lista_usuarios)))
                                    
                                )
                                
  ))
       
(define set_sesionActiva_False (lambda(username lista_usuarios)
                                (if (eqv? username (getUser_username(car lista_usuarios)))
                                    ;caso base
                                    (cons (user
                                           #f
                                          (getUser_idUser (car lista_usuarios))
                                          (getUser_username(car lista_usuarios))
                                          (getUser_password(car lista_usuarios))
                                          (getUser_date(car lista_usuarios))
                                          (getUser_amigos(car lista_usuarios))
                                          )
                                          (cdr lista_usuarios))
                                    ;caso recursivo
                                    (cons (car lista_usuarios)(set_sesionActiva_False username (cdr lista_usuarios)))
                                    
                                )
                                
  ))
  
                           

;---------------------------------SELECTORES----------------------------
(define getUser_sesionActiva car)
(define getUser_idUser cadr)
(define getUser_username caddr)
(define getUser_password cadddr)
(define getUser_date (lambda(usuario)
                    (car(cdr(cdr(cdr(cdr usuario)))))
                    ))

(define getUser_amigos (lambda(usuario)
                    (car(cdr(cdr(cdr(cdr (cdr usuario)))))
                    )))


(define getUsers_lastUser(lambda(usuario)
                          (car(reverse usuario))
                          )
  )

(define getUsers_lastID (lambda(usuario)
                         (getUser_idUser(getUsers_lastUser usuario ))
                         )
  )


(define son_amigos? (lambda(amigo lista_amigos)
                      (if (null? lista_amigos) #f
                          (or (eqv? amigo (car lista_amigos))(son_amigos? amigo (cdr lista_amigos)))
                          )
                      ))
;(define friends (list "pepe" "carlos" "andres"))
;(son_amigos? "carlos" lista_amigos)

(define amigos_user? (lambda(amigos_usuario personas)
                       (if (null? personas)#t
                           (and (son_amigos? (car personas) amigos_usuario) (amigos_user? amigos_usuario (cdr personas)))
                           )
                       ))
;---------------------------------OTRAS FUNCIONES----------------------------
(define agregar_amigo(lambda(nombreUsuario lista_usuarios lista_amigos)
                       (if(eqv? nombreUsuario (getUser_username (car lista_usuarios)))
                          (cons(user
                                (getUser_sesionActiva (car lista_usuarios))
                                (getUser_idUser (car lista_usuarios))
                                (getUser_username(car lista_usuarios))
                                (getUser_password(car lista_usuarios))
                                (getUser_date(car lista_usuarios))
                                (agregar_uno_a_uno (getUser_amigos (car lista_usuarios)) lista_amigos) 
                                )
                                (cdr lista_usuarios))

                          (cons (car lista_usuarios)(agregar_amigo nombreUsuario (cdr lista_usuarios) lista_amigos))
                          )))



(define usuario1(user_inicializado "user1" "pass1" (date 0 0 0) ))
usuario1
(define usuario2(user_inicializado "user2" "pass1" (date 0 0 0) ))
usuario2

(define usuarios (list usuario1 usuario2))
usuarios
;(set_sesionActiva_True "user1" usuarios)

(agregar_amigo "user1"(agregar_amigo "user1" usuarios (list "user2" "user3")) (list "user4" "user5"))

;------------------------------------------------------------------------
;DEFINITIVA
;TDA PARTICIPACION POST
(define agregarNuevaParticipacion(lambda(usuarios idPost R)
                                   (if(empty? usuarios)R
                                      (agregarNuevaParticipacion (cdr usuarios) idPost (cons (list (car usuarios) idPost) R))
                                      )))

