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

(define socialnetworkActualizado (lambda(snName snDate fn1 fn2 users posts usersActivity)
                                   (list snName snDate fn1 fn2 users posts usersActivity)
  ))

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

(define sonPublicaciones? (lambda (lista_publicaciones)
                            (if(null? lista_publicaciones)#t
                               (and (es_PublicacionValida? (car lista_publicaciones)) (es_PublicacionValida? (cdr lista_publicaciones)))
                               )
                            )
  ); pendiente


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


;descr:funcion que agrega elementos a la cola de la lista
;dom: list x list
;rec: list
;recursion: natural
(define agregar_cola (lambda(lista lista_a_agregar)
                            (if(empty? lista)
                               lista_a_agregar
                               (cons (car lista) (agregar_cola (cdr lista) lista_a_agregar))
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

;; Descripción: Función que aplica una función fn a todos y cada uno de los elementos
;;              de una lista lst
;; Dom: Función a aplicar a una colección de datos <función>,
;;      Lista de datos <lista>
;; Rec: Nueva lista con cada elemento
;; Tipo de recursión: Natural
;; Referencia: funcion vista en clase del profesor Gonzalo Martinez
(define (mi-map funcion lista)
  (cond
    [(null? lista) null]
    [else (cons (funcion (car lista))
                (mi-map funcion (cdr lista)))]))

;Descripcion: Funcion que aplica el conector logico "and" a cada uno de los elementos de una lista
;Dom: list
;Rec: boolean
;Recursion: natural

(define mi-andmap(lambda(lista)
              (if (null? lista) #t   
             (and(car lista)(mi-andmap (cdr lista)))
                   )
  ))

;Descripcion: Funcion que verifica si un item se encuentra en la lista
;Dom: string/number X list
;Rec: boolean
;Recursion: NA
;Referencia: funcion vista en clase del profesor Gonzalo Martinez
(define member?
  (lambda (item lst)
    (cond
      [(null? lst) #f]
      [(equal? item (car lst)) #t]
      [else (member? item (cdr lst))])))
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;-----------------------------TDA USER-------------------------------------

;user(sesionActiva,idUser,username,password,date,amigos,followers)
;donde amigos = (usernarme1,username2,....,usernameN)
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
                       (listaAmigos_valida? amigos)
                        ) 
                       (list sesionActiva userID username password date amigos '())
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
                        (date? (getUser_date usuario))
                        (listaAmigos_valida? (getUser_amigos usuario))) #t
                         #f)))
                   


  
(define listaAmigos_valida? (lambda (lista_amigos)
                              (if(empty? lista_amigos)#t
                                 (and(esString? (car lista_amigos))(listaAmigos_valida? (cdr lista_amigos)))
                                 )
                              ))
 
(define existe-usuario? (lambda(nombreUsuarioAVerificar contraseñaUsuario)
                          (lambda(usuario)
                            (and(eqv? nombreUsuarioAVerificar (getUser_username usuario)) (eqv? contraseñaUsuario (getUser_password usuario))))))

(define sesion-activa? (lambda (usuario)
                         (eqv? #t (getUser_sesionActiva usuario))
                         ))

(define son-amigos? (lambda(lista-amigos-usuario)
                      (lambda(personaAVerificar)
                      (member? personaAVerificar lista-amigos-usuario)
                      )))

#|ESTO ES LA CONDICION ANTES DE HACER UN POST
->TIENE QUE HABER UN USER QUE SESION INICIADA
->LAS PERSONAS TIENEN QUE ENCONTRARSE EN SU LISTA DE AMIGOS
|#
;asumiendo que se entrega como parametro de entrada la lista de personas a verificar si son amigos -> users
;y que los amigos fueron agregados anteriormente
(if(not(null? (getUser_sesionIniciada(get_snUsuario socialnetwork))))
   (if(not(null? users)) ;significa que el post va a ir dirigido a usuarios
      (if(mi-andmap(mi-map
                    (son_amigos? (getUser_amigos(getUser_sesionIniciada(get_snUsuarios socialnetwork)))) users))
         "son amigos y se puede ejecutar la funcion"
         "las personas ingresadas no se encuentran en la lista de amigos del user actual-> retornar socialnetwork"
         )
      "el post va dirigido al mismo usuario"
      )
   ;si no se cumple la condicion, retorna el mismo social network
   socialnetwork)

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

(define getUser_followers (lambda(usuario)
                    (car(cdr(cdr(cdr(cdr(cdr (cdr usuario)))))
                    ))))



(define getUsers_lastUser(lambda(usuarios)
                          (car(reverse usuarios))
                          )
  )

(define getUsers_lastID (lambda(usuarios)
                         (getUser_idUser(getUsers_lastUser usuarios ))
                         )
  )

(define getUser_sesionIniciada (lambda(usuarios)
                                (if (null? usuarios) '()
                               (if(eqv? #t (getUser_sesionActiva(car usuarios)))
                                  (car usuarios)
                                  (getUser_sesionIniciada (cdr usuarios))
                               )
                                )))


;---------------------------------OTRAS FUNCIONES----------------------------
(define agregar_amigo(lambda(nombreUsuario lista_usuarios lista_amigos)
                       (if(eqv? nombreUsuario (getUser_username (car lista_usuarios)))
                          (cons(user
                                (getUser_sesionActiva (car lista_usuarios))
                                (getUser_idUser (car lista_usuarios))
                                (getUser_username(car lista_usuarios))
                                (getUser_password(car lista_usuarios))
                                (getUser_date(car lista_usuarios))
                                (agregar_cola (getUser_amigos (car lista_usuarios)) lista_amigos) 
                                )
                                (cdr lista_usuarios))

                          (cons (car lista_usuarios)(agregar_amigo nombreUsuario (cdr lista_usuarios) lista_amigos))
                          )))
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;------------------------------TDA PUBLICACION------------------------------
;publicacion(idPost,date,autorPost,tipoPublicacion,contenido,reacciones,comments)
;donde contenido = mensaje encriptado
;reacciones (username1,username2,...usernameN) -> usernamei = string

;------------------------------CONSTRUCTOR----------------------------------
;Descripcion: funcion que crea una publicacion
;Dom: int X date X string X string X string X list X comment
;Rec: publicacion
;Recursion: NA

(define iniciarPublicacion(lambda(idPost date autorPost tipoPublicacion contenido)
                               (if(and(esNumero? idPost) (date? date) (esString? autorPost) (esString? tipoPublicacion) (esString? contenido))
                                   (list idPost date autorPost tipoPublicacion contenido '() '())
                                  '())
                                )

  )

(define publicacion(lambda(idPost date autorPost tipoPublicacion contenido reacciones comments)
                     (list idPost date autorPost tipoPublicacion contenido reacciones comments)
                     ))


;------------------------------SELECTORES----------------------------------
(define getPost_id car)
(define getPost_date cadr)
(define getPost_autor caddr)
(define getPost_tipoPublicacion cadddr)
(define getPost_contenido (lambda(p)(car(cdr(cdr(cdr(cdr p)))))))
(define getPost_reacciones(lambda(p)(car(cdr(cdr(cdr(cdr(cdr p))))))))
(define getPost_comments (lambda(p)(car(cdr(cdr(cdr(cdr(cdr(cdr p)))))))))


(define getPost_lastPost(lambda(publicaciones)
                          (car(reverse publicaciones))
                          )
  )

(define getPost_lastID (lambda(publicaciones)
                         (getPost_id(getPost_lastPost publicaciones))
                         )
  )
;------------------------------PERTENENCIA----------------------------------
(define es_PublicacionValida? (lambda(publicacion)
                                (and(esNumero? (getPost_id publicacion))
                                    (date? (getPost_id publicacion))
                                    (esString? (getPost_id publicacion))
                                    (esString? (getPost_id publicacion))
                                    (esString? (getPost_id publicacion))
                                    (list? (getPost_id publicacion))
                                    (comment? (getPost_id publicacion))
                                 )
                                ))
;------------------------------MODIFICADORES----------------------------------

;------------------------------OTRAS FUNCIONES----------------------------------


;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
;------------------------------TDA COMMENT----------------------------------
;comment(idPost,date,contenido,((autorComentario1,date,contenido),(autorComentario2,date,contenido),....,(autorComentarioN,date,contenido))

;---------------------------------------------------------------------------
#|
(define usuario1(user_inicializado "user1" "pass1" (date 0 0 0) ))
usuario1
(define usuario2(user_inicializado "user2" "pass1" (date 0 0 0) ))
usuario2

(define usuarios (list usuario1 usuario2))
usuarios
;(set_sesionActiva_True "user1" usuarios)

(agregar_amigo "user1"(agregar_amigo "user1" usuarios (list "user2" "user3")) (list "user4" "user5"))
|#
;------------------------------------------------------------------------
;DEFINITIVA
;TDA PARTICIPACION POST
;donde R: valor actual que tienen las participaciones
(define agregarNuevaParticipacion(lambda(usuarios idPost R)
                                   (if(empty? usuarios)R
                                      (agregarNuevaParticipacion (cdr usuarios) idPost (cons (list (car usuarios) idPost) R))
                                      )))

;--------------------------FUNCIONES OBLIGATORIAS--------------------------------------

#|
(define usuario1(user_inicializado "user1" "pass1" (date 0 0 0) ))
usuario1
(define usuario2(user_inicializado "user2" "pass1" (date 0 0 0) ))
usuario2

(define usuarios (list usuario1 usuario2))
usuarios
;(set_sesionActiva_True "user1" usuarios)

(agregar_amigo "user1"(agregar_amigo "user1" usuarios (list "user2" "user3")) (list "user4" "user5"))
|#


;--------------------PRUEBAS FUNCIONES OBLIGATORIAS-------------------------------------
;(define emptyFB (socialnetwork "fb" (date 25 10 2021) encryptFn encryptFn))
#|(define accionRegistrar(register (register (register emptyFB (date 25 10 2021) "user1" "pass1") (date 25 10 2021) "user2"
"pass2") (date 25 10 2021) "user3" "pass3"))
|#

;(login accionRegistrar "user2" "pass2" funcion)

;(define accionLogin (login accionRegistrar "user2" "pass2" funcion))
;(getUser_sesionIniciada(get_snUsuarios accionLogin))



;--------------------------REGISTER----------------------------------------------------
;Descripcion: funcion que registra a un nuevo usuario en la red social
;Dom: socialnetwork X date X string X string
;Rec: socialnetwork
;encabezado = (register socialnetwork date username password)


(define register(lambda(socialnetwork date username password)
                   (if(and(esRedSocial? socialnetwork) (date? date) (esString? username) (esString? password))
                      (if(null? (my-filter (existe-usuario? username password) (get_snUsuarios socialnetwork)))
                         (if (null? (get_snUsuarios socialnetwork))
                             ;primero usuario en registrarse
                             (socialnetworkActualizado (get_snName socialnetwork)
                                                   (get_snDate socialnetwork)
                                                   (get_encryptFn socialnetwork)
                                                   (get_decryptFn socialnetwork)
                                                   (agregar_cola (get_snUsuarios socialnetwork)(list(user #f 0 username password date '())))
                                                   (get_snPublicaciones socialnetwork)
                                                   (get_snGente_que_participo_en_post socialnetwork))
                             ;ya existe 1 o mas usuarios
                             (socialnetworkActualizado (get_snName socialnetwork)
                                                   (get_snDate socialnetwork)
                                                   (get_encryptFn socialnetwork)
                                                   (get_decryptFn socialnetwork)
                                                   (agregar_cola (get_snUsuarios socialnetwork)
                                                                 (list(user #f
                                                                       (+ 1 (getUsers_lastID (get_snUsuarios socialnetwork)))
                                                                       username
                                                                       password
                                                                       date
                                                                       '())))
                                                   (get_snPublicaciones socialnetwork)
                                                   (get_snGente_que_participo_en_post socialnetwork))
                             


                             )
                       
                         
                         "este usuario ya existe"



                         )
                      "parametro(s) no valido(s)"
                      )
                    )

  )

;--------------------------LOGIN----------------------------------------------------
;Descripcion: Función que permite autenticar a un usuario registrado iniciar sesión
;y junto con ello permite la ejecución de comandos concretos dentro de la red social.

;Dom:socialnetwork X string X string X function
;Rec:function
;Encabezado = (login socialnetwork username password operation)

(define login (lambda(socialnetwork username password operation)
                (if (and (esRedSocial? socialnetwork) (esString? username) (esString? password) (funcion? operation))
                    
                (if (not(null? (my-filter (existe-usuario? username password) (get_snUsuarios socialnetwork))))
                    (operation (socialnetworkActualizado (get_snName socialnetwork)
                                                   (get_snDate socialnetwork)
                                                   (get_encryptFn socialnetwork)
                                                   (get_decryptFn socialnetwork)
                                                   (set_sesionActiva_True username (get_snUsuarios socialnetwork))
                                                   (get_snPublicaciones socialnetwork)
                                                   (get_snGente_que_participo_en_post socialnetwork)))
                    operation
                )
                "parametros no validos")))

;--------------------------POST----------------------------------------------------
;(define funcion(lambda(sn)sn))
