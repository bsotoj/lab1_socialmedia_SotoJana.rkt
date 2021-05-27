#lang racket

(require "TDA_SocialNetwork.rkt")


;--------------------------FUNCIONES OBLIGATORIAS--------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------REGISTER----------------------------------------------------
;Descripcion: funcion que registra a un nuevo usuario en la red social
;Dom: socialnetwork X date X string X string
;Rec: socialnetwork
;encabezado = (register socialnetwork date username password)
;recursion: natural

(define register(lambda(socialnetwork date username password)
                   (if(and(esRedSocial? socialnetwork) (date? date) (esString? username) (esString? password))
                      (if(null? (my-filter (existe-usuario? username password) (get_snUsuarios socialnetwork)))
                         (if (null? (get_snUsuarios socialnetwork))
                             ;primero usuario en registrarse
                             (socialnetworkActualizado (get_snName socialnetwork)
                                                   (get_snDate socialnetwork)
                                                   (get_encryptFn socialnetwork)
                                                   (get_decryptFn socialnetwork)
                                                   (agregar_cola (get_snUsuarios socialnetwork)(list(user #f 0 username password date '() '())))
                                                   (get_snPublicaciones socialnetwork)
                                                   (get_snGente_que_participo_en_post socialnetwork)
                                                   (get_snPosts_Compartidos socialnetwork)
                                                   )
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
                                                                       '()
                                                                       '())))
                                                   (get_snPublicaciones socialnetwork)
                                                   (get_snGente_que_participo_en_post socialnetwork)
                                                   (get_snPosts_Compartidos socialnetwork)
                                                   )
                             


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
;esta hace uso de funciones de orden superior y recursion natural a traves de la funcion my-filter

;Dom:socialnetwork X string X string X function
;Rec:function
;Encabezado = (login socialnetwork username password operation)
;recursion: natural

(define login (lambda(socialnetwork username password operation)
                (if (and (esRedSocial? socialnetwork) (esString? username) (esString? password) (funcion? operation))
                    
                (if (not(null? (my-filter (existe-usuario? username password) (get_snUsuarios socialnetwork))))
                    (operation (socialnetworkActualizado (get_snName socialnetwork)
                                                   (get_snDate socialnetwork)
                                                   (get_encryptFn socialnetwork)
                                                   (get_decryptFn socialnetwork)
                                                   (set_sesionActiva_True username (get_snUsuarios socialnetwork))
                                                   (get_snPublicaciones socialnetwork)
                                                   (get_snGente_que_participo_en_post socialnetwork)
                                                   (get_snPosts_Compartidos socialnetwork)
                                                   ))
                    operation
                )
                "parametros no validos")))

;--------------------------POST----------------------------------------------------
;DADA LA INCONGRUENCIA QUE HABIA EN EL ENUNCIADO RESPECTO AL ARGUMENTO "tipo de publicacio" EN DONDE
;SE MENCIONABA QUE EL POST DEBIA INCLUIRLO PERO EN EL EJEMPLO DE APLICACION NO SE UTILIZA
;SE OPTA POR SEGUIR LO SEGUNDO, ESTO TAMPOCO AFECTA A LA FUNCION POST YA QUE "tipo de publicaciones"
;SOLO INDICA SI EL CONTENIDO ES UNA FOTO,VIDEO,URL,TEXTO O AUDIO, PERO EL CONTENIDO (content) SIGUE SIENDO
;UN STRING, POR LO CUAL NO SE CONSIDERO PROBLEMA EL NO INCLUIR "tipo de publicacion".




;Descripcion: Función currificada que permite a un usuario con sesión iniciada en
;la plataforma realizar una nueva publicación propia o dirigida a otros usuarios
;Dom: socialnetwork
;recFinal: socialnetwork
;recursion: natural
                                  
(define post(lambda (sn)
              (lambda(date)(lambda(content . users)
              (if(and(esRedSocial? sn) (date? date) (esString? content))
              ;se verifica que exista un usuario con sesion iniciada
              (if(not(null? (getUser_sesionIniciada(get_snUsuarios sn))))
                 ;esta condicion verifica si el post es dirigido a otros usuarios
                 ;null = post dirigido a mismo usuario
                 ;not null =  post dirigido a otros usuarios incluyendo al que lo creo
                 (if(not(null? users))
                    ;ahora se verifica si los usuarios ingresados pertenecen a la lista de amigos de la persona que realiza
                    ;el post
                    (if(mi-andmap(mi-map
                                  (son-amigos? (getUser_amigos(getUser_sesionIniciada(get_snUsuarios sn)))) users ))
                       ;es el primer post que se hace en la red social?           
                       (if(null? (get_snPublicaciones sn))        
                          (socialnetworkActualizado
                           (get_snName sn)
                           (get_snDate sn)
                           (get_encryptFn sn)
                           (get_decryptFn sn)
                           (set_sesionActiva_False (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) (get_snUsuarios sn))
                           (sn-agregarPublicacion (inicializarPublicacion 0 date (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) ((get_encryptFn sn)content))          
                                                  (get_snPublicaciones sn))
                           (agregarNuevaParticipacion 0 (get_snGente_que_participo_en_post sn) users  )
                           (get_snPosts_Compartidos sn)
                           )
                       ;existe una o mas publicaciones
                           (socialnetworkActualizado
                           (get_snName sn)
                           (get_snDate sn)
                           (get_encryptFn sn)
                           (get_decryptFn sn)
                           (set_sesionActiva_False (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) (get_snUsuarios sn))
                           (sn-agregarPublicacion (inicializarPublicacion (+ 1 (getLastID (get_snPublicaciones sn))) date (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) ((get_encryptFn sn)content))                         
                                                  (get_snPublicaciones sn) )

                           (agregarNuevaParticipacion (+ 1 (getLastID (get_snPublicaciones sn))) (get_snGente_que_participo_en_post sn) users)
                            (get_snPosts_Compartidos sn)
                           )
                          )

                       
                       ;las personas ingresadas no se encuentran en la lista de amigos del user actual
                       sn
                       )
                    
                    ;publicacion dirigida al mismo usuario
                    ;es primera publicacion que se hace en red social?
                    (if(null? (get_snPublicaciones sn))
                          (socialnetworkActualizado
                           (get_snName sn)
                           (get_snDate sn)
                           (get_encryptFn sn)
                           (get_decryptFn sn)
                           (set_sesionActiva_False (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) (get_snUsuarios sn))
                           (sn-agregarPublicacion (inicializarPublicacion 0 date (getUser_username(getUser_sesionIniciada(get_snUsuarios sn)))  ((get_encryptFn sn)content))        
                                                  (get_snPublicaciones sn))
                           (agregarNuevaParticipacion 0 (get_snGente_que_participo_en_post sn) (list (getUser_username(getUser_sesionIniciada(get_snUsuarios sn)))))
                            (get_snPosts_Compartidos sn)
                           )

                          ;existe una publicacion
                          (socialnetworkActualizado
                           (get_snName sn)
                           (get_snDate sn)
                           (get_encryptFn sn)
                           (get_decryptFn sn)
                           (set_sesionActiva_False (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) (get_snUsuarios sn))
                           (sn-agregarPublicacion (inicializarPublicacion (+ 1 (getLastID (get_snPublicaciones sn))) date (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) ((get_encryptFn sn)content))
                                                  (get_snPublicaciones sn))

                           (agregarNuevaParticipacion (+ 1 (getLastID (get_snPublicaciones sn))) (get_snGente_que_participo_en_post sn)  (list (getUser_username(getUser_sesionIniciada(get_snUsuarios sn)))))
                            (get_snPosts_Compartidos sn)
                           )
                

                    
                    )
                    )


                 ;no se encontro algun usuario con sesion iniciada
                 sn
                 )
                ;la red social ingresada no es valida
              '()
                )


              )
  )))

                               
;--------------------------FOLLOW----------------------------------------------------    
;descripcion: funcion currificada que permite a un usuario con sesion iniciada en la plataforma seguir a otro usuario
;dom: socialnetwork
;recorridoFinal: socialnetwork
;rec: NA
(define follow(lambda(sn)
         (lambda(date)
           (lambda(user)
             ;se verifica que exista una sesion activa
             (if(and(esRedSocial? sn) (esString? user) (date? date))
             (if(not(null? (getUser_sesionIniciada(get_snUsuarios sn))))
                ;se verifica que el usuario a seguir con el usuario login no sean iguales
                (if (not(eqv? user (getUser_username(getUser_sesionIniciada(get_snUsuarios sn)))))
                    (socialnetworkActualizado
                     (get_snName sn)
                     (get_snDate sn)
                     (get_encryptFn sn)
                     (get_decryptFn sn)
                     (set_sesionActiva_False (getUser_username(getUser_sesionIniciada(get_snUsuarios sn)))
                                             (agregar_seguidor user (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) date (get_snUsuarios sn)))
                     
                     
                     (get_snPublicaciones sn)
                     (get_snGente_que_participo_en_post sn)
                     (get_snPosts_Compartidos sn)
                     )
                    "un usuario no puede seguirse a si mismo"
                    )
                ;no se ha encontrado una sesion activa
                sn
                )
             ;la red social recibida como entrada no es valida
             '()
             )
             )

             ))
              )
  
;--------------------------SHARE----------------------------------------------------    
#|
descripcion: funcion currificada que permite compartir contenido de un usuario en su propio espacio o dirigido a otros usuarios
dom: socialnetwork
recorridoFinal: socialnetwork
rec: natural
|#
(define share(lambda(sn)
               (lambda(date)
                 (lambda(postID . users)
                   (if(and (esRedSocial? sn) (date? date) (esNumero? postID))
                      ;se verifica si hay un usuario con sesion activa
                      (if(not(null? (getUser_sesionIniciada(get_snUsuarios sn))))
                         ;si (null? users) es verdadero -> el usuario comparte el post a si mismo 
                         (if (null? users)
                             (socialnetworkActualizado
                              (get_snName sn)
                              (get_snDate sn)
                              (get_encryptFn sn)
                              (get_decryptFn sn)
                              (set_sesionActiva_False(getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) (get_snUsuarios sn))
                              (get_snPosts_Compartidos sn)
                              (get_snGente_que_participo_en_post sn)
                              (agregar_cola (get_snPosts_Compartidos sn) (list(agregar_cabeza (list date (getUser_username(getUser_sesionIniciada(get_snUsuarios sn)))) postID)))
                              )

                             ;se comparte la publicacion con users
                             ;se verifica que primero sean amigos
                             (if(mi-andmap(mi-map
                                  (son-amigos? (getUser_amigos(getUser_sesionIniciada(get_snUsuarios sn)))) users ))
                             (socialnetworkActualizado
                              (get_snName sn)
                              (get_snDate sn)
                              (get_encryptFn sn)
                              (get_decryptFn sn)
                              (set_sesionActiva_False(getUser_username(getUser_sesionIniciada(get_snUsuarios sn))) (get_snUsuarios sn))
                              (get_snPosts_Compartidos sn)
                              (get_snGente_que_participo_en_post sn)
                              (agregar_cola (get_snPosts_Compartidos sn) (list(agregar_cabeza (agregar_cabeza users date) postID)))
                              )
                             ;si no son amigos se omiten
                             sn
                             )
                             
                             
                             )
          
                         ;si no hay sesion activa se retorna la red social sin cambios
                         sn
                         )
                      "red social no valida"
                      )
                   )
                 )
               ))


;--------------------------SOCIALNETWORK->STRING----------------------------------------------------

#|descripcion: funcion que recibe una socialnetwork y entrega
una representacion del mismo como un string posible de visualizar de forma
comprensible al usuario

dom: socialnetwork
recorrido: string
rec:NA
|#
;------------------------------------


(define socialnetwork->string(lambda(sn)
                               ;si es nulo se muestra toda la red social
                              (if (null? (getUser_sesionIniciada(get_snUsuarios sn)))
                               (string-append "\nSOCIAL NETWORK:\nNombre: " (get_snName sn)
                                              "\nFecha de creacion: " (date->string (get_snDate sn))
                                              "\nFuncion encriptar"
                                              "\nFuncion desencriptar"
                                              "\nLista de usuarios: " (usuarios->string (get_snUsuarios sn)) "\n"
                                              "\nPublicaciones: " (publicaciones->string (get_snPublicaciones sn) (get_decryptFn sn))
                                              "\nGente que recibio un post: " (participacionesPost->string (get_snGente_que_participo_en_post sn))
                                              "\nGente a la que se les compartio un post: " (share->string (get_snPosts_Compartidos sn))

                                "\n"

                                )
                               ;caso contrario, se muestra toda la informacion del usuario con sesion activa
                               (string-append "\nINFORMACION USUARIO\n"
                                              (usuario->string (getUser_sesionIniciada(get_snUsuarios sn)))"\n"
                                              "\nPUBLICACIONES\n"
                                              (publicaciones->string (my-filter (es_publicacionUsuario? (getUser_username(getUser_sesionIniciada(get_snUsuarios sn)))) (get_snPublicaciones sn) ) (get_decryptFn sn) ) "\n"
                                              "\nPUBLICACIONES QUE PARTICIPA EL USUARIO\n"
                                              (participacionesPost->string  (my-filter (usuarioParticipa? (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))))  (get_snGente_que_participo_en_post sn))) "\n"
                                              "\nPUBLICACIONES COMPARTIDAS AL USUARIO\n"
                                              (share->string (my-filter (usuario-parte-del-share? (getUser_username(getUser_sesionIniciada(get_snUsuarios sn))))  (get_snPosts_Compartidos sn)))
                                              
                                              )


                               )

  )
  )
 


;mover a main.rkt              
;----------------------------------SECCION ALGO->STRING-------------------------------
;descripcion: funcion currificada que verifica si un usuario es parte del share
;dom:string X shares
;recorrido:boolean
;rec:cola
 (define usuario-parte-del-share?(lambda(nombreUsuario)
                                   (lambda(shares)
                                     (member? nombreUsuario shares)
                                     )
                                   

   ))
;descripcion: funcion que convierte una fecha a string
;dom:date
;recorrido:string
;rec:natural
(define date->string(lambda (fecha)
  (if(null? fecha)
     ""
     (string-append (number->string (getDay fecha)) " / " (number->string (getMonth fecha)) " / " (number->string (getYear fecha)))
  )))

 
;descripcion:funcion que convierte una lista de amigos a string
;dom:list
;recorrido:string
;rec:natural
(define amigos->string (lambda(lista-amigos)
                         (if(null? lista-amigos)
                            ""
                            (string-append " " (car lista-amigos) ", " (amigos->string (cdr lista-amigos)))
                            )
                         )
  )
;descripcion:funcion que convierte la lista de followers de un usuario a string
;dom:followers user
;recorrido:string
;rec:natural
(define followers->string(lambda(seguidores-usuario)
                           (if(null? seguidores-usuario)
                              ""
                              (string-append "\n  " "[ "(car(cdr(car seguidores-usuario))) ", " (date->string (car (car seguidores-usuario))) "]" (followers->string (cdr seguidores-usuario)))
                              )

                           ))
                         
;descripcion:funcion que convierte un user a string
;dom:user
;recorrido:string
;rec:natural
(define usuario->string(lambda(usuario)
               (if(null? usuario)
                  ""
                  (if(eqv? #t (getUser_sesionActiva usuario))
                     (string-append "\nUsuario: " (getUser_username usuario) "\nSesion Activa" "\nID Usuario: "
                                    (number->string (getUser_idUser usuario)) "\nFecha creacion usuario: "
                                    (date->string (getUser_date usuario)) "\nLista de amigos: " (amigos->string (getUser_amigos usuario))
                                    "\nSeguidores del usuario: " (followers->string (getUser_followers usuario))
                                    )

                     (string-append "\nUsuario: " (getUser_username usuario) "\nSesion Inactiva" "\nID Usuario: "
                                    (number->string (getUser_idUser usuario)) "\nFecha creacion usuario: "
                                    (date->string (getUser_date usuario)) "\nLista de amigos: " (amigos->string (getUser_amigos usuario))
                                    "\nSeguidores del usuario: " (followers->string (getUser_followers usuario))
                                    )
                    
                     )
                  )

                         ))
;descripcion:funcion que convierte una lista de users a string
;dom:users
;recorrido:string
;rec:natural
(define usuarios->string(lambda(lista-usuarios)
                          (if(null? lista-usuarios)
                             ""
                             (string-append "\n" (usuario->string (car lista-usuarios)) (usuarios->string (cdr lista-usuarios)))

                             )

                          ))
;descripcion:funcion que convierte una publicacion a string
;dom:publicacion X decryptFn
;recorrido:string
;rec:natural
(define publicacion->string (lambda(publicacion funcionDecrypt)
                              (string-append "\nAutor del post: " (getPost_autor publicacion) "\nID Post: " (number->string (getPost_id publicacion))
                              "\nFecha creacion del post: " (date->string (getPost_date publicacion)) "\nContenido post: "
                              (funcionDecrypt (getPost_contenido publicacion)) 
                              )

                              ))
;descripcion:funcion que convierte publicaciones a string
;dom:publicaciones X decryptFn
;recorrido:string
;rec:natural
(define publicaciones->string(lambda(lista-publicaciones funcionDecrypt)
                               (if(null? lista-publicaciones)
                                  ""
                                  (string-append "\n" (publicacion->string (car lista-publicaciones) funcionDecrypt) (publicaciones->string (cdr lista-publicaciones) funcionDecrypt))
                                  )
                               ))
;descripcion:funcion que convierte una participacion en string
;dom:participacion
;recorrido:string
;rec:natural
(define participacion-en-post->string(lambda(postParticipado)
                               (if(null? postParticipado)
                                  ""
                                  (string-append "\nIDpost que se publico en el muro de los usuarios: " (number->string(car postParticipado))
                                   "\nUsuarios que tienen la publicacion escrita en su muro: " (amigos->string (cdr postParticipado)))
                                  )

                               ))
;descripcion:funcion que convierte participaciones a string
;dom:participaciones
;recorrido:string
;rec:natural
(define participacionesPost->string (lambda(lista-participaciones)
                                      (if(null? lista-participaciones)
                                         ""
                                         (string-append "\n" (participacion-en-post->string (car lista-participaciones)) (participacionesPost->string (cdr lista-participaciones)) )

                                         )
                                      ))
;descripcion:funcion que convierte un share(no la funcion share) a string
;dom:share(no la funcion share de fun.obligatorias)
;recorrido:string
;rec:natural
(define postShare->string(lambda(publicacion-compartida)
                           (if(null? publicacion-compartida)
                              ""
                              (string-append "\nSe ha compartido el post: " (number->string (car publicacion-compartida))
                                             "\nEl dia: " (date->string (cadr publicacion-compartida))
                                             "\nPersonas que se les compartio el post: " (amigos->string (cddr publicacion-compartida))
                                             )
                              )

                           ))
;descripcion:funcion que convierte los shares (no la funcion) a string
;dom:shares (no la funcion share de fun.obligatorias)
;recorrido:string
;rec:natural
(define share->string(lambda(lista-publicacionesCompartidas)
                       (if(null? lista-publicacionesCompartidas)
                          ""
                          (string-append "\n" (postShare->string (car lista-publicacionesCompartidas)) (share->string (cdr lista-publicacionesCompartidas)))
                          )

                       ))
;--------------------------------------EJEMPLOS DE USO-------------------------------------------
;para los 3 casos, primero se debe crear la red social, registrar usuarios y agregar amigos

#|
->CREAR RED SOCIAL
(define emptyFB(socialnetwork "fb" (date 25 10 2021) encryptFn encryptFn))

->REGISTRAR USUARIOS A TRAVES DE REGISTER
(define registrarUsuarios(register(register(register (register (register emptyFB (date 1 1 2000) "user" "pass") (date 23 2 2007) "user1" "pass1") (date 12 7 2010)
            "user2" "pass2") (date 15 1 1998) "user3" "pass3") (date 30 1 1996) "user4" "pass4"))

->AGREGAR AMIGOS(se requiere que un usuario tenga amigos para hacer un post dirigido hacia ellos/el mismo
(define agregarAmigos(agregar_amigo "user3"(agregar_amigo "user1"(agregar_amigo "user" registrarUsuarios (list "user3" "user4")) (list "user" "user2")) (list "user4")))

-> AL COMIENZO DEL LOGIN 1 2 Y 3 SE HACE EL POST
Ejemplo1:

(define login1(((login agregarAmigos "user" "pass" post)(date 1 1 2004))"mi primer post" "user3" "user4"))
(define follow1(((login login1 "user2" "pass2" follow)(date 30 10 2020))"user"))
(define share1(((login follow1 "user" "pass" share)(date 1 1 2021))0 "user3" "user4"))
(display(socialnetwork->string share1))

Ejemplo2:
(define login2 (((login agregarAmigos "user2" "pass2" post) (date 11 12 2022))"segundo post"))
(define follow2(((login login2 "user1" "pass1" follow)(date 25 5 2021))"user2"))
(define share2(((login follow2 "user3" "pass3" share)(date 2 2 2000)) 0 "user4"))
(display(socialnetwork->string share2))

Ejemplo3:
(define login1(((login agregarAmigos "user" "pass" post)(date 1 1 2004))"mi primer post" "user3" "user4"))
(define login2 (((login login1 "user2" "pass2" post) (date 11 12 2022))"segundo post"))
(define follow1(((login login2 "user2" "pass2" follow)(date 30 10 2020))"user"))
(define follow2(((login follow1 "user1" "pass1" follow)(date 25 5 2021))"user2"))
(define share1(((login follow2 "user1" "pass1" share)(date 1 1 2021))1 "user2"))
(display(login share1 "user2" "pass2" socialnetwork->string))
|#

