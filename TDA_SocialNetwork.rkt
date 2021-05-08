#lang racket
;
;Descrip:
;Dom:
;Rec:
;Recursion:

;TDA Date


;Constructor
;Descr: Funcion que permite crear una fecha
;Dom: Dia <number> X mes <number> X año <number>
;Rec: Date
;Recursion: NA
(define date(lambda(dd mm yyyy)
              (if(date? (list dd mm yyyy)) (list dd mm yyyy)
                 '()
                 )
 ))           

;Pertenencia
;Descr: Funcion que verifica si los argumentos cumplen el formato fecha
;Dom: Dia <number> X mes <number> X año <number>
;Rec: Bool
;Recursion: NA
(define date? (lambda(fecha)
                (if(and(number? (getDay fecha))(number? (getMonth fecha))(number? (getYear fecha)))
                   #t
                   #f
                   )
                ))
  

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
;(name, date, usuarios, publicaciones EncryptFn DecryptFn)


;Constructor
;Dom: String X Date X EncryptFunction X DecryptFunction
;Rec: SocialNetwork

(define socialnetwork(lambda(name date encryptFn decryptFn)
                       (if(and(es_Name? name)(date? date))

                          (list -1)
                          )
;Pertenencia
(define es_Name?(lambda(name)
                  (if (string? name) #t
                      #f)
                  )
  )

;Selectores
(define snName car)
(define snDate cadr)
(define snUsers (lambda(s)(car(cddddr s))))
(define snPosts (lambda(s)(car (cdr(cddddr s)))))

;TDA user
;(idUser,username,pass,amigos)
;donde amigos= (idUser1,idUser2,....,udUserN)

;constructor
;Dom: number X string X string X amigos
;Rec: user
;bool representa sesionActiva donde #f: sesion no activa y #t: sesion activa
;inicializa en #f
(define primerUser(lambda(username pass)
                       (list 0 username pass '() #f)))

(define user(lambda(userID username pass amigos) 
              (list userID username pass amigos #f)))


;selectores
(define userID car)
(define username cadr)
(define pass caddr)
(define amigos cadddr)
(define sesionActiva (lambda(sN)(car(cddddr sN))))


;TDA Users
;user1 X user2 X....X userN
;selectores
(define firstUser car)
(define lastUser(lambda(users)(firstUser(reverse users))))
(define lastUserID(lambda(users)(userID(firstUser(reverse users)))))
(define lastUserSA(lambda(users)(sesionActiva(lastUser users))))

  

;; (id,dasd,asdasd,asd) (id,asdasd,asdasd,asdasd)

;userID(firstUser(reverse users))
#| 
// constructor TDA usuario
(define create-user
    (lambda (nombre contraseña lastUserId)
        (list nombre contraseña (+ 1 lastUserId)))

// (u1, u2, u3, u99)
    reverse (users)
        car (cdr users)

// if list is empty:
    id = 1
   else:
    id = lastId + 1
   return id


|#



#|
(define printVariables(lambda(user . labels)
                         (if(null? labels)"post dirigido al mismo usuario"
                            labels
                            )

                         )
   )
;usuario(id,user,pass,bool); donde bool: sesion activa
;selector
(define getUserId car)
(define getUserName cadr)
(define getUserPass caddr)
;Amigo (idUsuario x nombreAmigo1 nombreAmigo2)
(define amigos (list (list 1 "u1" "u2") (list 2 "u3" "u4") (list 3 "u5" "u6")))
(define buscar_por_ID (lambda(lista_amigos id_a_buscar)
                        (if (eqv? id_a_buscar (getUserId(car lista_amigos)))
                            (cdr (car lista_amigos))
                            (buscar_por_ID (cdr lista_amigos) id_a_buscar))

                        )

  )
(define printVariables(lambda(user . labels)
                         (if(null? labels)"post dirigido al mismo usuario"
                            labels
                            )

                         )
   )
(define date(lambda(dd mm yyyy)
              (if(date? (list dd mm yyyy)) (list dd mm yyyy)
                 '()
                 )
 ))           

|#