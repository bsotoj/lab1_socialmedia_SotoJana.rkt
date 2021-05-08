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
;(name, date, encryptFn decryptFn,usuarios, publicaciones)


;Constructor
;Dom: String X Date X EncryptFunction X DecryptFunction
;Rec: SocialNetwork

(define socialnetwork(lambda(name date encryptFn decryptFn)
                       (if(es_Name? name) (list name date encryptFn encryptFn '() '())
                          '()
                          )
                       )
  )
;Pertenencia
(define es_Name?(lambda(name)
                  (if (string? name) #t
                      #f)
                  )
  )

;Selectores
(define get_snName car)
(define get_snDate cadr)
(define get_encryptFn caddr)
(define get_decryptFn cadddr)
(define get_snUsuarios (lambda(sn)(car(cdr(cdr(cdr(cdr sn)))))))
(define get_snPublicaciones(lambda(sn)(car(cdr(cdr(cdr(cdr(cdr sn))))))))


#|
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

  

|#