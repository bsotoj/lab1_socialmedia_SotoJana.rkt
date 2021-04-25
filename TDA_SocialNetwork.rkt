#lang racket

;TDA Date
;Constructor TDA Date
;Dom: Dia <number> X mes <number> X año <number>
;Rec: TDA Date
(define Date(lambda(dd mm yyyy)(
                                cond [(Date? dd mm yyyy)(list dd mm yyyy)]
                                      [else '()]                            
                                
                                )

  ))
;Pertenencia
;Dom: Dia <number> X mes <number> X año <number>
;Rec: Bool
(define Date? (lambda (dd mm yyyy)(
                                   cond [(and(number? dd)(number? mm)(number? yyyy)(list dd mm yyyy))]

                                         [else #f])               )
                )
  
