(deffacts HECHOS-INICIALES
(A) (D) (C) (B) (Z))
(defrule r1
 (A)(B)(C)
=>
 (printout t "regla1" crlf))
(defrule r2
 (C) (D)
=>
 (printout t "regla2" crlf))
(defrule r3
 (Z)
=>
 (printout t "regla3" crlf))
(defrule r4
 (C) (A)
=>
 (printout t "regla4" crlf))

; agenda
con la z r3 y r31 primero r3 y luego r31
despues la B
