(deftemplate datos-persona
  (multislot nombre) ; este campo puede tener varios valores
  (slot color-ojos(type LEXEME) (allowed-values marron negro verde azul))
  (slot estado-civil (type LEXEME) (allowed-values soltero casado))
  (slot color-pelo (type LEXEME) (allowed-values marron negro rubio pelirrojo verde azul amarillo))
)

(deffacts datos-persona
  (datos-persona (nombre UnNombre) (color-ojos azul) (estado-civil soltero) (color-pelo marron))
  (datos-persona (nombre DosNombre) (color-ojos marron) (estado-civil soltero) (color-pelo marron))
  (datos-persona (nombre TresNombre) (color-ojos verde) (estado-civil soltero) (color-pelo negro))
  (datos-persona (nombre TresNombre Apellido) (color-ojos azul) (estado-civil soltero) (color-pelo negro))
  (datos-persona (nombre CuatroNombre) (color-ojos marron) (estado-civil casado) (color-pelo negro))
  (datos-persona (nombre CincoNombre) (color-ojos azul) (estado-civil soltero) (color-pelo verde))
)

; (defrule solteros-pelo-marron
;   ?v <- (datos-persona (nombre $?n) (estado-civil soltero) (color-pelo ~marron))
; =>
;   (printout t $?n " tiene pelo distinto a marron y es soltera: " ?v crlf)
; )

; (defrule marron-negro
;   (datos-persona (nombre $?n) (estado-civil ?) (color-pelo marron | negro))
; =>
;   (printout t $?n " tiene pelo marron o negro" crlf)
; )

; (defrule marron-negro
;   (datos-persona (nombre $?n) (estado-civil ?) (color-pelo ~marron & ~negro))
; =>
;   (printout t $?n " no tiene  el pelo ni marron ni negro" crlf)
; )

(defrule marron-negro
  (datos-persona (nombre $?uno_nombre) (color-ojos ?uno_ojos) (estado-civil ?) (color-pelo ?uno_pelo))
  (datos-persona (nombre $?dos_nombre) (color-ojos ?dos_ojos) (estado-civil ?) (color-pelo ?dos_pelo))
  (test
    (and
      (or
        (eq ?uno_ojos azul)
        (eq ?uno_ojos verde)
      )
      (neq ?uno_pelo negro)
      (neq ?uno_ojos ?dos_ojos)
      (or
        (eq ?dos_pelo pelirrojo)
        (eq ?dos_pelo ?uno_pelo)
      )
    )
  )
=>
  (printout t $?uno_nombre " - " $?dos_nombre crlf)
)
