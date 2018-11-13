(defglobal ?*RESULT* = (create$ ))

(defrule imprimirResultado
=>
	(printout t ?*RESULT* crlf)
)

(defrule unir
	(declare (salience 50))
	?a<-(hecho ?x)
=>
	(if (not(integerp(member$ ?x ?*RESULT*)))then
		(bind ?*RESULT* (insert$ ?*RESULT* (+ 1 (length$ ?*RESULT*)) ?x))
	)
	(retract ?a)
)

(defrule separarMulticampo
	(declare (salience 100))
	(not(hecho ))
	?a<-(hecho1 $?x)
=>
	(bind ?i (length$ $?x))
	(while (> ?i 0)
		(assert (hecho (nth$ ?i $?x)))
		(bind ?i (- ?i 1))
	)
	(retract ?a)
)

(deffacts init
	(hecho1 a b c d e)
	(hecho1 g f c b a)
)
