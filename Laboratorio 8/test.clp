(defglobal ?*ultimo_id* = 0)

(deffunction update_id()
(printout t ?*ultimo_id* crlf)
(bind ?*ultimo_id* (+ ?*ultimo_id* 1))
)

(defrule init
    ?f <- (true)
    =>
    (update_id)
    (retract ?f)
    (assert (true))
)

(deffacts name
    (true)
)
