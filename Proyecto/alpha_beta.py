
g = {
    # nivel 1
    "a": ["b", "c", "d"],
    # nivel 2
    "b": ["e", "f"],
    "c": ["g", "h"],
    "d": ["i", "j"],
    # nivel 3
    "e": ["k", "l"],
    "f": ["m", "n", "o"],
    "g": ["p", "q", "r"],
    "h": ["s", "t"],
    "i": ["u", "v"],
    "j": ["w", "x"],
    #nivel 4
    "k": 4,
    "l": 5,
    "m": 3,
    "n": 6,
    "o": 1,
    "p": 4,
    "q": 2,
    "r": 1,
    "s": 1,
    "t": 2,
    "u": 5,
    "v": 4,
    "w": 9,
    "x": 8,
}


def terminal(s):
    """Devuelve True si el nodo es terminal"""
    try:
        int(s)
        return True
    except: # error si es una lista
        return False


def max_value(s, id_padre, alpha=None, beta=None):
    if terminal(s):
        return (s, id_padre)
    v = -9999
    for c in s:
        v_aux, id_padre_aux = min_value(g[c], c, alpha, beta)
        if v_aux > v:
            v = v_aux
            id_padre = id_padre_aux
        if beta:  # si beta es None no entra
            if v_aux >= beta:
                return v
        if alpha:
            if v_aux > alpha:
                alpha = v_aux
    return (v, id_padre)


def min_value(s, id_padre, alpha=None, beta=None):
    if terminal(s):
        return (s, id_padre)
    v = 9999
    for c in s:
        v_aux, id_padre_aux = max_value(g[c], c, alpha, beta)
        if v_aux < v:
            v = v_aux
            id_padre = id_padre_aux
        if alpha:  # si alpha es None no entra
            if v_aux <= alpha:
                return v
        if beta:
            if v_aux < beta:
                beta = v_aux
    return (v, id_padre)


def encontrar_camino_vuelta(id_padre):
    # mientras que no sea la raiz
    inicio = False
    camino = [id_padre]
    while not inicio:
        for key, value in g.items():
            try:
                if id_padre in value:
                    id_padre = key
                    camino.append(id_padre)
                    break
            except:
                pass
        if id_padre == "a":
            inicio = True
    return camino

value, id_padre = max_value(g["a"], "a")

print("el valor es:", value)
print("el camino es:", encontrar_camino_vuelta(id_padre))
