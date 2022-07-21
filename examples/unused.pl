bad(X, Y).


bad(X, X, Y, Z, _T) :-
    toto(term(Y)),
    titi(R1),
    tutu(R1, R1, R0),
    =(R4, term(R8)),
    is(R7, +(X, Y)),
    ==(XXX, YYY).
