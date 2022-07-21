append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :-
    append(Xs, Ys, Zs).

flatten([], []).
flatten([[]|Xs], Res) :-
    flatten(Xs, Res).
flatten([X|Xs], [X|Res]) :-
    \=(X, [_|_]),
    \==(X, []),
    flatten(Xs, Res).
flatten([X|Xs], Res) :-
    =(X, [_|_]),
    flatten(X, Res1),
    flatten(Xs, Res2),
    append(Res1, Res2, Res).

insert(X, [], [X]).
insert(X, Ys, [X|Ys]) :-
    \==(Ys, []).
insert(X, [Y|Ys], [Y|Zs]) :-
    insert(X, Ys, Zs).

permutation([], []).
permutation([X|Xs], Res) :-
    permutation(Xs, Res1),
    insert(X, Res1, Res).

/*
   Magic square

   A1 A2 A3
   B1 B2 B3
   C1 C2 C3
*/
solve([A1, A2, A3, B1, B2, B3, C1, C2, C3]) :-
    permutation([1,2,3,4,5,6,7,8,9], [A1, A2, A3, B1, B2, B3, C1, C2, C3]),
    =:=(15, +(A1, +(A2, A3))),
    =:=(15, +(B1, +(B2, B3))),
    =:=(15, +(C1, +(C2, C3))),
    =:=(15, +(A1, +(B1, C1))),
    =:=(15, +(A2, +(B2, C2))),
    =:=(15, +(A3, +(B3, C3))),
    =:=(15, +(A1, +(B2, C3))),
    =:=(15, +(A3, +(B2, C1))).
