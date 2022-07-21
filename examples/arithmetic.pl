add(zero, N, N).
add(s(N1), N2, s(N3)) :- add(N1, N2, N3).

sub(N1, N2, N3) :- add(N2, N3, N1).

mul(zero, N, zero).
mul(s(N1), N2, N3) :- mul(N1, N2, N4), add(N2, N4, N3).

inf(zero, s(N)).
inf(s(N1), s(N2)) :- inf(N1, N2).

fac(zero, s(zero)).
fac(s(N1), N2) :- fac(N1, N3), mul(s(N1), N3, N2).

factorial(0, 1).
factorial(N, Res) :- =\=(N, 0), is(N1, -(N, 1)), factorial(N1, Res1), is(Res, *(N, Res1)).

power(_, 0, 1).
power(X, N, Res) :-
    >(N, 0),
    is(N1, -(N, 1)),
    power(X, N1, Res1),
    is(Res, *(X, Res1)).
