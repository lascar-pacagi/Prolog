/*
From Baptiste Wicht.

Here is the riddle as stated by Einstein:

   1. In a street there are five houses, painted five different colours.
   2. In each house lives a person of different nationality
   3. These five homeowners each drink a different kind of beverage, smoke different brand of cigar and keep a different pet.

The question is : Who owns the fish ?

And there are 15 hints :

   1. The Brit lives in a red house.
   2. The Swede keeps dogs as pets.
   3. The Dane drinks tea.
   4. The Green house is next to, and on the left of the White house.
   5. The owner of the Green house drinks coffee.
   6. The person who smokes Pall Mall rears birds.
   7. The owner of the Yellow house smokes Dunhill.
   8. The man living in the centre house drinks milk.
   9. The Norwegian lives in the first house.
  10. The man who smokes Blends lives next to the one who keeps cats.
  11. The man who keeps horses lives next to the man who smokes Dunhill.
  12. The man who smokes Blue Master drinks beer.
  13. The German smokes Prince.
  14. The Norwegian lives next to the blue house.
  15. The man who smokes Blends has a neighbour who drinks water.
*/

persons(0, []).
persons(N, [house(_Men,_Color,_Drink,_Smoke,_Animal)|T]) :- =\=(N, 0), is(N1, -(N,1)), persons(N1,T).

person(1, [H|_], H).
person(N, [_|T], R) :- =\=(N, 1), is(N1, -(N,1)), person(N1, T, R).

% The Brit lives in a red house
hint1([house(brit,red,_, _, _)|_]).
hint1([_|T]) :- hint1(T).

% The Swede keeps dogs as pets
hint2([house(swede,_,_,_,dog)|_]).
hint2([_|T]) :- hint2(T).

% The Dane drinks tea
hint3([house(dane,_,tea,_,_)|_]).
hint3([_|T]) :- hint3(T).

% The Green house is on the left of the White house
hint4([house(_,green,_,_,_),house(_,white,_,_,_)|_]).
hint4([_|T]) :- hint4(T).

% The owner of the Green house drinks coffee.
hint5([house(_,green,coffee,_,_)|_]).
hint5([_|T]) :- hint5(T).

% The person who smokes Pall Mall rears birds
hint6([house(_,_,_,pallmall,bird)|_]).
hint6([_|T]) :- hint6(T).

% The owner of the Yellow house smokes Dunhill
hint7([house(_,yellow,_,dunhill,_)|_]).
hint7([_|T]) :- hint7(T).

% The man living in the centre house drinks milk
hint8(Persons) :- person(3, Persons, house(_,_,milk,_,_)).

% The Norwegian lives in the first house
hint9(Persons) :- person(1, Persons, house(norwegian,_,_,_,_)).

% The man who smokes Blends lives next to the one who keeps cats
hint10([house(_,_,_,blend,_),house(_,_,_,_,cat)|_]).
hint10([house(_,_,_,_,cat),house(_,_,_,blend,_)|_]).
hint10([_|T]) :- hint10(T).

% The man who keeps horses lives next to the man who smokes Dunhill
hint11([house(_,_,_,dunhill,_),house(_,_,_,_,horse)|_]).
hint11([house(_,_,_,_,horse),house(_,_,_,dunhill,_)|_]).
hint11([_|T]) :- hint11(T).

% The man who smokes Blue Master drinks beer
hint12([house(_,_,beer,bluemaster,_)|_]).
hint12([_|T]) :- hint12(T).

% The German smokes Prince
hint13([house(german,_,_,prince,_)|_]).
hint13([_|T]) :- hint13(T).

% The Norwegian lives next to the blue house
hint14([house(norwegian,_,_,_,_),house(_,blue,_,_,_)|_]).
hint14([house(_,blue,_,_,_),house(norwegian,_,_,_,_)|_]).
hint14([_|T]) :- hint14(T).

% The man who smokes Blends has a neighbour who drinks water
hint15([house(_,_,_,blend,_),house(_,_,water,_,_)|_]).
hint15([house(_,_,water,_,_),house(_,_,_,blend,_)|_]).
hint15([_|T]) :- hint15(T).

% The question : Who owns the fish ?
question([house(_,_,_,_,fish)|_]).
question([_|T]) :- question(T).

solution(Persons) :-
  persons(5, Persons),
  hint1(Persons),
  hint2(Persons),
  hint3(Persons),
  hint4(Persons),
  hint5(Persons),
  hint6(Persons),
  hint7(Persons),
  hint8(Persons),
  hint9(Persons),
  hint10(Persons),
  hint11(Persons),
  hint12(Persons),
  hint13(Persons),
  hint14(Persons),
  hint15(Persons),
  question(Persons).
