male(james1).
male(charles1).
male(charles2).
male(james2).
male(george1).

female(elizabeth).
female(catherine).
female(sophia).

parent(charles1, james1).
parent(elizabeth, james1).
parent(charles2, charles1).
parent(catherine, charles1).
parent(james2, charles1).
parent(sophia, elizabeth).
parent(george1, sophia).
parent(sophia, james1).

sister(X, Y) :- parent(X, P), parent(Y, P), female(Y).
brother(X, Y) :- parent(X, P), parent(Y, P), male(Y).

head([H | T], H).



append([], Y, Y).
append([H | T], Y, [H | R]) :- append(T, Y, R).

%% this is a nice comment
length([], 0).
length([H|T], R) :- length(T, R1), R is R1 + 1.

member(X, [X|T]).
member(X, [Y|T]) :- member(X, T).

last([X], X).
last([Y|T], X) :- last(T, X).

element_at(X, [X|T], 0).
element_at(X, [Y|T], N) :- N1 is N - 1, element_at(X, T, N1).

element_where(X, [X|T], 0).
element_where(X, [Y|T], N) :- element_where(X, T, N1), N is N1 + 1.

%this is another comment