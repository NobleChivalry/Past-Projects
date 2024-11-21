% EVAL(E, V)
eval(E, E) :- % If E is a number, then V is just E.
    number(E).

eval(A + B, V) :- % Addition
    eval(A, VA),
    eval(B, VB),
    V is VA + VB.

eval(A - B, V) :- % Subtraction
    eval(A, VA),
    eval(B, VB),
    V is VA - VB.

eval(A * B, V) :- % Multiplication
    eval(A, VA),
    eval(B, VB),
    V is VA * VB.

eval(A / B, V) :- % Division
    eval(A, VA),
    eval(B, VB),
    VB \= 0,
    V is VA // VB. % Uses integer division

eval(A ^ B, V) :- % Exponent
    eval(A, VA),
    eval(B, VB),
    V is VA ^ VB.



% -------------------------------------------------------------------------------------------------------------------------------------------------------



% SIMPLIFY(E, S)
simplify(E, E) :- % Numbers and variables are already simplified
    number(E);
    atom(E).

simplify(C * A * A / A, S) :-
    simplify(A, DA),
    simplify(C, DC),
    number(DC),
    simplify(DC * DA, S).

simplify(A + B, SA) :- % Adding 0
    simplify(A, SA),
    simplify(B, SB),
    SB == 0.
simplify(B + A, SA) :-
    simplify(A, SA),
    simplify(B, SB),
    SB == 0.

simplify(A - B, SA) :- % Subtracting 0
    simplify(A, SA),
    simplify(B, SB),
    SB == 0.
simplify(B - A, -SA) :- % Subtracting 0
    simplify(A, SA),
    simplify(B, SB),
    SB == 0.

simplify(_ * B, 0) :- % Multiplying by 0
    simplify(B, SB),
    SB == 0.
simplify(B * _, 0) :-
    simplify(B, SB),
    SB == 0.

simplify(A * B, -SA) :- % Multiplying by -1
    simplify(A, SA),
    simplify(B, SB),
    SB == -1.
simplify(B * A, -SA) :-
    simplify(A, SA),
    simplify(B, SB),
    SB == -1.
simplify(-A * B, SA) :-
    simplify(A, SA),
    simplify(B, SB),
    SB == -1.
simplify(B * -A, SA) :-
    simplify(A, SA),
    simplify(B, SB),
    SB == -1.

simplify(A * B, SA) :- % Multiplying by 1
    simplify(A, SA),
    simplify(B, SB),
    SB == 1.
simplify(B * A, SA) :-
    simplify(A, SA),
    simplify(B, SB),
    SB == 1.

simplify(A / B, SA) :- % Dividing by 1
    simplify(A, SA),
    simplify(B, SB),
    SB == 1.

simplify(_ ^ B, 1) :- % Any base with exponent 0 equals 1
    simplify(B, SB),
    SB == 0.

simplify(A ^ B, SA) :- % Any base with exponent 1 equals base
    simplify(A, SA),
    simplify(B, SB),
    SB == 1.

simplify(A - A, 0). % Subtraction with identical operands equals 0

simplify(A / A, 1) :- % Division with identical operands equals 1
    A \= 0.

simplify(B * A, A * B) :- % Makes coefficient in front of variable
    number(A).

simplify(A + B, S) :- % Addition expressions
    simplify(A, SA),
    simplify(B, SB),
    number(SA),
    number(SB),
    eval(SA + SB, S).
simplify(A + (-B), SA - SB) :-
    simplify(A, SA),
    simplify(B, SB).
simplify(A + B, SA - X) :-
    simplify(A, SA),
    simplify(B, SB),
    not(number(SB)),
    remove_negative_sign(SB, X).
simplify(A + B, SA + SB) :-
    simplify(A, SA),
    simplify(B, SB).

simplify(A - B, S) :- % Subtraction expressions
    simplify(A, SA),
    simplify(B, SB),
    number(SA),
    number(SB),
    eval(SA - SB, S).
simplify(A - (-B), SA + SB) :-
    simplify(A, SA),
    simplify(B, SB).
simplify(A - B, SA + X) :-
    simplify(A, SA),
    simplify(B, SB),
    not(number(SB)),
    remove_negative_sign(SB, X).
simplify(A - B, SA - SB) :-
    simplify(A, SA),
    simplify(B, SB).

simplify(A * B, S) :- % Multiplication expressions
    simplify(A, SA),
    simplify(B, SB),
    number(SA),
    number(SB),
    eval(SA * SB, S).
simplify(A * B, SA * SB) :-
    simplify(A, SA),
    simplify(B, SB).

simplify(A / B, S) :- % Division expressions
    simplify(A, SA),
    simplify(B, SB),
    number(SA),
    number(SB),
    SB \= 0,
    eval(SA / SB, S).
simplify(A / B, SA / SB) :-
    simplify(A, SA),
    simplify(B, SB),
    SB \= 0.

simplify(A ^ B, S) :- % Exponent expressions
    simplify(A, SA),
    simplify(B, SB),
    number(SA),
    number(SB),
    eval(SA ^ SB, S).
simplify(A ^ B, SA ^ SB) :-
    simplify(A, SA),
    simplify(B, SB).



% -------------------------------------------------------------------------------------------------------------------------------------------------------



% DERIV(E, D)
deriv(C, 0) :- % Derivative of constant is 0
    number(C), !.

deriv(x, 1) :- !. % Derivative of x is 1

deriv(C * x, D) :- % Derivative of constant times x is constant
    simplify(C, D),
    number(C), !.
deriv(x * C, D) :-
    simplify(C, D),
    number(C), !.

deriv(x^N, D) :- % Power rule
    simplify(N, DN),
    DN >= 0,
    K is DN - 1,
    simplify(DN * x^K, D), !.

deriv(x^N, D) :- % Power rule for negative exponents
    simplify(N, DN),
    DN < 0,
    K is -1 * (DN - 1),
    simplify(DN / x^K, D), !.

deriv(C * x^N, D) :- % Power rule with coefficient
    simplify(C, DC),
    simplify(N, DN),
    number(C),
    number(N),
    DN >= 0,
    K is DN - 1,
    X is DC * DN,
    simplify(X * x^K, D), !.
deriv(x^N * C, D) :-
    simplify(C, DC),
    simplify(N, DN),
    number(C),
    number(N),
    DN >= 0,
    K is DN - 1,
    X is DC * DN,
    simplify(X * x^K, D), !.

deriv(C * x^N, D) :- % Power rule with coefficient for negative exponents
    simplify(C, DC),
    simplify(N, DN),
    number(C),
    number(N),
    DN < 0,
    K is -1 * (DN - 1),
    X is DC * DN,
    simplify(X / x^K, D), !.
deriv(x^N * C, D) :-
    simplify(C, DC),
    simplify(N, DN),
    number(C),
    number(N),
    DN < 0,
    K is -1 * (DN - 1),
    X is DC * DN,
    simplify(X / x^K, D), !.

deriv(A / x, D / x^2) :- % Differentiates fractions of power 1
    simplify(A, DA),
    number(DA),
    D is -1 * DA.

deriv(A / x^B, D / x^C) :- % Differentiates fractions of higher power
    simplify(A, DA),
    simplify(B, DB),
    number(DA),
    number(DB),
    D is -1 * DA * DB,
    C is B + 1.

deriv(A + B, D) :- % Sum rule
    simplify(A, DA1),
    simplify(B, DB1),
    deriv(DA1, DA),
    deriv(DB1, DB),
    not(number(DB)),
    remove_negative_sign(DB, X),
    simplify(DA - X, D), !.
deriv(A + B, D) :-
    simplify(A, DA1),
    simplify(B, DB1),
    deriv(DA1, DA),
    deriv(DB1, DB),
    simplify(DA + DB, D), !.

deriv(A - B, D) :- % Difference rule
    simplify(A, DA1),
    simplify(B, DB1),
    deriv(DA1, DA),
    deriv(DB1, DB),
    not(number(DB)),
    remove_negative_sign(DB, X),
    simplify(DA + X, D), !.
deriv(A - B, D) :-
    simplify(A, DA1),
    simplify(B, DB1),
    deriv(DA1, DA),
    deriv(DB1, DB),
    simplify(DA - DB, D), !.

deriv(A / B, D) :- % Simplifies division and differentiates
    simplify(A, DA),
    simplify(B, DB),
    simplify(DA / DB, X),
    deriv(X, D), !.



remove_negative_sign(-A, A). % Handles double negation of variables

remove_negative_sign(A, S) :- % Handles double negation
    simplify(A, DA),
    number(A),
    X is DA * -1,
    simplify(X, S).

remove_negative_sign(A * B, S) :- % Handles double negation with constants
    simplify(A, DA),
    simplify(B, DB),
    number(DA),
    DA < 0,
    X is DA * -1,
    simplify(X * DB, S).

remove_negative_sign(A / B, S) :- % Handles double negation for fractions
    simplify(A, DA),
    simplify(B, DB),
    number(DA),
    DA < 0,
    X is DA * -1,
    simplify(X / DB, S).



% -------------------------------------------------------------------------------------------------------------------------------------------------------



% PARTY_SEATING(L)

guest(G) :- % Guest defined as either male or female person
    male(G);
    female(G).

common_language(G1, G2) :- % Checks if guests speak a common language
    speaks(G1, Language),
    speaks(G2, Language),
    G1 \= G2.

not_both_female(G1, G2) :- % Checks if guests are not both female
    (male(G1), male(G2)), G1 \= G2;
    (male(G1), female(G2)), G1 \= G2;
    (female(G1), male(G2)), G1 \= G2.

party_seating([G1, G2, G3, G4, G5, G6, G7, G8, G9, G10]) :-
    guest_list([G1, G2, G3, G4, G5, G6, G7, G8, G9, G10]),
    seating([G1, G2, G3, G4, G5, G6, G7, G8, G9, G10]).

% Generate a list of exactly 10 unique guests
guest_list([G1, G2, G3, G4, G5, G6, G7, G8, G9, G10]) :-
    guest(G1),
    guest(G2), G2 \= G1,
    guest(G3), G3 \= G1, G3 \= G2,
    guest(G4), G4 \= G1, G4 \= G2, G4 \= G3,
    guest(G5), G5 \= G1, G5 \= G2, G5 \= G3, G5 \= G4,
    guest(G6), G6 \= G1, G6 \= G2, G6 \= G3, G6 \= G4, G6 \= G5,
    guest(G7), G7 \= G1, G7 \= G2, G7 \= G3, G7 \= G4, G7 \= G5, G7 \= G6,
    guest(G8), G8 \= G1, G8 \= G2, G8 \= G3, G8 \= G4, G8 \= G5, G8 \= G6, G8 \= G7,
    guest(G9), G9 \= G1, G9 \= G2, G9 \= G3, G9 \= G4, G9 \= G5, G9 \= G6, G9 \= G7, G9 \= G8,
    guest(G10), G10 \= G1, G10 \= G2, G10 \= G3, G10 \= G4, G10 \= G5, G10 \= G6, G10 \= G7, G10 \= G8, G10 \= G9.

seating([H|T]) :-
    valid_seating([H|T], H).

valid_seating([_], _).
valid_seating([A, B | Tail], Head) :-
    common_language(A, B),
    not_both_female(A, B),
    valid_seating([B | Tail], Head).
valid_seating([A], Head) :- % Ensures circular seating
    common_language(A, Head),
    not_both_female(A, Head).
