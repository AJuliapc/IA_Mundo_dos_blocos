:- use_module(library(clpfd)).

% Definição dos blocos
block(a, 1).
block(b, 1).
block(c, 2).
block(d, 3).

% Definição das posições válidas (6x4)
place((X, Y)) :- between(1, 6, X), between(1, 4, Y).

% Exemplos de estados iniciais e objetivos
state1([
    occupied((1,1)), clear((1,2)), clear((1,3)), clear((1,4)),
    occupied((2,1)), clear((2,2)), clear((2,3)), clear((2,4)),
    clear((3,1)), clear((3,2)), clear((3,3)), clear((3,4)),
    occupied((4,1)), occupied((4,2)), clear((4,3)), clear((4,4)),
    clear((5,1)), occupied((5,2)), clear((5,3)), clear((5,4)),
    occupied((6,1)), occupied((6,2)), clear((6,3)), clear((6,4)),
    occupied((1,0)), occupied((2,0)), occupied((3,0)), occupied((4,0)), 
    occupied((5,0)), occupied((6,0)),
    clear((1,5)), clear((2,5)), clear((3,5)), clear((4,5)), 
    clear((5,5)), clear((6,5)),
    on(a,(4,1)), on(b,(6,1)), on(c,(1,1)), on(d,(4,2))
]).

state2([
    occupied((1,1)), occupied((1,2)), clear((1,3)), clear((1,4)),
    occupied((2,1)), occupied((2,2)), clear((2,3)), clear((2,4)),
    clear((3,1)), clear((3,2)), clear((3,3)), clear((3,4)),
    occupied((4,1)), clear((4,2)), clear((4,3)), clear((4,4)),
    clear((5,1)), clear((5,2)), clear((5,3)), clear((5,4)),
    occupied((6,1)), clear((6,2)), clear((6,3)), clear((6,4)),
    occupied((1,0)), occupied((2,0)), occupied((3,0)), occupied((4,0)), 
    occupied((5,0)), occupied((6,0)),
    clear((1,5)), clear((2,5)), clear((3,5)), clear((4,5)), 
    clear((5,5)), clear((6,5)),
    on(a,(1,2)), on(b,(6,1)), on(c,(1,1)), on(d,(3,1))
]).

goal1([
    on(a,(1,2)), on(b,(6,1)), on(c,(1,1)), on(d,(3,1))
]).

goal2a([
    on(a,(1,2)), on(b,(6,1)), on(c,(2,1)), on(d,(3,1))
]).

goal2b([
    on(a,(1,2)), on(b,(6,1)), on(c,(2,1)), on(d,(3,2))
]).

% Movimentos
action(move1(Block, From, To)) :- block(Block, 1), place(From), place(To), From \== To.
action(move2(Block, From, To)) :- block(Block, 2), place(From), place(To), From \== To.
action(move3(Block, From, To)) :- block(Block, 3), place(From), place(To), From \== To.

% Regras de movimento
can(move1(Block, From, To), [on(Block,From)|Conditions]) :-
    block(Block, 1),
    place(To),
    stable(To, 1, OccpList),
    place(From),
    From \== To,
    \+ above_itself(From,1,OccpList),
    clear_above(From, 1, [], ClearList),
    append([clear(To)|ClearList], OccpList, Conditions).

can(move2(Block, From, To), [on(Block,From)|Conditions]) :-
    block(Block, 2),
    valid_region(To, 2),
    stable(To, 2, OccpList),
    place(From),
    From \== To,
    \+ above_itself(From,2,OccpList),
    clear_above(From, 2, [], ClearList1),
    clear_positions(To, 2, [], ClearList2),
    append(ClearList1, ClearList2, ClearList),
    append(ClearList, OccpList, Conditions).

can(move3(Block, From, To), [on(Block,From)|Conditions]) :-
    block(Block, 3),
    valid_region(To, 3),
    stable(To, 3, OccpList),
    place(From),
    From \== To,
    \+ above_itself(From,3,OccpList),
    clear_above(From, 3, [], ClearList1),
    clear_positions(To, 3, [], ClearList2),
    append(ClearList1, ClearList2, ClearList),
    append(ClearList, OccpList, Conditions).

% Efeitos dos movimentos
adds(move1(Block, From, To), [on(Block, To), occupied(To), clear(From)]).
adds(move2(Block, From, To), [on(Block, To)|Conditions]) :-
    occ_positions(To, 2, [], OccList),
    clear_positions(From, 2, [], ClearList),
    append(OccList, ClearList, Conditions).
adds(move3(Block, From, To), [on(Block, To)|Conditions]) :-
    occ_positions(To, 3, [], OccList),
    clear_positions(From, 3, [], ClearList),
    append(OccList, ClearList, Conditions).

deletes(move1(Block, From, To), [on(Block, From), occupied(From), clear(To)]).
deletes(move2(Block, From, To), [on(Block, From)|Conditions]) :-
    occ_positions(From, 2, [], OccList),
    clear_positions(To, 2, [], ClearList),
    append(OccList, ClearList, Conditions).
deletes(move3(Block, From, To), [on(Block, From)|Conditions]) :-
    occ_positions(From, 3, [], OccList),
    clear_positions(To, 3, [], ClearList),
    append(OccList, ClearList, Conditions).

% Predicados auxiliares
above_itself((X,Y), 1, [occupied((X,Y))]).
above_itself((Xb,Y), 2, [occupied((X,Y)),occupied((X2,Y))]) :-
    Xb2 is Xb + 1,
    (Xb == X; Xb == X2; Xb2 == X).
above_itself((Xb,Y), 3, [occupied((X,Y))|_]) :-
    Xb2 is Xb + 1,
    Xb3 is Xb2 + 1,
    (Xb == X; Xb2 == X; Xb3 == X).

occ_positions(_, 0, List, List).
occ_positions((X,Y), Size, List, OccList) :-
    Size > 0,
    X2 #= X + 1,
    Size2 is Size - 1,
    occ_positions((X2,Y), Size2, [occupied((X,Y))|List], OccList).

clear_positions(_, 0, List, List).
clear_positions((X,Y), Size, List, ClearList) :-
    Size > 0,
    X2 #= X + 1,
    Size2 is Size - 1,
    clear_positions((X2,Y), Size2, [clear((X,Y))|List], ClearList).

valid_region((X, Y), Size) :-
    X2 #= X + Size - 1,
    place((X2, Y)).

clear_above(_, 0, List, List).
clear_above((X,Y), Size, List, ClearList) :-
    Size > 0,
    X2 #= X + 1,
    Y2 #= Y + 1,
    Size2 is Size - 1,
    clear_above((X2,Y), Size2, [clear((X,Y2))|List], ClearList).

stable((X,Y), 1, [occupied((X,Y2))]) :- Y2 #= Y - 1.
stable((X,Y), 2, [occupied((X,Y2)), occupied((X2,Y2))]) :- 
    Y2 #= Y - 1, 
    X2 #= X + 1.
stable((X,Y), 3, [occupied((X2,Y2))]) :- 
    Y2 #= Y - 1, 
    X2 #= X + 1.
stable((X,Y), 3, [occupied((X,Y2)), occupied((X2,Y2))]) :- 
    Y2 #= Y - 1, 
    X2 #= X + 2.

% Predicados auxiliares do planejador
satisfied(_, []).
satisfied(State, [Goal|Goals]):-
    member(Goal, State),
    satisfied(State, Goals).

select(State, Goals, Goal):-
    member(Goal, Goals),
    \+ member(Goal, State).

achieves(Action, Goal):-
    adds(Action, Goals),
    member(Goal, Goals).

apply(State, Action, NewState):-
    deletes(Action, DelList),
    delete_all(State, DelList, State1),
    adds(Action, AddList),
    append(AddList, State1, NewState).

delete_all([], _, []).
delete_all([X|L1], L2, Diff):-
    member(X, L2), !,
    delete_all(L1, L2, Diff).
delete_all([X|L1], L2, [X|Diff]):-
    delete_all(L1, L2, Diff).

% ----------- BUSCA EM LARGURA (BFS) -----------
bfs(InitialState, Goals, Plan) :-
    bfs_queue([node(InitialState, [], [])], Goals, [], RevPlan),
    reverse(RevPlan, Plan).

% bfs_queue(Fila, Goals, Visitados, PlanoReverso)
bfs_queue([node(State, Plan, )|], Goals, _, Plan) :-
    satisfied(State, Goals), !.
bfs_queue([node(State, Plan, Path)|Rest], Goals, Visited, Solution) :-
    findall(node(NewState, [Action|Plan], [State|Path]),
        (   possible_action(State, Action),
            apply(State, Action, NewState),
            \+ member_state(NewState, [State|Path]), % evita ciclos
            \+ member_state(NewState, Visited)
        ),
        NewNodes),
    append(Rest, NewNodes, Queue),
    bfs_queue(Queue, Goals, [State|Visited], Solution).

% Gera todas as ações possíveis a partir do estado atual
possible_action(State, Action) :-
    (action(Action), can(Action, Conditions), satisfied(State, Conditions)).

% Compara estados (listas) ignorando ordem
member_state(State, [H|_]) :- equal_state(State, H), !.
member_state(State, [_|T]) :- member_state(State, T).
member_state(_, []) :- fail.

equal_state(A, B) :-
    msort(A, SA), msort(B, SB), SA == SB.

% Teste genérico: testar_bfs(NomeEstadoInicial, NomeObjetivo)
testar_bfs(EstadoInicial, Objetivo) :-
    call(EstadoInicial, S),
    call(Objetivo, G),
    (bfs(S, G, Plan) ->
        format('Plano mínimo encontrado de ~w para ~w:~n~w~n', [EstadoInicial, Objetivo, Plan])
    ;   format('Não foi possível encontrar um plano de ~w para ~w.~n', [EstadoInicial, Objetivo])
    ).

% Exemplos de uso:
% ?- testar_bfs(state1, goal1).
% ?- testar_bfs(state2, goal2a).
% ?- testar_bfs(state2, goal2b).
% ?- testar_bfs(state1, goal2a).