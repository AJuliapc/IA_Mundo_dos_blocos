:- use_module(library(clpfd)).
:- use_module(library(heaps)).

% Definição dos blocos
block(a, 1).
block(b, 1).
block(c, 2).
block(d, 3).

% Definição das posições válidas (6x4)
place((X, Y)) :- between(1, 6, X), between(1, 4, Y).

% Exemplos de estados iniciais e objetivos 

%i1
state1([ 
    occupied((1,1)), clear((1,2)), clear((1,3)), clear((1,4)),
    occupied((2,1)), clear((2,2)), clear((2,3)), clear((2,4)),
    clear((3,1)), clear((3,2)), clear((3,3)), clear((3,4)),
    occupied((4,1)), occupied((4,2)), clear((4,3)), clear((4,4)),
    clear((5,1)), occupied((5,2)), clear((5,3)), clear((5,4)),
    occupied((6,1)), occupied((6,2)), clear((6,3)), clear((6,4)),
    occupied((1,0)), occupied((2,0)), occupied((3,0)), occupied((4,0)), 
    occupied((5,0)), occupied((6,0)),
    on(a,(4,1)), on(b,(6,1)), on(c,(1,1)), on(d,(4,2))
]).

%i2
state2([
    occupied((1,1)), occupied((1,2)), clear((1,3)), clear((1,4)),
    occupied((2,1)), clear((2,2)), clear((2,3)), clear((2,4)),
    occupied((3,1)), clear((3,2)), clear((3,3)), clear((3,4)),
    occupied((4,1)), clear((4,2)), clear((4,3)), clear((4,4)),
    occupied((5,1)), clear((5,2)), clear((5,3)), clear((5,4)),
    occupied((6,1)), clear((6,2)), clear((6,3)), clear((6,4)),
    occupied((1,0)), occupied((2,0)), occupied((3,0)), occupied((4,0)), 
    occupied((5,0)), occupied((6,0)),
    on(a,(1,2)), on(b,(6,1)), on(c,(1,1)), on(d,(3,1))
]).

%status 2
status2([
    occupied((1,1)), occupied((1,2)), clear((1,3)), clear((1,4)),
    occupied((2,1)), occupied((2,2)), clear((2,3)), clear((2,4)),
    clear((3,1)), clear((3,2)), clear((3,3)), clear((3,4)),
    occupied((4,1)), clear((4,2)), clear((4,3)), clear((4,4)),
    occupied((5,1)), clear((5,2)), clear((5,3)), clear((5,4)),
    occupied((6,1)), clear((6,2)), clear((6,3)), clear((6,4)),
    occupied((1,0)), occupied((2,0)), occupied((3,0)), occupied((4,0)), 
    occupied((5,0)), occupied((6,0)),
    on(a,(1,2)), on(b,(2,2)), on(c,(1,1)), on(d,(4,1))
]).

%status 3 e 4
status3e4([
    occupied((1,1)), clear((1,2)), clear((1,3)), clear((1,4)),
    occupied((2,1)), clear((2,2)), clear((2,3)), clear((2,4)),
    clear((3,1)), clear((3,2)), clear((3,3)), clear((3,4)),
    occupied((4,1)), occupied((4,2)), clear((4,3)), clear((4,4)),
    clear((5,1)), occupied((5,2)), clear((5,3)), clear((5,4)),
    occupied((6,1)), occupied((6,2)), clear((6,3)), clear((6,4)),
    occupied((1,0)), occupied((2,0)), occupied((3,0)), occupied((4,0)), 
    occupied((5,0)), occupied((6,0)),
    on(a,(4,1)), on(b,(6,1)), on(c,(1,1)), on(d,(4,2))
]).

%i2
goali2([
    on(a,(1,2)), on(b,(6,1)), on(c,(1,1)), on(d,(3,1))
]).

%a
goalia([
    on(a,(1,2)), on(b,(6,1)), on(c,(2,1)), on(d,(3,1))
]).

%b
goalib([
    on(a,(5,3)), on(b,(6,3)), on(c,(5,2)), on(d,(4,1))
]).

%c
goalic([
    on(a,(3,1)), on(b,(6,1)), on(c,(1,1)), on(d,(1,2))
]).

%goalstatus2
goalstatus2([
    on(a,(5,3)), on(b,(6,3)), on(c,(5,2)), on(d,(4,1))
]).

%goalstatus3
goalstatus3([
    on(a,(5,2)), on(b,(3,3)), on(c,(3,2)), on(d,(3,1))
]).

%goalstatus4
goalstatus4([
    on(a,(5,1)), on(b,(6,1)), on(c,(3,1)), on(d,(4,2))
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

% ----------- HEURÍSTICAS -----------

% 1. Blocos fora do lugar
h_blocks_out_of_place(State, Goals, Value) :-
    findall(1, (member(on(Block,Pos), Goals), \+ member(on(Block,Pos), State)), L),
    length(L, Value).

% 2. Distância Manhattan dos blocos
h_manhattan_distance(State, Goals, Value) :-
    findall(Dist, (
        member(on(Block,Pos1), State),
        member(on(Block,Pos2), Goals),
        Pos1 \= Pos2,
        Pos1 = (X1,Y1),
        Pos2 = (X2,Y2),
        Block \= _,
        Dist is abs(X2-X1) + abs(Y2-Y1)
    ), Distances),
    sum_list(Distances, Value).

% 3. Blocos bloqueados (há algo acima deles)
h_blocking_blocks(State, Goals, Value) :-
    findall(1, (
        member(on(Block, (X, Y)), State),
        member(on(Block, GoalPos), Goals),
        (X, Y) \= GoalPos,
        Y1 is Y + 1,
        member(occupied((X, Y1)), State)
    ), L),
    length(L, Value).

% 4. Blocos na pilha errada (coluna errada)
h_wrong_stack(State, Goals, Value) :-
    findall(1, (
        member(on(Block, (X1, _)), State),
        member(on(Block, (X2, _)), Goals),
        X1 \= X2
    ), L),
    length(L, Value).

% 5. Blocos soltos (deveriam estar empilhados, mas estão sozinhos)
h_isolated_blocks(State, Goals, Value) :-
    findall(1, (
        member(on(Block, (X, Y)), State),
        member(on(Block, (X, Yg)), Goals),
        Yg > 1, % deveria estar empilhado
        Y = 1   % está sozinho
    ), L),
    length(L, Value).

% 6. Blocos na ordem errada na pilha
h_wrong_order(State, Goals, Value) :-
    findall(1, (
        member(on(Block, (X, Y)), State),
        member(on(Block, (X, Yg)), Goals),
        X = X, % mesma pilha
        Y \= Yg
    ), L),
    length(L, Value).

% 7. Pilhas intermediárias ocupadas (blocos em pilhas que não fazem parte do objetivo)
h_intermediate_stacks(State, Goals, Value) :-
    findall(1, (
        member(on(Block, (X, _)), State),
        \+ (member(on(Block, (X, _)), Goals))
    ), L),
    length(L, Value).

% ----------- Função de avaliação combinada -----------

h_value(State, Goals, H) :-
    h_blocks_out_of_place(State, Goals, H1),
    h_manhattan_distance(State, Goals, H2),
    h_blocking_blocks(State, Goals, H3),
    h_wrong_stack(State, Goals, H4),
    h_isolated_blocks(State, Goals, H5),
    h_wrong_order(State, Goals, H6),
    h_intermediate_stacks(State, Goals, H7),
    % Ajuste os pesos conforme necessário
    H is 3*H1 + 2*H2 + 4*H3 + 2*H4 + 2*H5 + 2*H6 + 1*H7.

% ----------- BEST-FIRST SEARCH -----------

best_first_search(InitialState, Goals, Plan) :-
    empty_heap(EmptyHeap),
    h_value(InitialState, Goals, H),
    add_to_heap(EmptyHeap, H, node(InitialState, [], []), Heap),
    best_first_search_loop(Heap, Goals, [], RevPlan),
    reverse(RevPlan, Plan).

best_first_search_loop(Heap, Goals, _, Plan) :-
    get_from_heap(Heap, _, node(State, Plan, _), _),
    satisfied(State, Goals), !.
best_first_search_loop(Heap, Goals, Visited, Solution) :-
    get_from_heap(Heap, _, node(State, Plan, Path), RestHeap),
    findall(h(H, node(NewState, [Action|Plan], [State|Path])),
        (   possible_action(State, Action),
            apply(State, Action, NewState),
            \+ member_state(NewState, [State|Path]),
            \+ member_state(NewState, Visited),
            h_value(NewState, Goals, H)
        ),
        NewNodes),
    add_nodes_to_heap(RestHeap, NewNodes, NewHeap),
    best_first_search_loop(NewHeap, Goals, [State|Visited], Solution).

add_nodes_to_heap(Heap, [], Heap).
add_nodes_to_heap(Heap, [h(H,Node)|Rest], FinalHeap) :-
    add_to_heap(Heap, H, Node, NextHeap),
    add_nodes_to_heap(NextHeap, Rest, FinalHeap).

possible_action(State, Action) :-
    action(Action), can(Action, Conditions), satisfied(State, Conditions).

member_state(State, [H|_]) :- equal_state(State, H), !.
member_state(State, [_|T]) :- member_state(State, T).
member_state(_, []) :- fail.

equal_state(A, B) :-
    msort(A, SA), msort(B, SB), SA == SB.

% ----------- TESTE -----------

testar_best_first(EstadoInicial, Objetivo) :-
    call(EstadoInicial, S),
    call(Objetivo, G),
    statistics(runtime, [Start|_]),
    (best_first_search(S, G, Plan) ->
        statistics(runtime, [End|_]),
        Time is End - Start,
        format('Plano encontrado de ~w para ~w em ~w ms:~n~w~n', 
               [EstadoInicial, Objetivo, Time, Plan])
    ;   format('Não foi possível encontrar um plano de ~w para ~w.~n', 
               [EstadoInicial, Objetivo])
    ).

% Exemplos de uso:

% Testes com state1:
%?- testar_best_first(state1, goali2).
%?- testar_best_first(state1, goalib).
%?- testar_best_first(state1, goalic).
%?- testar_best_first(state1, goalstatus2).
%?- testar_best_first(state1, goalstatus3).
%?- testar_best_first(state1, goalstatus4).

% Testes com state2:
%?- testar_best_first(state2, goali2).
%?- testar_best_first(state2, goalib).
%?- testar_best_first(state2, goalic).
%?- testar_best_first(state2, goalstatus2).
%?- testar_best_first(state2, goalstatus3).
%?- testar_best_first(state2, goalstatus4).

% Testes com status2:
%?- testar_best_first(status2, goali2).
%?- testar_best_first(status2, goalia).
%?- testar_best_first(status2, goalib).
%?- testar_best_first(status2, goalic).
%?- testar_best_first(status2, goalstatus2).
%?- testar_best_first(status2, goalstatus3).
%?- testar_best_first(status2, goalstatus4).

% Testes com status3e4:
%?- testar_best_first(status3e4, goali2).
%?- testar_best_first(status3e4, goalia).
%?- testar_best_first(status3e4, goalib).
%?- testar_best_first(status3e4, goalic).
%?- testar_best_first(status3e4, goalstatus2).
%?- testar_best_first(status3e4, goalstatus3).
%?- testar_best_first(status3e4, goalstatus4).
