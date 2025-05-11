### Explicação linha a linha do código Prolog

---

```prolog
:- use_module(library(clpfd)).
:- use_module(library(heaps)).
```
Importa as bibliotecas `clpfd` (restrições sobre inteiros) e `heaps` (estrutura de dados heap para busca eficiente).

---

```prolog
% Definição dos blocos
block(a, 1).
block(b, 1).
block(c, 2).
block(d, 3).
```
Define quatro blocos nomeados `a`, `b`, `c` e `d` com seus respectivos tamanhos (1, 1, 2 e 3).

---

```prolog
% Definição das posições válidas (6x4)
place((X, Y)) :- between(1, 6, X), between(1, 4, Y).
```
Define as posições válidas no tabuleiro como pares `(X,Y)` onde `X` está entre 1 e 6 e `Y` entre 1 e 4.

---

```prolog
% Exemplos de estados iniciais e objetivos
state1([...]).
state2([...]).
status2([...]).
status3e4([...]).
goali2([...]).
goalia([...]).
goalib([...]).
goalic([...]).
goalstatus2([...]).
goalstatus3([...]).
goalstatus4([...]).
```
Define vários estados iniciais e objetivos, cada um representando a ocupação do tabuleiro e a posição dos blocos.

---

```prolog
% Movimentos
action(move1(Block, From, To)) :- ...
action(move2(Block, From, To)) :- ...
action(move3(Block, From, To)) :- ...
```
Define ações possíveis para mover blocos de tamanhos 1, 2 e 3 entre posições válidas diferentes.

---

```prolog
% Regras de movimento (can)
can(move1(...), Conditions) :- ...
can(move2(...), Conditions) :- ...
can(move3(...), Conditions) :- ...
```
Define as condições para que um movimento seja possível, incluindo estabilidade, posições livres, e restrições específicas para cada tamanho de bloco.

---

```prolog
% Efeitos dos movimentos (adds e deletes)
adds(move1(...), ...).
adds(move2(...), ...).
adds(move3(...), ...).
deletes(move1(...), ...).
deletes(move2(...), ...).
deletes(move3(...), ...).
```
Define o que é adicionado e removido do estado após executar um movimento.

---

```prolog
% Predicados auxiliares para verificar posições acima, ocupadas, livres, estabilidade, etc.
above_itself(...).
occ_positions(...).
clear_positions(...).
valid_region(...).
clear_above(...).
stable(...).
```
Predicados que verificam condições geométricas e de ocupação no tabuleiro para validar movimentos.

---

```prolog
% Predicados do planejador para verificar satisfação de objetivos, aplicar ações, etc.
satisfied(...).
select(...).
achieves(...).
apply(...).
delete_all(...).
```
Manipulam estados e verificam se objetivos estão satisfeitos.

---

```prolog
% Heurísticas para avaliação do estado
h_blocks_out_of_place(...).
h_manhattan_distance(...).
h_blocking_blocks(...).
h_wrong_stack(...).
h_isolated_blocks(...).
h_wrong_order(...).
h_intermediate_stacks(...).
```
Define várias heurísticas que avaliam o quão "distante" um estado está do objetivo, considerando blocos fora do lugar, distância, bloqueios, pilhas erradas, etc.

---

```prolog
% Função de avaliação combinada
h_value(State, Goals, H) :-
    h_blocks_out_of_place(State, Goals, H1),
    h_manhattan_distance(State, Goals, H2),
    h_blocking_blocks(State, Goals, H3),
    h_wrong_stack(State, Goals, H4),
    h_isolated_blocks(State, Goals, H5),
    h_wrong_order(State, Goals, H6),
    h_intermediate_stacks(State, Goals, H7),
    H is 3*H1 + 2*H2 + 4*H3 + 2*H4 + 2*H5 + 2*H6 + 1*H7.
```
Combina as heurísticas com pesos para calcular um valor único que guia a busca.

---

```prolog
% Busca A* usando heap
astar_search(InitialState, Goals, Plan) :-
    empty_heap(EmptyHeap),
    h_value(InitialState, Goals, H),
    add_to_heap(EmptyHeap, H, node(InitialState, [], 0, []), Heap),
    astar_search_loop(Heap, Goals, [], RevPlan),
    reverse(RevPlan, Plan).
```
Inicializa a busca A* com o estado inicial e sua heurística.

---

```prolog
astar_search_loop(Heap, Goals, _, Plan) :-
    get_from_heap(Heap, _, node(State, Plan, _, _), _),
    satisfied(State, Goals), !.
astar_search_loop(Heap, Goals, Visited, Solution) :-
    get_from_heap(Heap, _, node(State, Plan, G, Path), RestHeap),
    findall(h(F, node(NewState, [Action|Plan], G1, [State|Path])),
        (possible_action(State, Action),
         apply(State, Action, NewState),
         \+ member_state(NewState, [State|Path]),
         \+ member_state(NewState, Visited),
         h_value(NewState, Goals, H),
         G1 is G + 1,
         F is G1 + H
        ),
        NewNodes),
    add_nodes_to_heap(RestHeap, NewNodes, NewHeap),
    astar_search_loop(NewHeap, Goals, [State|Visited], Solution).
```
Loop principal da busca A*:
- Remove o nó com menor custo `f = g + h`.
- Se o estado satisfaz o objetivo, retorna o plano.
- Caso contrário, expande ações possíveis, calcula heurísticas, adiciona novos nós ao heap e continua.

---

```prolog
add_nodes_to_heap(Heap, [], Heap).
add_nodes_to_heap(Heap, [h(H,Node)|Rest], FinalHeap) :-
    add_to_heap(Heap, H, Node, NextHeap),
    add_nodes_to_heap(NextHeap, Rest, FinalHeap).
```
Adiciona uma lista de nós ao heap.

---

```prolog
possible_action(State, Action) :-
    action(Action), can(Action, Conditions), satisfied(State, Conditions).
```
Gera ações possíveis a partir do estado atual.

---

```prolog
member_state(State, [H|_]) :- equal_state(State, H), !.
member_state(State, [_|T]) :- member_state(State, T).
member_state(_, []) :- fail.

equal_state(A, B) :-
    msort(A, SA), msort(B, SB), SA == SB.
```
Verifica se um estado está em uma lista, ignorando a ordem dos elementos.

---

```prolog
% Predicado para testar a busca A*
testar_astar(EstadoInicial, Objetivo) :-
    call(EstadoInicial, S),
    call(Objetivo, G),
    statistics(runtime, [Start|_]),
    (astar_search(S, G, Plan) ->
        statistics(runtime, [End|_]),
        Time is End - Start,
        format('Plano encontrado de ~w para ~w em ~w ms:~n~w~n', 
               [EstadoInicial, Objetivo, Time, Plan])
    ;   format('Não foi possível encontrar um plano de ~w para ~w.~n', 
               [EstadoInicial, Objetivo])
    ).
```
Executa a busca A* para um estado inicial e objetivo, mostrando o plano e o tempo gasto, ou mensagem de falha.

---

```prolog
% Exemplos de uso:

% Testes com state1:
%?- testar_astar(state1, goali2).
%?- testar_astar(state1, goalib).
%?- testar_astar(state1, goalic).
%?- testar_astar(state1, goalstatus2).
%?- testar_astar(state1, goalstatus3).
%?- testar_astar(state1, goalstatus4).

% Testes com state2:
%?- testar_astar(state2, goali2).
%?- testar_astar(state2, goalia).
%?- testar_astar(state2, goalib).
%?- testar_astar(state2, goalic).
%?- testar_astar(state2, goalstatus2).
%?- testar_astar(state2, goalstatus3).
%?- testar_astar(state2, goalstatus4).

% Testes com status2:
%?- testar_astar(status2, goali2).
%?- testar_astar(status2, goalia).
%?- testar_astar(status2, goalib).
%?- testar_astar(status2, goalic).
%?- testar_astar(status2, goalstatus2).
%?- testar_astar(status2, goalstatus3).
%?- testar_astar(status2, goalstatus4).

% Testes com status3e4:
%?- testar_astar(status3e4, goali2).
%?- testar_astar(status3e4, goalia).
%?- testar_astar(status3e4, goalib).
%?- testar_astar(status3e4, goalic).
%?- testar_astar(status3e4, goalstatus2).
%?- testar_astar(status3e4, goalstatus3).
%?- testar_astar(status3e4, goalstatus4).
```
Exemplos de consultas para testar o planejador com diferentes estados e objetivos.

---

### Resumo geral

Este código implementa um planejador para mover blocos em um tabuleiro 6x4, usando busca A* guiada por uma heurística combinada que avalia a qualidade dos estados. Ele define estados, ações, regras de movimento, efeitos, heurísticas detalhadas e um mecanismo eficiente de busca com heap para encontrar planos mínimos que transformam um estado inicial em um objetivo.

### Saída do código 

```prolog 
testar_astar(state1, goali2).
```

```smv 
Plano encontrado de situacao2 para goalib em 688 ms:
[move1(a,(1,2),(5,2)),move1(b,(2,2),(3,1)),move1(a,(5,2),(6,2)),move1(b,(3,1),(6,3)),move2(c,(1,1),(4,2)),move2(c,(4,2),(2,1)),move1(b,(6,3),(5,2)),move1(a,(6,2),(5,3)),move1(a,(5,3),(4,2)),move1(b,(5,2),(3,2)),move1(b,(3,2),(1,1)),move2(c,(2,1),(5,2)),move1(b,(1,1),(6,3)),move1(a,(4,2),(5,3))]
true

0.697 seconds cpu time
```

```prolog
testar_astar(state1, goalib).
```

```smv
Plano encontrado de state1 para goalib em 7028 ms:
[move3(d,(4,2),(1,2)),move1(a,(4,1),(6,2)),move3(d,(1,2),(3,1)),move1(a,(6,2),(2,2)),move3(d,(3,1),(1,3)),move1(b,(6,1),(3,1)),move3(d,(1,3),(4,1)),move1(a,(2,2),(4,2)),move2(c,(1,1),(5,2)),move1(b,(3,1),(6,3)),move1(a,(4,2),(5,3))]
true

7.048 seconds cpu time
```

```prolog
testar_astar(state1, goalic).
```

```smv
Plano encontrado de state1 para goalic em 19 ms:
[move3(d,(4,2),(1,2)),move1(a,(4,1),(3,1))]
true

0.019 seconds cpu time
```

```prolog
testar_astar(state1, goalstatus2).
```

```smv
Plano encontrado de state1 para goalstatus2 em 7153 ms:
[move3(d,(4,2),(1,2)),move1(a,(4,1),(6,2)),move3(d,(1,2),(3,1)),move1(a,(6,2),(2,2)),move3(d,(3,1),(1,3)),move1(b,(6,1),(3,1)),move3(d,(1,3),(4,1)),move1(a,(2,2),(4,2)),move2(c,(1,1),(5,2)),move1(b,(3,1),(6,3)),move1(a,(4,2),(5,3))]
true

7.153 seconds cpu time
```

```prolog
testar_astar(state1, goalstatus3).
```

```smv
Plano encontrado de state1 para goalstatus3 em 1651 ms:
[move3(d,(4,2),(1,2)),move1(a,(4,1),(6,2)),move3(d,(1,2),(3,1)),move1(a,(6,2),(5,2)),move2(c,(1,1),(3,2)),move1(b,(6,1),(3,3))]
true

1.651 seconds cpu time
```

```prolog
testar_astar(state1, goalstatus4).
```

```smv
Plano encontrado de state1 para goalstatus4 em 77 ms:
[move3(d,(4,2),(1,2)),move1(a,(4,1),(5,1)),move3(d,(1,2),(4,2)),move2(c,(1,1),(3,1))]
true

0.077 seconds cpu time
```

Todos os outros estados e metas (goals) foram testados, mas para não extender a quantidade de saídas obtidas e demonstradas, acima é exposto apenas alguns casos. Frisa-se que de todas as metas (goals), apenas uma não foi possível compilar, pois excedeu o limite de memória durante sua execução, que foi o caso de qualquer estado com a meta de ir para goalia. A saída obtida nesse caso é:

```prolog
testar_astar(status2, goalia).
```

```smv
Time limit exceeded
```


