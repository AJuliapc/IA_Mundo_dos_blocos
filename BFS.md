### Explicação linha a linha do código Prolog

---

```prolog
:- use_module(library(clpfd)).
```
Importa a biblioteca `clpfd` (Constraint Logic Programming over Finite Domains), usada para restrições sobre inteiros, útil para manipulação de variáveis e restrições aritméticas.

---

```prolog
% Definição dos blocos
block(a, 1).
block(b, 1).
block(c, 2).
block(d, 3).
```
Define blocos nomeados `a`, `b`, `c` e `d` com seus respectivos tamanhos (1, 1, 2 e 3 unidades).

---

```prolog
% Definição das posições válidas (6x4)
place((X, Y)) :- between(1, 6, X), between(1, 4, Y).
```
Define as posições válidas no tabuleiro como pares `(X,Y)` onde `X` está entre 1 e 6 e `Y` entre 1 e 4, formando uma grade 6x4.

---

```prolog
% Exemplos de estados iniciais e objetivos
state1([...]).
state2([...]).
```
Define dois estados iniciais (`state1` e `state2`) que descrevem o estado do tabuleiro: quais posições estão ocupadas (`occupied`), livres (`clear`), e onde cada bloco está (`on(Block, Position)`).

---

```prolog
goal1([...]).
goal2a([...]).
goal2b([...]).
```
Define objetivos (estados finais desejados) para o problema, especificando onde cada bloco deve estar.

---

```prolog
% Movimentos
action(move1(Block, From, To)) :- block(Block, 1), place(From), place(To), From \== To.
action(move2(Block, From, To)) :- block(Block, 2), place(From), place(To), From \== To.
action(move3(Block, From, To)) :- block(Block, 3), place(From), place(To), From \== To.
```
Define ações possíveis de mover blocos de tamanho 1, 2 ou 3 de uma posição `From` para outra `To`, garantindo que as posições são válidas e diferentes.

---

```prolog
% Regras de movimento
can(move1(Block, From, To), [on(Block,From)|Conditions]) :- ...
can(move2(Block, From, To), [on(Block,From)|Conditions]) :- ...
can(move3(Block, From, To), [on(Block,From)|Conditions]) :- ...
```
Define as condições para que um movimento seja possível (`can`). Para cada tamanho de bloco, verifica se o destino é estável, se não está "acima de si mesmo", se as posições acima estão livres, e outras condições específicas para blocos maiores.

---

```prolog
% Efeitos dos movimentos
adds(move1(Block, From, To), [on(Block, To), occupied(To), clear(From)]).
adds(move2(Block, From, To), [on(Block, To)|Conditions]) :- ...
adds(move3(Block, From, To), [on(Block, To)|Conditions]) :- ...
```
Define os efeitos positivos (o que é adicionado ao estado) após executar um movimento: o bloco estará na nova posição, a posição ficará ocupada, e a antiga ficará livre.

---

```prolog
deletes(move1(Block, From, To), [on(Block, From), occupied(From), clear(To)]).
deletes(move2(Block, From, To), [on(Block, From)|Conditions]) :- ...
deletes(move3(Block, From, To), [on(Block, From)|Conditions]) :- ...
```
Define os efeitos negativos (o que é removido do estado) após o movimento: o bloco não estará mais na posição antiga, a posição antiga deixa de estar ocupada, e a nova deixa de estar livre.

---

```prolog
% Predicados auxiliares
above_itself((X,Y), 1, [occupied((X,Y))]).
above_itself((Xb,Y), 2, [occupied((X,Y)),occupied((X2,Y))]) :- ...
above_itself((Xb,Y), 3, [occupied((X,Y))|_]) :- ...
```
Define restrições para evitar que um bloco esteja "acima de si mesmo" em posições adjacentes, dependendo do tamanho do bloco.

---

```prolog
occ_positions(_, 0, List, List).
occ_positions((X,Y), Size, List, OccList) :- ...
```
Gera uma lista de posições ocupadas consecutivas horizontalmente para um bloco de determinado tamanho.

---

```prolog
clear_positions(_, 0, List, List).
clear_positions((X,Y), Size, List, ClearList) :- ...
```
Gera uma lista de posições livres consecutivas horizontalmente para um bloco de determinado tamanho.

---

```prolog
valid_region((X, Y), Size) :-
    X2 #= X + Size - 1,
    place((X2, Y)).
```
Verifica se uma região horizontal de tamanho `Size` a partir de `(X,Y)` está dentro do tabuleiro.

---

```prolog
clear_above(_, 0, List, List).
clear_above((X,Y), Size, List, ClearList) :- ...
```
Verifica se as posições acima de um bloco (na linha acima) estão livres para o tamanho do bloco.

---

```prolog
stable((X,Y), 1, [occupied((X,Y2))]) :- Y2 #= Y - 1.
stable((X,Y), 2, [occupied((X,Y2)), occupied((X2,Y2))]) :- ...
stable((X,Y), 3, [occupied((X2,Y2))]) :- ...
stable((X,Y), 3, [occupied((X,Y2)), occupied((X2,Y2))]) :- ...
```
Define as condições para que uma posição seja estável para blocos de tamanhos 1, 2 e 3, ou seja, que tenha suporte abaixo (ocupado).

---

```prolog
% Predicados auxiliares do planejador
satisfied(_, []).
satisfied(State, [Goal|Goals]) :- ...
```
Verifica se um estado satisfaz uma lista de condições (objetivos).

---

```prolog
select(State, Goals, Goal) :- ...
achieves(Action, Goal) :- ...
apply(State, Action, NewState) :- ...
delete_all([], _, []).
delete_all([X|L1], L2, Diff) :- ...
```
Predicados para selecionar objetivos não satisfeitos, verificar se uma ação alcança um objetivo, aplicar uma ação a um estado (removendo e adicionando fatos), e remover elementos de uma lista.

---

```prolog
% ----------- BUSCA EM LARGURA (BFS) -----------
bfs(InitialState, Goals, Plan) :- ...
bfs_queue([node(State, Plan, _)|_], Goals, _, Plan) :- ...
bfs_queue([node(State, Plan, Path)|Rest], Goals, Visited, Solution) :- ...
```
Implementa busca em largura para encontrar um plano (sequência de ações) que leva do estado inicial ao objetivo, evitando ciclos e estados já visitados.

---

```prolog
% Gera todas as ações possíveis a partir do estado atual
possible_action(State, Action) :-
    (action(Action), can(Action, Conditions), satisfied(State, Conditions)).
```
Gera todas as ações possíveis que podem ser executadas no estado atual, verificando se as condições para a ação são satisfeitas.

---

```prolog
% Compara estados (listas) ignorando ordem
member_state(State, [H|_]) :- equal_state(State, H), !.
member_state(State, [_|T]) :- member_state(State, T).
member_state(_, []) :- fail.

equal_state(A, B) :-
    msort(A, SA), msort(B, SB), SA == SB.
```
Compara estados ignorando a ordem dos elementos, para evitar considerar estados iguais como diferentes.

---

```prolog
% Teste genérico: testar_bfs(NomeEstadoInicial, NomeObjetivo)
testar_bfs(EstadoInicial, Objetivo) :-
    call(EstadoInicial, S),
    call(Objetivo, G),
    (bfs(S, G, Plan) ->
        format('Plano mínimo encontrado de ~w para ~w:~n~w~n', [EstadoInicial, Objetivo, Plan])
    ;   format('Não foi possível encontrar um plano de ~w para ~w.~n', [EstadoInicial, Objetivo])
    ).
```
Predicado para testar a busca em largura entre um estado inicial e um objetivo, exibindo o plano encontrado ou mensagem de falha.

---

```prolog
% Exemplos de uso:
% ?- testar_bfs(state1, goal1).
% ?- testar_bfs(state2, goal2a).
% ?- testar_bfs(state2, goal2b).
% ?- testar_bfs(state1, goal2a).
```
Exemplos de consultas para testar o planejador com diferentes estados e objetivos.

---

### Resumo geral

O código implementa um planejador baseado em busca em largura para mover blocos de diferentes tamanhos em um tabuleiro 6x4, respeitando regras de estabilidade e ocupação. Ele define estados, ações possíveis, condições para mover blocos, efeitos das ações, e um mecanismo para buscar um plano que transforma um estado inicial em um estado objetivo.

---

### Saída do código 

```prolog 
testar_bfs(state1, goal1).
```

```smv 
Plano mínimo encontrado de state1 para goal1:
[move3(d,(4,2),(1,2)),move1(a,(4,1),(6,2)),move3(d,(1,2),(3,1)),move1(a,(6,2),(1,2))]
true

4.445 seconds cpu time
```

```prolog
testar_bfs(state2, goal2a).
```

```smv
Stack limit (0.2Gb) exceeded
  Stack sizes: local: 3Kb, global: 0.2Gb, trail: 3Kb
  Stack depth: 12,469, last-call: 100%, Choice points: 13
  In:
    [12,469] lists:append([length:809], [length:2], _1946)
    [2,479] bfs_queue([length:10,799], [length:4], [length:2,449], _1988)
    [29] bfs('<garbage_collected>', '<garbage_collected>', _2034)
    [28] testar_bfs(state2, goal2a)
    [27] swish_trace:swish_call('<garbage_collected>')

Use the --stack_limit=size[KMG] command line option or
?- set_prolog_flag(stack_limit, 2_147_483_648). to double the limit.
```
