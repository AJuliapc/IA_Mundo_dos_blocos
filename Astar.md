### Explicação linha a linha do código Prolog

---

```prolog
:- use_module(library(clpfd)).
```
Importa a biblioteca `clpfd` para usar restrições sobre inteiros, útil para cálculos e restrições aritméticas.

---

```prolog
% Definição dos blocos
block(a, 1).
block(b, 1).
block(c, 2).
block(d, 3).
```
Define quatro blocos nomeados `a`, `b`, `c` e `d` com seus respectivos tamanhos (1, 1, 2 e 3 unidades).

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
goal1([...]).
goal2a([...]).
goal2b([...]).
```
Define estados iniciais (`state1`, `state2`) e objetivos (`goal1`, `goal2a`, `goal2b`). Cada estado é uma lista de fatos que indicam quais posições estão ocupadas (`occupied`), livres (`clear`), e onde cada bloco está (`on(Block, Position)`).

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
% Gera todas as ações possíveis a partir do estado atual
possible_action(State, Action) :-
    action(Action),
    can(Action, Conditions),
    satisfied(State, Conditions).
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
% --- Heurística: soma das distâncias Manhattan dos blocos até suas posições alvo ---
heuristic(State, Goals, H) :-
    findall(Dist, (
        member(on(Block, Pos1), State),
        member(on(Block, Pos2), Goals),
        manhattan(Pos1, Pos2, Dist)
    ), Dists),
    sum_list(Dists, H).

manhattan((X1,Y1), (X2,Y2), D) :-
    D #= abs(X1 - X2) + abs(Y1 - Y2).
```
Define uma heurística para a busca A* que calcula a soma das distâncias Manhattan entre a posição atual e a posição objetivo de cada bloco.

---

```prolog
% --- Predicado auxiliar para extrair estados da lista de nós ---
nodes_states([], []).
nodes_states([node(State, _, _, _) | Ns], [State | Ss]) :-
    nodes_states(Ns, Ss).
```
Extrai os estados da lista de nós para facilitar a verificação de estados já abertos.

---

```prolog
% --- Busca A* ---
astar(InitialState, Goals, Plan) :-
    heuristic(InitialState, Goals, H),
    astar_queue([node(InitialState, [], 0, H)], Goals, [], RevPlan),
    reverse(RevPlan, Plan).
```
Inicia a busca A* com o estado inicial, calculando a heurística e chamando o predicado principal da fila.

---

```prolog
% astar_queue(Fila, Goals, Visitados, PlanoReverso)
astar_queue(Open, Goals, Closed, Plan) :-
    select_best_node(Open, node(State, PlanSoFar, G, _), RestOpen),
    (satisfied(State, Goals) ->
        Plan = PlanSoFar
    ;
        nodes_states(Open, OpenStates),
        findall(node(NewState, [Action|PlanSoFar], G1, H1),
            (possible_action(State, Action),
             apply(State, Action, NewState),
             \+ member_state(NewState, Closed),
             \+ member_state(NewState, OpenStates),
             G1 is G + 1,
             heuristic(NewState, Goals, H1)
            ),
            Children),
        append(RestOpen, Children, NewOpen),
        astar_queue(NewOpen, Goals, [State|Closed], Plan)
    ).
```
Executa a busca A*:
- Seleciona o nó com menor custo `f = g + h`.
- Se o estado satisfaz o objetivo, retorna o plano.
- Caso contrário, gera os filhos (novos estados) aplicando ações possíveis, evitando estados já visitados ou abertos.
- Atualiza a fila e continua a busca.

---

```prolog
% Seleciona o nó com menor f = g + h da lista
select_best_node([N], N, []) :- !.
select_best_node([N1,N2|Ns], Best, Rest) :-
    f_value(N1, F1),
    f_value(N2, F2),
    (F1 =< F2 ->
        select_best_node([N1|Ns], Best, Rest1),
        Rest = [N2|Rest1]
    ;
        select_best_node([N2|Ns], Best, Rest1),
        Rest = [N1|Rest1]
    ).

f_value(node(_,_,G,H), F) :- F is G + H.
```
Seleciona o nó com menor custo `f` da lista de nós abertos, para expandir primeiro.

---

```prolog
% Teste genérico: testar_astar(NomeEstadoInicial, NomeObjetivo)
testar_astar(EstadoInicial, Objetivo) :-
    call(EstadoInicial, S),
    call(Objetivo, G),
    (astar(S, G, Plan) ->
        format('Plano mínimo encontrado de ~w para ~w:~n~w~n', [EstadoInicial, Objetivo, Plan])
    ;   format('Não foi possível encontrar um plano de ~w para ~w.~n', [EstadoInicial, Objetivo])
    ).
```
Predicado para testar a busca A* entre um estado inicial e um objetivo, exibindo o plano encontrado ou mensagem de falha.

---

```prolog
% Exemplos de uso:
% ?- testar_astar(state1, goal1).
% ?- testar_astar(state2, goal2a).
% ?- testar_astar(state2, goal2b).
```
Exemplos de consultas para testar o planejador com diferentes estados e objetivos.

---

### Resumo geral

O código implementa um planejador para mover blocos em um tabuleiro 6x4, usando busca A* com heurística de distância Manhattan para encontrar planos mínimos que transformam um estado inicial em um estado objetivo, respeitando regras de estabilidade e ocupação.

### Saída do código 

```prolog 
testar_astar(state1, goal1).
```

```smv 
[move3(d,(4,2),(1,2)),move1(a,(4,1),(6,2)),move3(d,(1,2),(3,1)),move1(a,(6,2),(1,2))]
true

0.162 seconds cpu timePlano mínimo encontrado de state1 para goal1:
```

```prolog
testar_astar(state2, goal2a).
```

### Análise dos Resultados

#### 1. `testar_astar(state1, goal1)`

```
testar_astar(state1, goal1).
Plano mínimo encontrado de state1 para goal1:
[move3(d,(4,2),(1,2)),move1(a,(4,1),(6,2)),move3(d,(1,2),(3,1)),move1(a,(6,2),(1,2))]
1true
0.162 seconds cpu time
```

- **Resultado**: Um plano mínimo foi encontrado com sucesso.
- **Plano**:
  1. `move3(d,(4,2),(1,2))`: Move o bloco `d` (tamanho 3) da posição `(4,2)` para `(1,2)`.
  2. `move1(a,(4,1),(6,2))`: Move o bloco `a` (tamanho 1) da posição `(4,1)` para `(6,2)`.
  3. `move3(d,(1,2),(3,1))`: Move o bloco `d` (tamanho 3) da posição `(1,2)` para `(3,1)`.
  4. `move1(a,(6,2),(1,2))`: Move o bloco `a` (tamanho 1) da posição `(6,2)` para `(1,2)`.
- **Interpretação**:
  - O plano mostra a sequência de movimentos necessários para transformar `state1` em `goal1`.
  - O tempo de CPU (0.162 segundos) indica a eficiência da busca para este caso.

#### 2. `testar_astar(state2, goal2a)`

```
testar_astar(state2, goal2a).
Não foi possível encontrar um plano de state2 para goal2a.
1true
```

- **Resultado**: Nenhum plano foi encontrado.
- **Interpretação**:
  - A busca A* não conseguiu encontrar uma sequência de movimentos que transformasse `state2` em `goal2a`.
  - Isso pode ocorrer por vários motivos:
    1. **Inexistência de Solução**: Pode não existir uma sequência de movimentos válida que leve de `state2` para `goal2a` respeitando as restrições do problema.
    2. **Heurística Subótima**: A heurística utilizada (distância Manhattan) pode não ser suficientemente informativa para guiar a busca de forma eficiente, levando a exploração de caminhos irrelevantes.
    3. **Limitações do Algoritmo**: A* pode ter dificuldades em certos tipos de problemas, especialmente se o espaço de busca for muito grande ou a heurística não for bem projetada.

#### Possíveis Causas para a Falha em `state2, goal2a`

1. **Restrições de Movimento**:
   - `state2` tem blocos mais agrupados, o que pode dificultar a movimentação de blocos maiores como `c` (tamanho 2) e `d` (tamanho 3).
   - As regras de estabilidade e posições livres podem impedir certos movimentos.

2. **Heurística**:
   - A distância Manhattan pode não capturar a complexidade real do problema. Por exemplo, pode ser necessário mover um bloco para longe do objetivo temporariamente para liberar espaço para outros blocos.
   - Uma heurística mais sofisticada poderia considerar o número de blocos bloqueando o caminho, ou a necessidade de liberar posições específicas.

3. **Complexidade do Espaço de Busca**:
   - A combinação de `state2` e `goal2a` pode gerar um espaço de busca muito grande, onde a A* não consegue encontrar uma solução em tempo razoável.

Em resumo, enquanto a A* conseguiu encontrar um plano para `state1, goal1` de forma eficiente, a falha em encontrar um plano para `state2, goal2a` sugere que o problema é mais complexo e pode exigir uma heurística mais sofisticada ou um algoritmo de busca diferente.

```smv
Não foi possível encontrar um plano de state2 para goal2a.
true
```
