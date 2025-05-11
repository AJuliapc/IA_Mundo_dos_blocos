### Explicação linha a linha do código Prolog

---

```prolog
:- use_module(library(clpfd)).
:- use_module(library(heaps)).
```
Importa as bibliotecas `clpfd` (restrições sobre inteiros) e `heaps` (para manipulação de heaps, usadas na busca best-first).

---

```prolog
% Definição dos blocos
block(a, 1).
block(b, 1).
block(c, 2).
block(d, 3).
```
Define quatro blocos com seus tamanhos (1, 1, 2 e 3).

---

```prolog
% Definição das posições válidas (6x4)
place((X, Y)) :- between(1, 6, X), between(1, 4, Y).
```
Define as posições válidas no tabuleiro, formando uma grade 6x4.

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
% Busca Best-First usando heap
best_first_search(InitialState, Goals, Plan) :-
    empty_heap(EmptyHeap),
    h_value(InitialState, Goals, H),
    add_to_heap(EmptyHeap, H, node(InitialState, [], []), Heap),
    best_first_search_loop(Heap, Goals, [], RevPlan),
    reverse(RevPlan, Plan).
```
Inicializa a busca best-first com o estado inicial e sua heurística.

---

```prolog
best_first_search_loop(Heap, Goals, _, Plan) :-
    get_from_heap(Heap, _, node(State, Plan, _), _),
    satisfied(State, Goals), !.
best_first_search_loop(Heap, Goals, Visited, Solution) :-
    get_from_heap(Heap, _, node(State, Plan, Path), RestHeap),
    findall(h(H, node(NewState, [Action|Plan], [State|Path])),
        (possible_action(State, Action),
         apply(State, Action, NewState),
         \+ member_state(NewState, [State|Path]),
         \+ member_state(NewState, Visited),
         h_value(NewState, Goals, H)
        ),
        NewNodes),
    add_nodes_to_heap(RestHeap, NewNodes, NewHeap),
    best_first_search_loop(NewHeap, Goals, [State|Visited], Solution).
```
Loop principal da busca:
- Remove o nó com menor heurística.
- Se objetivo satisfeito, retorna plano.
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
% Predicado para testar a busca best-first
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
```
Executa a busca best-first para um estado inicial e objetivo, mostrando o plano e o tempo gasto, ou mensagem de falha.

---

```prolog
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
```
Exemplos de consultas para testar o planejador com diferentes estados e objetivos.

---

### Resumo geral

Este código implementa um planejador para mover blocos em um tabuleiro 6x4, usando uma busca best-first guiada por uma heurística combinada que avalia a qualidade dos estados. Ele define estados, ações, regras de movimento, efeitos, heurísticas detalhadas e um mecanismo eficiente de busca com heap para encontrar planos mínimos que transformam um estado inicial em um objetivo.

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

---

### Explicação do Estouro de Memória em `state2` vs. `state1`

O problema de estouro de memória ocorre em `state2` com `goal2a` e `goal2b` devido a **diferenças na complexidade do espaço de busca** entre esses estados e `state1` com `goal1`. Aqui estão os motivos específicos:

---

#### 1. **Complexidade dos Estados Iniciais**
- **`state1`**:
  - Blocos estão mais espalhados e há mais posições livres (`clear`).
  - Exemplo: `a` em `(4,1)`, `b` em `(6,1)`, `c` em `(1,1)`, `d` em `(4,2)`.
  - Isso limita as ações possíveis a cada passo, reduzindo o ramificação da busca.

- **`state2`**:
  - Blocos estão mais agrupados e há menos posições livres.
  - Exemplo: `a` em `(1,2)`, `b` em `(6,1)`, `c` em `(1,1)`, `d` em `(3,1)`.
  - Isso cria mais ações possíveis (e estados intermediários) porque blocos maiores (como `d` de tamanho 3) exigem mais verificações de estabilidade e posições livres adjacentes.

---

#### 2. **Natureza dos Objetivos**
- **`goal1`**:
  - Objetivo simples: mover `a` para `(1,2)` e `d` para `(3,1)`.
  - Requer poucas ações e não envolve reposicionamento complexo de blocos grandes.

- **`goal2a` e `goal2b`**:
  - Exigem mover `c` (tamanho 2) para `(2,1)` (em `goal2a`) ou `d` (tamanho 3) para `(3,2)` (em `goal2b`).
  - Blocos maiores (`c` e `d`) demandam mais verificações de:
    - Regiões válidas (`valid_region`).
    - Posições estáveis (`stable`).
    - Posições livres adjacentes (`clear_positions`).
  - Cada movimento de um bloco grande gera **mais estados intermediários** para serem armazenados na fila da BFS.

---

#### 3. **Busca em Largura (BFS) e Memória**
- A BFS armazena **todos os nós do nível atual** antes de avançar para o próximo nível.
  - Em `state1` + `goal1`, a busca é rasa (poucos níveis) e cada nível tem poucos nós.
  - Em `state2` + `goal2a/goal2b`, a busca é mais profunda e cada nível tem **muitos nós** devido às combinações de movimentos de blocos grandes.
- O estouro ocorre porque a fila da BFS cresce exponencialmente com:
  - Número de ações possíveis por estado.
  - Profundidade necessária para alcançar o objetivo.

---

#### 4. **Exemplo Prático**
Suponha que em `state2`:
- Para mover `d` (tamanho 3), o sistema deve:
  1. Verificar todas as posições `To` que são válidas para um bloco de tamanho 3.
  2. Checar se `To` está estável (exige suporte abaixo).
  3. Garantir que as posições acima de `From` estão livres (`clear_above`).
- Cada verificação gera novos estados na fila, mesmo que muitos sejam inválidos ou redundantes.

---

### Soluções Possíveis
1. **Otimizar a Representação de Estados**:
   - Usar estruturas mais compactas (como bitsets) para `occupied`/`clear`.
   - Evitar recálculo de condições repetidas (ex: cache de `stable`).

2. **Usar Busca Heurística (A*)**:
   - Priorizar nós mais próximos do objetivo com uma função heurística (ex: distância de blocos ao destino).
   - Reduz a memória porque não armazena todos os nós de um nível.

3. **Limitar a Profundidade**:
   - Implementar **iterative deepening DFS** (IDDFS), que combina BFS com DFS para controle de memória.

4. **Podar Estados Redundantes**:
   - Identificar e ignorar estados semanticamente iguais (mesma configuração de blocos, mesmo que em ordem diferente na lista).

---

### Comparação Direta
| Caso               | `state1` + `goal1`       | `state2` + `goal2a/goal2b`  |
|--------------------|--------------------------|-----------------------------|
| **Blocos Críticos** | `a` (tamanho 1), `d` (3) | `c` (2), `d` (3)            |
| **Ações por Nó**   | Poucas (blocos pequenos) | Muitas (blocos grandes)     |
| **Profundidade**   | Baixa                    | Alta                        |
| **Memória**        | Controlada               | Estoura (exponencial)       |

---

### Conclusão
O estouro de memória em `state2` ocorre porque a combinação de:
1. Blocos grandes (`c` e `d`) com mais restrições de movimento.
2. Objetivos que exigem reposicionamento complexo.
3. BFS armazenando todos os nós intermediários.

Para resolver, seria necessário otimizar o algoritmo ou usar técnicas de busca mais eficientes em memória.
