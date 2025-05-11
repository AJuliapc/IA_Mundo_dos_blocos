____
# Equipe

1. Alberth Viana de Lima 
2. Ana Júlia Pereira Corrêa
3. Guilherme Sahdo Maciel
____

# Inteligência Artificial - 1º Trabalho Pratico: Planejador para Empilhar blocos de diferente dimensões

Implemente o projeto descrito na pagina 403 do capítulo 17 do material (Código: planejadores do livro Bratko) 
do livro do Bratko, respondendo os pontos referentes às figuras no arquivo T1_MundoDosBlocos.pdf. 

**Opcionalmente** você pode pedir ajuda de um Chatbot, mas, para isso, deve 
seguir os seguintes passos (e REPORTAR CADA UM DELES); 

# REPRESENTAÇÃO DO CONHECIMENTO

1) Descrever o problema em linguagem natural, falando dos blocos de comprimento diferentes, iguais
    apenas na altura
2) Definir os conceitos de livre (clear) e outros necessários (vide descrição em anexo), estado do mundo deve ser  
    uma lista com as relações de sobreposição, isto é on(X,Y),
3) Pedir ao chat GPT uma representação adequada de termos em lógica para representar os elementos
     descritos no documento anexo, sem o uso de assign e retract.

# RACIOCÍNIO E PLANEJAMENTO AUTOMÁTICO

4) Definir as restrições em linguagem natural e pedir para o chat utilizar as descrições por ele geradas para
    escrever as restrições em código Prolog
 4.1 Fornecer exatamente o código do livro (disponível neste material do Classroom), principalmente as constraints (restrições) 
 4.2 Pedir para o Chatbot implementar o descrito, considerando goal regression e means-ends do livro
5) Testar com os exemplos do documento em anexo e corrigir os erros do código usando trace.

# Documentação do código e como rodar 

Para melhor documentação e organização, à parte do README, foi criado BFS.md e Astar.md, com explicações completas dos códigos implentados e como compilá-los. Mas caso aja pressa em compilar os códigos, aqui o trecho com apenas as linhas de como rodar:

1. A*

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

2. BFS 

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
