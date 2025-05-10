____
# Equipe

1. Alberth Viana de Lima 
2. Ana Júlia Pereira Corrêa
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

Para melhor documentação e organização, à parte do README, foi criado BFS.md e Astar.md, com explicações completas dos códigos implentados e como compilá-los. 
