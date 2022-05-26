% codificar e decodificar: transforma a palavra baseado em uma chave conhecida

:- use_module(library(clpfd)).
:- consult('baseDePalavras.pl').

cesar(Chave,Mensagem,Cifra) :-
    (   (string(Mensagem); atom(Mensagem)) ->
        string2code(Mensagem,M1),
        maplist(cifra_cesar(Chave), M1, Cifra_Lista),
        string2code(Cifra,Cifra_Lista)
    ;
        (   (string(Cifra); atom(Cifra)) ->
            string2code(Cifra,C1),
            maplist(cifra_cesar(Chave), Mensagem_Lista, C1),
            string2code(Mensagem,Mensagem_Lista)
        )
    ).
    
    %writef('Codificado cesar: %t\n', [Cifra_String]).

cifra_cesar(Chave,X,Y) :-
    C1 #= Chave mod 27,
    Y #= (X+C1) mod 27.



    
