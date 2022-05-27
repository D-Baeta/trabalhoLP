:- consult('utils.pl').

  cesar(Mensagem,Chave,Cifra) :-
    (   (is_string_or_atom(Mensagem)) ->
      string2code(Mensagem,M1),
      maplist(calculate_offset(Chave), M1, Cifra_Lista),
      string2code(Cifra,Cifra_Lista)
    ;
      string2code(Cifra,C1),
      maplist(calculate_offset(Chave), Mensagem_Lista, C1),
      string2code(Mensagem,Mensagem_Lista),
      !
    ).

  vigenere(Mensagem, Mensagem_Chave, Cifra) :-
    (   (is_string_or_atom(Mensagem)) ->
      string2code(Mensagem, M1),
      string2code(Mensagem_Chave, M2),
      length(M1, L1),
      stretchfy(M2, L1, M2_Normalizado),
      maplist(calculate_offset, M1, M2_Normalizado, Cifra_Lista),
      string2code(Cifra, Cifra_Lista)
    ;
      string2code(Cifra, C1),
      string2code(Mensagem_Chave, M2),
      length(C1, L1),
      stretchfy(M2, L1, M2_Normalizado),
      maplist(calculate_offset, Mensagem_Lista, M2_Normalizado, C1),
      string2code(Mensagem, Mensagem_Lista),
      !
    ).