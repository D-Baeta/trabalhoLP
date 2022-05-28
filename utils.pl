:- use_module(library(clpfd)).
:- consult('words.pl').
:- consult('alphabet.pl').

/*
* É verdadeiro se a string for convertida no respectivo valor inteiro.
* Ex: ?:- string2code("abc", [1,2,3]).
*    true.
*
* @param String - String a ser convertida
* @param Code - Lista de números resultante da conversão
* @return - true se a lista de números for uma conversão direta da string
*/
string2code(String, Code) :-
  (   (string(String); atom(String)) ->
      string_chars(String,Chars),
      maplist(code(), Chars, Code)
  ;
      maplist(code(), Chars, Code),
      string_chars(String,Chars)
  ).

/*
* É verdadeiro se uma nova palavra for adicionada ao conjunto de palavras (words.pl).
*
* @param Word - Palavra a ser adicionada
* @return - true se a inserção for verdadeira
*/
add_word(Word) :-
  assertz(word(Word)),
  append('words.pl'),
  write(word(Word)), 
  write('.'),
  nl,
  told.

/*
* É verdadeiro se o total de palavras for igual ao total de palavras do arquivo words.pl.
*
* @param Total - Número total de palavras
* @return - true se o total for igual ao total de palavras do arquivo words.pl
*/
count_words(Total) :- 
  aggregate_all(count, word(_), Total).

/*
* É verdadeiro se o total de códigos for igual ao total de códigos do arquivo alphabet.pl.
*
* @param Total - Número total de códigos
* @return - true se o total for igual ao total de códigos do arquivo alphabet.pl
*/
count_codes(Total) :- 
  aggregate_all(count, code(_,_), Total).

/*
* É verdadeiro se a chave for uma string ou atom
*
* @param Key - Chave a ser avaliada
* @return - true se Key for string ou atom.
*/
is_string_or_atom(Key) :- 
  string(Key); atom(Key).

/*
* É verdadeiro se a lista resultante tiver o mesmo prefixo da lista original e um tamanho definido.
* Ex: ?- trim_list([1,2,3,4,5], 3, [1,2,3]).
*    true.
*
* @param List - Lista original
* @param Length - Tamanho final da lista
* @param Result_List - Lista resultante
* @return - true se Result_List tiver o mesmo prefixo que List, e o tamanho de Length
*/
trim_list(List,Length,Result_List) :-
  append(Result_List,_,List) ,
  length(Result_List,Length).

/*
* É verdadeiro se a lista resultante tiver o mesmo tamanho que a referência e o mesmo padrão da lista original.
* Ex: ?- stretchfy([a,b,c], 7, [a,b,c,a,b,c,a]).
*    true.
*
* @param List - Lista original
* @param Reference_Length - Comprimento referência
* @param Result_List - Lista resultante
* @return - true se Result_List tiver o mesmo padrão da lista original e comprimento de Reference_Length
*/
stretchfy(Subject_List, Reference_Length, Result_List) :-
  length(Subject_List, Subject_Length),
  (Subject_Length < Reference_Length ->
    append(Subject_List, Subject_List, Subject_List2),
    stretchfy(Subject_List2, Reference_Length, Result_List)
    ;
    trim_list(Subject_List, Reference_Length, Result_List)
  ),
  !.

/*
* É verdadeiro se o offset calculado for igual ao offset esperado.
* OBS.: O offset é limitado a um valor máximo correspondente ao total de codes (alphabet.pl).
* Ex: ?- calculate_offset(30, 10, 13). (valor máximo: 27)
*    true.
*
* @param Offset - Valor do offset esperado
* @param Entry - Valor inicial
* @param Output - Valor de saída após a soma com offset e o módulo com valor máximo
* @return - true se o resto de (Offset + Entry)/Total de Códigos for igual a Output 
*/
calculate_offset(Offset,Entry,Output) :-
  count_codes(Total_Codes),
  Output #= (Entry+Offset) mod Total_Codes.



/*
* É verdadeiro se for encontrada a letra com maior frequencia na mensagem.
*
* @param Message - Mensagem do tipo String
* @param Letter - Letra mais encontrada na Mensagem
* @return - true se encontrar a Letra que mais aparece na mensagem
*/
findMostFreqLetter(Message,Letter) :-
  string_chars(Message,MsgChars),
  sort(MsgChars,Sorted),
  maplist(count(MsgChars),Sorted,Freq),
  pairs_keys_values(Pairs,Freq,Sorted),
  keysort(Pairs, SortedByKey),
  pairs_values(SortedByKey, JustValues),
  my_last(Letter,JustValues).


/*
* É verdadeiro se o Resultado N conter a frequencia que o elemento X aparece na lista [].
*
* @param [] - Lista
* @param X - Elemento qualquer
* @param N - Frequencia de X em []
* @return - 
*/
count([], _, 0).
count([X | T], X, N) :-
  !, count(T, X, N1),
  N #= N1 + 1.
count([_ | T], X, N) :-
  count(T, X, N).


/*
* É verdadeiro se X for o único elemento da lista.
*
* @param X - Elemento
* @param [] - Lista
* @return - true se o elemento for o último da lista.
*/
my_last(X,[X]).
my_last(X,[_|L]) :- my_last(X,L).




/*
* É verdadeiro se K conter as possíveis chaves baseada na ordem das letras mais frequentes da lingua portuguesa.
*
* @param L - Letra que mais aparece na mensagem cifrada.
* @param K - Lista de possiveis chaves.
* @return - 
*/
getPotencialKeys(L,K) :-
  code(L, Code),
  maplist(code(), [' ',a,e,o,s,r,i,n,d,m,u,t,c,l,p,v,g,h,q,b,f,z,j,x,k,w,y], FreqCode),
  maplist(sub(Code),FreqCode,K).


/*
* É verdadeiro se o valor da chave for a subtração das letras mais usadas na lingua portuguesa pela letra mais frequente da mensagem.
*
* @param Code - Representação númerica da letra mais frequente na mensagem.
* @param Element - Representação númerica de uma letra qualquer do alphabeto.
* @param Key - Potencial chave para quebra da cifra.
* @return - true se Key for a subtração de Element e Code.
*/
sub(Code, Element, Key) :-
  Key #= Element - Code.

/*
* É verdadeiro se o parametro for uma palavra
*
* @param Pds - Palavra decifrada.
* @return - true se Pds estiver no predicado word
*/
isValid(Pds) :-
  maplist(word,Pds).


/*
* É verdadeiro se Splited for a união dos elementos de P separados por um espaço.
*
* @param P - Lista de strings.
* @param Splited - atom
* @return -
*/
splitMessage(P, Splited) :-
  split_string(P," ", "",SplitedString),
  maplist(atom_string(), Splited, SplitedString).





/*
* Junta as duas strings em uma lista de pares
* Ex: "abcde","ghi" --> [a,b,c,d,e],[g,h,i] --> [a-g,b-h,c-i,d-g,e-h]
*
* @param Mensagem - String.
* @param Mensagem_Chave - String
* @return -
*/
pairingLists(Mensagem,Mensagem_Chave,ListaPares) :-
  string_chars(Mensagem, M1),
  string_chars(Mensagem_Chave, M2),
  length(M1, L1),
  stretchfy(M2, L1, M2_Normalizado),
  pairs_keys_values(ListaPares,M1,M2_Normalizado).


/*
* predicado que relaciona uma mensagem cifrada, um tamanho de chave, 
* uma palavra que sabidamente ocorre na mensagem decifrada e sua posicao,
* com a chave.
*/
achave(Mensagem,TamChave,PalavraDecifrada,PosicaoI,Chave) :-
  string2code(Mensagem, M1),
  PosicaoF #= PosicaoI + TamChave-1,
  slice(M1,PosicaoI,PosicaoF,L),
  string2code(PalavraDecifrada,P1),
  slice(P1,1,TamChave,P2),
  maplist(subMod(),P2,L,PossivelChave),
  findall(Perm, perm(PossivelChave,Perm),Ps),
  maplist(backString2code(),Ps,PotencialKeySplited),
  maplist(atom_string(), AtomKey, PotencialKeySplited),
  include(word, AtomKey, ChaveList),
  atomics_to_string(ChaveList," ",Chave),
  !.


/*
* Aplica do String2code ao contrário.
*/
backString2code(Permutation,PossibleKey) :-
  string2code(PossibleKey,Permutation).

/*
* Faz a permutação, não sei como.
*/
takeout(X,[X|R],R).  
takeout(X,[F |R],[F|S]) :- takeout(X,R,S).

perm([X|Y],Z) :- perm(Y,W), takeout(X,Z,W).  
perm([],[]).


/*
* Aplica a chave inversamente.
*/
subMod(M, C, K) :-
  K #= (C - M) mod 27.

/*
* Recorta uma Lista.
* @param [_|Xs] - Lista que será recortada.
* @param I - Inicio do Corte.
* @param K - Final do corte.
* @param Ys - Lista recortada
*/
slice([X|_],1,1,[X]).
slice([X|Xs],1,K,[X|Ys]) :- K > 1, 
   K1 is K - 1, slice(Xs,1,K1,Ys).
slice([_|Xs],I,K,Ys) :- I > 1, 
   I1 is I - 1, K1 is K - 1, slice(Xs,I1,K1,Ys).


/*
* um predicado que relaciona uma mensagem cifrada, 
* um tamanho de chave e uma palavra que ocorre no texto
* com a mensagem decifrada;
*/
terceiroPred(Mensagem,TamChave,Palavra,MensagemDecifrada) :-
  string2code(Palavra,P1),
  slice(P1,1,TamChave,P2),
  string2code(Mensagem, M1),

  teste([_|M1],P2,TamChave,[],W),
  maplist(text_to_string,W,WString),
  maplist(string2code,WString,WChars),
  maplist(func,WChars,Permutation),
  flatten(Permutation,DecriptedFlattenList),
  maplist(deVigenere(Mensagem),DecriptedFlattenList,PossibleDecriptedMessages),
  include(isSentence, PossibleDecriptedMessages, MensagemDecifradaList),
  atomics_to_string(MensagemDecifradaList," ",MensagemDecifrada),
  !.


/*
* Se a String é uma frase formada pelas palavras do predicado word
*/
isSentence(PossibleStringMessage) :-
  splitMessage(PossibleStringMessage,PossibleListMessage),
  isValid(PossibleListMessage).

/*
* Aplica a vinegere inversamente.
*/
deVigenere(Cifra,Key,Mensagem) :-
  vigenere(Mensagem,Key,Cifra).

/*
* Aplica a permutação nas possiveis cifras, pois ela pode estar em qualquer ordem.
* Retorna as possibilidades que corresponderem a uma palafra contida no predicado word.
*/
func(W,P) :-
  findall(Perm, perm(W,Perm),Pe),
  maplist(backString2code(),Pe,PotencialKeySplited),
  maplist(atom_string(), AtomKey, PotencialKeySplited),
  include(word, AtomKey, P).

/*
* Percorre a lista de acordo com o tamanho da chave.
* Cada pedaço é transformado a partir da palavra decifrada conhecida.
* Retorna uma lista com as possiveis cifras.
*/
teste([_|Y],Z,T,L,W) :- 
  slice(Y,1,T,Y2),
  maplist(subMod, Z, Y2, Cifra_Lista),
  string2code(CifraStr, Cifra_Lista),
  atom_string(AtomKey, CifraStr),
  insere(L,AtomKey,L1),
  teste(Y,Z,T,L1,W).
teste([_,_,_],_,_,W,W).

/*
* Inserse Elemento no inicio de uma lista.
*/
insere(Lista,Elemento,[Elemento|Lista]).

/*
* Utiliza o terceito predicado para passar como paramentro uma lista de possiveis palavras.
*/
quartoPred(Mensagem,ListaPossiveisPalavras,TamChave,MensagemDecifrada) :-
  maplist(terceiroPred(Mensagem,TamChave),ListaPossiveisPalavras,MensagemDecifradaLista),
  sort(MensagemDecifradaLista,MensagemDecifradaListaUnica),
  atomics_to_string(MensagemDecifradaListaUnica,"",MensagemDecifrada),
  !.