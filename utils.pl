/*
* Daniel Augusto Machado Baeta - 201965122C
* Thiago do Vale Cabral - 201965220AC
*/

:- use_module(library(clpfd)).

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
find_most_freq_letter(Message,Letter) :-
  string_chars(Message,Msg_Chars),
  sort(Msg_Chars,Msg_Chars_Sorted),
  maplist(count(Msg_Chars),Msg_Chars_Sorted,Freq),
  pairs_keys_values(Pairs,Freq,Msg_Chars_Sorted),
  keysort(Pairs, Sorted_By_Key),
  pairs_values(Sorted_By_Key, Just_Values),
  my_last(Letter,Just_Values).


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
get_potencial_keys(L,K) :-
  code(L, Code),
  maplist(code(), [' ',a,e,o,s,r,i,n,d,m,u,t,c,l,p,v,g,h,q,b,f,z,j,x,k,w,y], Freq_Code),
  maplist(sub(Code),Freq_Code,K).


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
is_valid(Pds) :-
  maplist(word,Pds).


/*
* É verdadeiro se Splited for a união dos elementos de P separados por um espaço.
*
* @param P - Lista de strings.
* @param Splited - atom
* @return -
*/
split_message(P, Splited) :-
  split_string(P," ", ":,.\n\t\s",Splited_String),
  maplist(atom_string(), Splited, Splited_String).

/*
* Junta as duas strings em uma lista de pares
* Ex: "abcde","ghi" --> [a,b,c,d,e],[g,h,i] --> [a-g,b-h,c-i,d-g,e-h]
*
* @param Mensagem - String.
* @param Mensagem_Chave - String
* @return -
*/
pairing_lists(Message,Message_Keys,Pair_List) :-
  string_chars(Message, M1),
  string_chars(Message_Keys, M2),
  length(M1, L1),
  stretchfy(M2, L1, M2_Normalized),
  pairs_keys_values(Pair_List,M1,M2_Normalized).

/*
* predicado que relaciona uma mensagem cifrada por vigenere, um tamanho de chave, 
* uma palavra que sabidamente ocorre na mensagem decifrada e sua posicao,
* com a chave.
*/
find_key(Message,Key_Length,Decripted_Word,Initial_Pos,Key) :-
  string2code(Message, M1),
  Final_Pos #= Initial_Pos + Key_Length-1,
  slice(M1,Initial_Pos,Final_Pos,Sliced),
  string2code(Decripted_Word,W1),
  slice(W1,1,Key_Length,W2),
  maplist(subMod(),W2,Sliced,Possible_Key),
  findall(Perm, perm(Possible_Key,Perm),Ps),
  maplist(backString2code(),Ps,Potencial_Key_Splited),
  maplist(atom_string(), Atom_Key, Potencial_Key_Splited),
  include(word, Atom_Key, Chave_List),
  sort(Chave_List,Chave_List_Sorted),
  atomics_to_string(Chave_List_Sorted," ",Key),
  !.

/*
* Aplica do String2code ao contrário.
*/
backString2code(Permutation,Possible_Key) :-
  string2code(Possible_Key,Permutation).

/*
* Faz a permutação
*/
takeout(X,[X|R],R).  
takeout(X,[F |R],[F|S]) :- takeout(X,R,S).

perm([X|Y],Z) :- perm(Y,W), takeout(X,Z,W).  
perm([],[]).


/*
* Aplica a chave inversamente.
*/
subMod(M, C, K) :-
  count_codes(Total_Codes),
  K #= (C - M) mod Total_Codes.

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
* Se a String é uma frase formada pelas palavras do predicado word
*/
is_sentence(Possible_String_Message) :-
  writef('%t\n',[Possible_String_Message]),
  split_message(Possible_String_Message,Possible_List_Message),
  writef('%t\n',[Possible_List_Message]),
  is_valid(Possible_List_Message).

/*
* Aplica a permutação nas possiveis cifras, pois ela pode estar em qualquer ordem.
* Retorna as possibilidades que corresponderem a uma palafra contida no predicado word.
*/
func(W,P) :-
  findall(Perm, perm(W,Perm),Pe),
  maplist(backString2code(),Pe,Potencial_Key_Splited),
  maplist(atom_string(), Atom_Key, Potencial_Key_Splited),
  include(word, Atom_Key, P).

/*
* Percorre a lista de acordo com o tamanho da chave.
* Cada pedaço é transformado a partir da palavra decifrada conhecida.
* Retorna uma lista com as possiveis cifras.
*/
goes_through_list([_|Encripted_Code_List],Encripted_Code_List_Length,Known_Word_Slice,Key_Length,Aux_List,Possible_Key_List) :-
  (
    (Encripted_Code_List_Length =:= Key_Length),
    goes_through_list([],_,_,_,Aux_List,Possible_Key_List)
  )
  ;
  (
    slice(Encripted_Code_List,1,Key_Length,Y2),
    maplist(subMod, Known_Word_Slice, Y2, Cifra_Lista),
    string2code(CifraStr, Cifra_Lista),
    atom_string(AtomKey, CifraStr),
    insert_element_list(Aux_List,AtomKey,L1),
    length(Encripted_Code_List, LY2),
    goes_through_list(Encripted_Code_List,LY2,Known_Word_Slice,Key_Length,L1,Possible_Key_List)
  ).
goes_through_list([],_,_,_,Possible_Key_List,Possible_Key_List).

/*
* Inserse Elemento no inicio de uma lista.
*/
insert_element_list(List,Element,[Element|List]).
