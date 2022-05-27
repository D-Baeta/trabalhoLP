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