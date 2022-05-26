string2code(String,Code) :-
    (   (string(String); atom(String)) ->
        string_chars(String,Chars),
        maplist(code(), Chars, Code)
    ;
        maplist(code(), Chars, Code),
        string_chars(String,Chars)
    ).

addWord(Word) :-
  assertz(word(Word)),
  append('words.pl'),
  write(word(Word)), 
  write('.'),
  nl,
  told.
