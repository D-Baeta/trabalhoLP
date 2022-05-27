% decifrar: Pega um texto crifrado e volta com ele sem saber a chave

:- use_module(library(clpfd)).
:- consult('codificar.pl').

deCesar(Message,Decripted) :-
    findMostFreqLetter(Message,Letter),
    getPotencialKeys(Letter,Keys),
    decifra_cesar(Message, Keys,Decripted),
    !.

decifra_cesar(Message, Keys, Decripted) :-
    maplist(cesar(Message), Keys, PotencialDecripted),
    maplist(splitMessage(), PotencialDecripted, PotencialDecriptedSplited),
    include(isValid, PotencialDecriptedSplited, DecriptedList),
    flatten(DecriptedList,DecriptedFlattenList),
    atomics_to_string(DecriptedFlattenList," ",Decripted).

isValid(Pds) :-
    maplist(word,Pds).
    

splitMessage(P, Splited) :-
    split_string(P," ", "",SplitedString),
    maplist(atom_string(), Splited, SplitedString).

    
getPotencialKeys(L,K) :-
    code(L, Code),
    maplist(code(), [' ',a,e,o,s,r,i,n,d,m,u,t,c,l,p,v,g,h,q,b,f,z,j,x,k,w,y], FreqCode),
    maplist(sub(Code),FreqCode,K).

sub(Code, Element, Key) :-
    Key #= Element - Code.

findMostFreqLetter(Message,Letter) :-
    string_chars(Message,MsgChars),
    sort(MsgChars,Sorted),
    maplist(count(MsgChars),Sorted,Freq),
    pairs_keys_values(Pairs,Freq,Sorted),
    keysort(Pairs, SortedByKey),
    pairs_values(SortedByKey, JustValues),
    my_last(Letter,JustValues).


count([], _, 0).
count([X | T], X, N) :-
  !, count(T, X, N1),
  N #= N1 + 1.
count([_ | T], X, N) :-
  count(T, X, N).


my_last(X,[X]).
my_last(X,[_|L]) :- my_last(X,L).