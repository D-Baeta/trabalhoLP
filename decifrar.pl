:- consult('utils.pl').
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
