:- consult('utils.pl').
:- consult('codificar.pl').

deCesar(Message,Decripted) :-
    find_most_freq_letter(Message,Letter),
    get_potencial_keys(Letter,Keys),
    decifra_cesar(Message, Keys,Decripted),
    !.

decifra_cesar(Message, Keys, Decripted) :-
    maplist(cesar(Message), Keys, Potencial_Decripted),
    maplist(split_message(), Potencial_Decripted, Potencial_Decripted_Splited),
    include(is_valid, Potencial_Decripted_Splited, Decripted_List),
    flatten(Decripted_List,Decripted_Flatten_List),
    atomics_to_string(Decripted_Flatten_List," ",Decripted).
