/*
* Daniel Augusto Machado Baeta - 201965122C
* Thiago do Vale Cabral - 201965220AC
*/

decript_cesar(Encripted_Message,Decripted) :-
    find_most_freq_letter(Encripted_Message,Letter),
    get_potencial_keys(Letter,Keys),
    decifra_cesar(Encripted_Message, Keys,Decripted),
    !.

decifra_cesar(Encripted_Message, Keys, Decripted) :-
    maplist(cesar(Encripted_Message), Keys, Potencial_Decripted),
    maplist(split_message(), Potencial_Decripted, Potencial_Decripted_Splited),
    include(is_valid, Potencial_Decripted_Splited, Decripted_List),
    flatten(Decripted_List,Decripted_Flatten_List),
    atomics_to_string(Decripted_Flatten_List," ",Decripted).
