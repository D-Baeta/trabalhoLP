/*
* Daniel Augusto Machado Baeta - 201965122C
* Thiago do Vale Cabral - 201965220AC
*/

decript_cesar(Encripted_Message,Decripted) :-
    find_most_freq_letter(Encripted_Message,Letter),
    get_potencial_keys(Letter,Keys),
    find_cesar(Encripted_Message, Keys,Decripted),
    !.

find_cesar(Encripted_Message, Keys, Decripted) :-
    maplist(cesar(Encripted_Message), Keys, Potencial_Decripted),
    maplist(split_message(), Potencial_Decripted, Potencial_Decripted_Splited),
    include(is_valid, Potencial_Decripted_Splited, Decripted_List),
    flatten(Decripted_List,Decripted_Flatten_List),
    atomics_to_string(Decripted_Flatten_List," ",Decripted).

decript_vigenere_multiple_words(Message,Possible_Words_List,Key_Length,Decripted_Message) :-
  maplist(terceiroPred(Message,Key_Length),Possible_Words_List,Decripted_Message_List),
  sort(Decripted_Message_List,Unic_Decripted_Message),
  atomics_to_string(Unic_Decripted_Message,"",Decripted_Message),
  !.

de_vigenere(Cipher,Key,Message) :-
  vigenere(Message,Key,Cipher).

decript_vigenere_one_word(Message,Key_Length,Known_Word,Decripted_Message) :-
  string2code(Known_Word,P1),
  slice(P1,1,Key_Length,P2),
  string2code(Message, M1),
  length(M1, L1),
  goes_through_list([_|M1],L1,P2,Key_Length,[],W),
  writef('%t\n',[W]),
  maplist(text_to_string,W,WString),
  writef('%t\n',[WString]),
  maplist(string2code,WString,WChars),
  writef('%t\n',[WChars]),
  maplist(func,WChars,Permutation),
  flatten(Permutation,Decripted_Flatten_List),
  maplist(de_vigenere(Message),Decripted_Flatten_List,Possible_Decripted_Messages),
  writef('%t\n',[Possible_Decripted_Messages]),
  include(is_sentence, Possible_Decripted_Messages, Decripted_Message_List),
  writef('%t\n',[Decripted_Message_List]),
  sort(Decripted_Message_List,Decripted_Message_Sorted_List),
  atomics_to_string(Decripted_Message_Sorted_List," ",Decripted_Message),
  !.