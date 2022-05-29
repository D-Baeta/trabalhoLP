/*
* Daniel Augusto Machado Baeta - 201965122C
* Thiago do Vale Cabral - 201965220AC
*/

cesar(Message,Key,Cipher) :-
  (   (is_string_or_atom(Message)) ->
    string2code(Message,M1),
    maplist(calculate_offset(Key), M1, Cipher_List),
    string2code(Cipher,Cipher_List)
  ;
    string2code(Cipher,C1),
    maplist(calculate_offset(Key), Message_List, C1),
    string2code(Message,Message_List),
    !
  ).

vigenere(Message, Message_Key, Cipher) :-
  (   (is_string_or_atom(Message)) ->
    string2code(Message, M1),
    string2code(Message_Key, M2),
    length(M1, L1),
    stretchfy(M2, L1, M2_Normalized),
    maplist(calculate_offset, M1, M2_Normalized, Cipher_List),
    string2code(Cipher, Cipher_List)
  ;
    string2code(Cipher, C1),
    string2code(Message_Key, M2),
    length(C1, L1),
    stretchfy(M2, L1, M2_Normalized),
    maplist(calculate_offset, Message_List, M2_Normalized, C1),
    string2code(Message, Message_List),
    !
  ).