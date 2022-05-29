Alunos:
    Daniel Augusto Machado Baeta - 201965122C
    Thiago do Vale Cabral - 201965220AC

# Instrução

Para iniciar programa: swipl -s index.pl


# Codificação e Base de Dados:

    word(X) :- Retorna verdadeirio se X estiver na base de palavras.

    code(Char, Code) :- Relaciona um caractere à um número.

    string2Code(S, L) :- Relaciona uma String com uma Lista de códigos de seus caracteres.

# Cifrando e Decifrando Mensagens

    cesar(Message,Key,Cipher) :- codifica/decodifica pela cifra de cesar uma mensagem dada uma chave.

    vigenere(Message, Message_Key, Cipher) :- codifica/decodifica pela cifra de vigenere uma mensagem dada uma chave.

    decript_cesar(Message,Decripted) :- decifra uma mensagem criptografada pela cifra de cesar.

    pairing_lists(Message,Message_Keys,Pair_List) :- Parear duas Strings.

    find_key(Message,Key_Length,Decripted_Word,Initial_Pos,Key) :- Encontrar a chave sabendo a Mensagem cifrada, Tamanho da Chave, Uma palavra decifrada e sua posição.

    decript_vigenere_one_word(Message,Key_Length,Known_Word,Decripted_Message) :- Decifrar a mensagem sabendo, a Mensagem cifrada, Tamanho da Chave, uma Palavra contida na mensagem.

    decript_vigenere_multiple_words(Message,Possible_Words_List,Key_Length,Decripted_Message) :-  Decifrar a mensagem sabendo, a Mensagem cifrada, Tamanho da Chave, uma Lista de Palavras contidas, ou não, na mensage.


"um predicado que relaciona uma mensagem cifrada uma lista de possiveis palavras que ocorrem no texto e um tamanho de chave com a mensagem decifrada"