# trabalhoLP

Codificar/decodificar uma mensagem atraves da cifra de cesar:

    cesar(Mensagem,Chave,Cifra).

Decifrar uma mensagem cifrada por cesar sem a chave:

    deCesar(Message,Decripted).

Codificar/decodificar uma mensagem atraves da cifra de vigenere:

    vigenere(Mensagem, Mensagem_Chave, Cifra).

Para uma mensagem cifrada por vigenere temos os seguintes predicaods:

    Parear duas Strings:
        pairingLists(Mensagem,Mensagem_Chave,ListaPares).

    Encontrar a chave sabendo a Mensagem cifrada, Tamanho da Chave, Uma palavra decifrada e sua posição:
        achave(Mensagem,TamChave,PalavraDecifrada,PosicaoI,Chave)

    Decifrar a mensagem sabendo, a Mensagem cifrada, Tamanho da Chave, uma Palavra contida na mensagem:
        terceiroPred(Mensagem,TamChave,Palavra,MensagemDecifrada)

    Decifrar a mensagem sabendo, a Mensagem cifrada, Tamanho da Chave, uma Lista de Palavras contidas, ou não, na mensagem:
        quartoPred(Mensagem,ListaPossiveisPalavras,TamChave,MensagemDecifrada)


"um predicado que relaciona uma mensagem cifrada uma lista de possiveis palavras que ocorrem no texto e um tamanho de chave com a mensagem decifrada"