# Connect Four

Implementação em Haskell para o jogo de tabuleiro Connect Four.

## Pré-requisitos

Para executar o jogo é necessário ter o compilador GHCI e a biblioteca `System.Console.ANSI` instalados.

```bash
$ cabal install ansi-terminal
```

## Como jogar

Para iniciar o jogo você deve executar os seguintes comandos:

```bash
$ ghci ConnectFour.hs
*Main> main
```

O jogo é pensado para ser jogado por dois jogadores. A primeira rodada é jogada pelo **Jogador 1** (peças vermelhas), seguido pelo **Jogador 2** (peças azuis). Isso se repete até que um dos jogadores ganhe o jogo.

O objetivo do jogo é ser o primeiro a formar uma sequência horizontal, vertical ou diagonal de quatro peças do próprio jogador.

Para inserir uma peça, o jogador deve digitar um caractere de 'a' à 'g' de acordo com o cabeçalho acima do tabuleiro. O jogo só prossegue quando o jogador digitar um caractere válido.
