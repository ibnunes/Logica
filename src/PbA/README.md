# Problema A &mdash; Quantas cavalgadas?

**_By_ Simão Melo de Sousa, PhD, UBI, Portugal**

Para começar a série de exercıcios desta unidade curricular, eis um problema elucidativo. Espera-se que defina uma resolução recursiva mas também que tente definir uma solução concisa, eficaz e elegante.

## Problema

Considere um tabuleiro de Xadrez $N$ por $N$ e um cavaleiro posicionado na célula $(a,b)$ (com $0 \leq a, b < N$). Levanta-se a questão seguinte, dado um inteiro natural positivo $k$ (i.e. $k>0$), quantos caminhos de comprimento $k$ e exclusivamente dentro dos limites do tabuleiro, o cavaleiro pode tomar.

O seu desafio é responder a esta questão.


## Formato de entrada

A entrada deste exercício consiste numa linha onde consta os 4 inteiros $N$, $k$, $a$ e $b$, separados por um espaço.


## Formato de saída

Uma linha com o inteiro $M$ que indica quantas cavalgadas de comprimento $k$ diferentes são possíveis no tabuleiro partindo de $(a, b)$.


## Limites

$1 \leq N \leq 50 \quad 1 \leq k \leq 8$


## Exemplo de entrada

`4 2 0 0`


## Exemplo de saída

`8`
