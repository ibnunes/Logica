# Problema A &mdash; Quantas cavalgadas?

**_By_ Simão Melo de Sousa, PhD, UBI, Portugal**

Para começar a série de exercıcios desta unidade curricular, eis um problema elucidativo. Espera-se que defina uma resolução recursiva mas também que tente definir uma solução concisa, eficaz e elegante.

## Problema

Considere um tabuleiro de Xadrez _N_ por _N_ e um cavaleiro posicionado na célula _(a,b)_ (com **0 ≤ _a, b_ < _N_**). Levanta-se a questão seguinte, dado um inteiro natural positivo _k_ (i.e. _k > 0_), quantos caminhos de comprimento _k_ e exclusivamente dentro dos limites do tabuleiro, o cavaleiro pode tomar.

O seu desafio é responder a esta questão.


## Formato de entrada

A entrada deste exercício consiste numa linha onde consta os 4 inteiros _N_, _k_, _a_ e _b_, separados por um espaço.


## Formato de saída

Uma linha com o inteiro _M_ que indica quantas cavalgadas de comprimento _k_ diferentes são possíveis no tabuleiro partindo de _(a, b)_.


## Limites

* _1 ≤ N ≤ 50_
* _1 ≤ k ≤ 8_


## Exemplo de entrada

`4 2 0 0`


## Exemplo de saída

`8`
