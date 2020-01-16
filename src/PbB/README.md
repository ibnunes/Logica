# Problema B &mdash; Fórmulas Proposicionais Bem Formadas

**_By_ Simão Melo de Sousa, PhD, UBI, Portugal**

O objectivo deste problema é a da análise de fórmulas proposicionais e da sua boa formação.

Por exemplo as strings:

```
(X -> Y)
(False & ((True & Y) | (X <-> Y)))
((False) & ((True & Y) | (X <-> Y)))
```

representam fórmulas proposicionais bem formadas (bem parenteseadas, todos os operadores lógicos são suados devidamente). Em constraste, as strings seguintes não representam fórmulas bem formadas:

```
( & ((True & Y) | (X <-> Y)))
((False & (!(True & Y) | (X <-> Y)))
```

Na primeira há um uso desadequado da conjunção, e a segunda fórmula está mal parenteseada.


## Tarefa

Implemente um programa que, recebendo uma fórmula em entrada, verifique se está bem formada ou não.


## Input

A fórmula por analisar é dade de uma forma faseada, elemento por elemento, um por linha.

`True` representa o valor de verdade, `False` o valor falso, qualquer outra string de caracteres é uma variável proposicional (assuma que usaremos somente letras maiúsculas para simplificar).

Usamos igualmente os parêntesis habituais `( )`, o caracter `&` para a conjunção, `!` para a negação, `|` para a disjunção, `->` para a implicação e finalmente `<->` para a equivalência.

O input é então organizado da seguinte forma. Na primeira linha indica-se com um inteiro _N_ o tamanho da fórmula (quantos elementos sintácticos constituem a fórmula &mdash; ver exemplo).

As _N_ linhas introduzem na ordem e um a um os elementos da fórmula.


### Constraints

* _0 ≤ N ≤ 100_


## Output

Uma linha com a palavra `YES` se a fórmula em entrada estiver bem formada, ou a palavra `NO` caso contrário.


### Input 1

```
17
(
False
&
(
(
True
&
Y
)
|
(
X
<->
Y
)
)
)
```


### Output 1

```
YES
```


### Input 2

```
18
(
(
False
&
(
(
True
&
Y
)
|
(
X
<->
Y
)
)
)
```


### Output 2

```
NO
```
