---
title: "Fórmula del número de clase"
date: 2024-01-15
tags: matemáticas
abstract: |
  <p>La factorización es una idea esencial en la teoría de números. Tras aprender los fundamentos del álgebra abstracta, uno descubre que es posible factorizar un elemento de cualquier anillo conmutativo, no solo de los enteros. Sin embargo, en un anillo arbitrario, la factorización no es necesariamente única.</p>
---

Existe un teorema que relaciona la factorización única en un anillo con el valor de una función $\zeta$.
También ayuda a determinar si el anillo tiene factorización única si se puede calcular el valor de dicha función $\zeta$.[^approx]. Describiré los anillos, luego la función $\zeta$ y, finalmente, la conexión entre ambos.

[^approx]: No es necesario un valor exacto; a veces incluso una aproximación es suficiente.

# Cuerpos

Sea $K = \mathbb{Q}(\sqrt{m} )$ donde $m < 0$ es un entero libre de cuadrados.
Consiste en números de la forma $a+b\sqrt{m}$, donde $a, b \in \mathbb{Q}$.
El cuerpo $K$ se llama *cuerpo cuadrático imaginario*, ya que es una extensión de grado 2 de $\mathbb{Q}$ dada por $K = \mathbb{Q}[x]/(x^2 - m)$.

También tiene un *anillo de enteros* $\mathcal{O}_K$, que es el análogo de $\mathbb{Z}$ para este cuerpo.
De hecho, $\mathcal{O}_K$ es la [clausura integral](https://es.wikipedia.org/wiki/Elemento_entero#Clausura_entera) de $\mathbb{Z}$ en $K$.

Desafortunadamente, el anillo $\mathcal{O}_K$ no siempre es un dominio de factorización única.[^lame]
Por ejemplo, en $\mathbb{Z}[\sqrt{-5}]$ tenemos $6 = 2 \cdot 3 = (1 + \sqrt{-5})(1 - \sqrt{-5})$.
El *número de clase* mide exactamente cuánto falla la factorización única en $\mathcal{O}_K$.[^classnumber]

[^lame]: Si $\mathcal{O}_K$ fuera siempre un dominio de factorización única, ¡entonces el último teorema de Fermat se habría probado casi 150 años antes!
    En 1847, [Gabriel Lamé](https://es.wikipedia.org/wiki/Gabriel_Lam%C3%A9) dio una [demostración](https://gallica.bnf.fr/ark:/12148/bpt6k29812/f310.item) [errónea](https://math.stackexchange.com/q/953462) que dependía de que $\mathbb{Z}[\zeta_n]$ fuera un dominio de factorización única, donde $\zeta_n$ es una raíz $n$-ésima primitiva de la unidad.

[^classnumber]: Es el orden de un cierto grupo conocido como el [grupo de clases de ideales](https://es.wikipedia.org/wiki/Grupo_de_clases_de_ideales).
    Cuando el orden es $1$, $\mathcal{O}_K$ es un dominio de factorización única.

Ahora, sea
\\[
N = \begin{cases} |m| & \text{si } m \equiv 1 \mod{4} \\\ 4|m| & \text{si } m \equiv 2,3\mod{4} \end{cases}
\\]

El número $N$ se conoce como el *discriminante* del cuerpo numérico.
Podemos definir un homomorfismo de grupos $\chi : (\mathbb{Z}/N\mathbb{Z})^\times \to \{\pm 1\} $ como sigue:
para cualquier primo $p$ que no divida a $m$, ponemos $\chi(p) = \left( \frac{m}{p} \right) $
(Este es el símbolo de Legendre, que devuelve $1$ si $m$ tiene una raíz cuadrada en $\mathbb{F}_p$ y $-1$ en caso contrario).
Usando la [reciprocidad cuadrática](https://es.wikipedia.org/wiki/Ley_de_reciprocidad_cuadr%C3%A1tica), podemos extenderlo a todos los enteros coprimos con $m$.

# Funciones L

Formamos entonces la serie
\\[
  L(s,\chi) = \sum_{n=1}^{\infty} \frac{\chi(n)}{n^s}.
\\]
donde $\chi(n)$ es $\chi(n \mod N)$ si $n$ es coprimo con $N$, y $0$ en caso contrario.
Este tipo de serie, que podemos definir para cualquier homomorfismo $\chi : (\mathbb{Z}/n\mathbb{Z})^\times \to \mathbb{C}^\times$,
se conoce como una función $L$[^dirich], y es una extensión de la función $\zeta$ de Riemann.[^riemann]

[^dirich]: Es una [función L de Dirichlet](https://es.wikipedia.org/wiki/Funci%C3%B3n_L_de_Dirichlet) para ser precisos; hay muchos tipos de funciones $L$ en la teoría de números.

[^riemann]: La función $\zeta$ de Riemann es una función $L$ con $N=2$, porque $(\mathbb{Z}/2\mathbb{Z})^\times = \{1\}$ y el homomorfismo debe enviar el $1$ al $1$.

Por ejemplo, si $m = -1$, entonces $N=4$.
Tenemos $\chi(1 \mod 4) = 1$ y $\chi(3 \mod 4) = -1$.
Así, \\[L(s,\chi) = 1 - \frac{1}{3^s} + \frac{1}{5^s} - \frac{1}{7^s} + \dots\\]
Y en particular, \\[L(1,\chi) = 1 - \frac13 +\frac15 - \frac17 + \dots = \frac\pi4\\]

# El teorema

He aquí el teorema principal, llamado la fórmula del número de clase para cuerpos cuadráticos imaginarios.

> Para un cuerpo cuadrático imaginario $K$, sea $h_K$ su número de clase y $w_K$ el número de raíces de $1$ en $K$. Entonces:
\\[h_K = \frac{w_K}{2} L(0,\chi) = \frac{w_K \sqrt{N} }{2\pi} L(1,\chi).\\]

Así pues, el hecho de que el anillo de enteros sea o no un dominio de factorización única depende del valor de una cierta función $\zeta$.

Por ejemplo, tomemos de nuevo $m = -1$, por lo que $K = \mathbb{Q}(\sqrt{-1} )$.
Las raíces de la unidad en este cuerpo son $1, -1, \sqrt{-1}$, y $-\sqrt{-1}$, así que $w_k = 4$.
Usando el teorema, obtenemos:
\\[
 h_K = \frac{w_K \sqrt{N}}{2\pi} L(1,\chi) = \frac{4 \cdot 2}{2\pi} L(1,\chi) = \frac{4}{\pi} \cdot L(1,\chi)
\\]
Aquí $L(1,\chi) = 1 - \frac{1}{3} + \frac{1}{5} - \dots = \frac{\pi}{4}$, por lo que $h_K = 1$.
Por lo tanto, $\mathcal{O}_K = \mathbb{Z}[\sqrt{-1}]$ es un dominio de ideales principales.
¡Es sorprendente que hayamos tomado un desvío a través del análisis para demostrar un hecho algebraico!
