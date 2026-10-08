---
title: Prueba de funcionalidades
date: 2026-10-04
tags: prueba, tipografía
abstract: Página de prueba que reúne, sección por sección, todo lo que el sitio sabe mostrar. Sirve para revisar de un vistazo que nada se haya roto después de un cambio en las plantillas, el CSS o el código.
toc-depth: 2
---

<div class="epigraph">
> «Todo lo que puede ser dicho, puede ser dicho con claridad.»
>
> Ludwig Wittgenstein, *Tractatus*
</div>

## Texto y tipografía

Un párrafo normal, justificado y con separación silábica. Las comillas «angulares», las “inglesas” y las 'simples' se respetan; los guiones largos —como este— y los rangos 1914–1918 también. Las siglas en mayúsculas, como UNESCO o ADN, pasan solas a versalitas. Hay *cursiva*, **negrita**, ***ambas***, ~~tachado~~, super^índice^ y sub~índice~, además de `código en línea`.

Un segundo párrafo comienza con sangría, como en un libro. Las cifras de estilo antiguo se notan en números como 1234567890 dentro del texto corrido.

## Enlaces

- Wikipedia, con su ícono: [teoría de categorías](https://es.wikipedia.org/wiki/Teor%C3%ADa_de_categor%C3%ADas)
- GitHub: [el código del sitio](https://github.com/a-peirogon/a-peirogon.github.io)
- arXiv: [Baez & Stay, *Rosetta Stone*](https://arxiv.org/abs/0903.0340)
- Un PDF: [Selinger, *A survey of graphical languages*](https://arxiv.org/pdf/0908.3347.pdf)
- Un DOI: [Worrall, 1989](https://doi.org/10.1111/j.1746-8361.1989.tb00933.x)
- Interno, a la wiki: [Mónadas](/wiki/matematicas/monadas.html)
- Interno, a otra sección de esta página: [ir a Matemáticas](#matemáticas)

## Notas al pie y notas al margen

Las notas se muestran al margen en pantallas anchas y al final en las demás.[^corta] Una nota larga pone a prueba el recorte y el desplazamiento de las notas al margen.[^larga] También hay notas en línea, escritas junto al texto.^[Esta nota se escribió en línea, entre corchetes, sin definirla aparte.]

[^corta]: Una nota corta.

[^larga]: Una nota larga, con varios párrafos y formato. *Cursiva*, **negrita** y `código`.

    Segundo párrafo de la misma nota, con un enlace a [Wikipedia](https://es.wikipedia.org/wiki/Nota_al_pie) y una fórmula: $e^{i\pi} + 1 = 0$.

## Citas

> Una cita en bloque, con su borde y su fondo.
>
> > Una cita anidada dentro de la anterior.
> >
> > > Y un tercer nivel.

## Listas

- Un elemento
- Otro, con una lista anidada:
    - Primer subelemento
    - Segundo subelemento
        - Y un tercer nivel
- Un último elemento

1. Primero
2. Segundo
    1. Subpaso en numeración romana
    2. Otro subpaso
3. Tercero

Término
:   Una lista de definiciones: el término y, debajo, su definición.

Otro término
:   Su definición correspondiente.

## Código

Haskell, resaltado con Pygments:

```haskell
-- | Una mónada sobre una categoría es un monoide en sus endofuntores.
class Applicative m => Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a
  return = pure

main :: IO ()
main = mapM_ print [x * x | x <- [1 .. 5 :: Int]]
```

Python:

```python
def fib(n: int) -> int:
    """Fibonacci, en versión iterativa."""
    a, b = 0, 1
    for _ in range(n):
        a, b = b, a + b
    return a
```

Y un bloque sin lenguaje:

```
texto preformateado, sin resaltado
    respetando los espacios
```

## Matemáticas

En línea: si $f : A \to B$ y $g : B \to C$, entonces $g \circ f : A \to C$. En bloque:

$$
\mu \circ T\mu = \mu \circ \mu T, \qquad \mu \circ T\eta = \mu \circ \eta T = 1_T
$$

## Diagramas TikZ

```{.tikzpicture caption="Cuadrado conmutativo, compilado a SVG con TikZ."}
\begin{tikzpicture}[node distance=2.4cm, >=stealth]
  \node (A) {$A$};
  \node (B) [right of=A] {$B$};
  \node (C) [below of=A] {$C$};
  \node (D) [below of=B] {$D$};
  \draw[->] (A) -- node[above] {$f$} (B);
  \draw[->] (A) -- node[left] {$g$} (C);
  \draw[->] (B) -- node[right] {$h$} (D);
  \draw[->] (C) -- node[below] {$k$} (D);
\end{tikzpicture}
```

## Tablas

Las tablas se pueden ordenar haciendo clic en los encabezados.

| Estructura        | Objetos      | Morfismos              | Año  |
|-------------------|--------------|------------------------|------|
| **Set**           | conjuntos    | funciones              | 1945 |
| **Grp**           | grupos       | homomorfismos          | 1945 |
| **Top**           | espacios     | funciones continuas    | 1945 |
| **Vect**          | esp. vectoriales | aplicaciones lineales | 1945 |

## Imágenes

![*La noche estrellada*, Vincent van Gogh, 1889.](/img/obras/noche_estrellada.jpg)

## Cajas

```{.caja título="Recursos externos (plegable)"}
- [nLab](https://ncatlab.org) — la referencia obligada en teoría de categorías
- [Stanford Encyclopedia of Philosophy](https://plato.stanford.edu)
- [arXiv: math.CT](https://arxiv.org/list/math.CT/recent)
```

```{.caja título="Caja fija" plegable="no"}
Una caja con `plegable="no"` no se pliega: su contenido siempre está a la vista.
```

---

## Matemáticas

Una segunda sección con el mismo nombre, para comprobar que los anclajes de la tabla de contenidos no se confunden.
