---
title: Lorem ipsum
date: 2026-01-13
tags: testing, demo, lorem-ipsum
---

# Introduction to Lorem Ipsum

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat[^1].

## Code highlighting with pygments

Here's some Haskell:

```haskell
-- Factorial function
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- QuickSort implementation
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]
```

## Mathematical expressions

Testing inline math: $E = mc^2$ and the quadratic formula $x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}$.

Display math:

$$
\int_{-\infty}^{\infty} e^{-x^2} dx = \sqrt{\pi}
$$

Another example:

$$
\nabla \times \mathbf{E} = -\frac{\partial \mathbf{B}}{\partial t}
$$

## TikZ diagram test

Here's a simple TikZ diagram:

```tikzpicture
\begin{tikzpicture}
  \draw[->] (0,0) -- (4,0) node[right] {$x$};
  \draw[->] (0,0) -- (0,3) node[above] {$y$};
  \draw[domain=0:3.5,smooth,variable=\x,blue,thick] plot ({\x},{0.5*\x*\x});
  \node at (2,2.5) {$y = \frac{1}{2}x^2$};
\end{tikzpicture}
```

## Lists and structure

### Unordered list

- Lorem ipsum dolor sit amet
- Consectetur adipiscing elit
  - Nested item one
  - Nested item two
- Sed do eiusmod tempor incididunt

### Ordered list

1. First important point about lorem ipsum
2. Second crucial observation
3. Third significant detail
   1. Sub-point alpha
   2. Sub-point beta
4. Fourth and final consideration

## Block quotes

> Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris.
>
> — Marcus Tullius Cicero

## Tables

| Language | Paradigm | Type System |
|----------|----------|-------------|
| Haskell  | Functional | Static, Strong |
| Python   | Multi-paradigm | Dynamic, Strong |
| C        | Procedural | Static, Weak |
| JavaScript | Multi-paradigm | Dynamic, Weak |

## More complex TikZ

```tikzpicture
\begin{tikzpicture}[node distance=2cm]
  \node[circle,draw] (A) {A};
  \node[circle,draw] (B) [right of=A] {B};
  \node[circle,draw] (C) [below of=A] {C};
  \node[circle,draw] (D) [right of=C] {D};
  
  \draw[->] (A) -- (B);
  \draw[->] (A) -- (C);
  \draw[->] (B) -- (D);
  \draw[->] (C) -- (D);
\end{tikzpicture}
```

## Emphasis and formatting

This is *italic text* and this is **bold text**. We can also combine ***both italic and bold***.

Here's some `inline code` within a sentence.

## Footnotes

This sentence has a footnote[^2]. And this one has another[^3].

## Conclusion

Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

---

[^1]: This is the first footnote with some additional context about Lorem Ipsum being placeholder text since the 1500s.

[^2]: Footnotes are automatically numbered and linked.

[^3]: They appear at the bottom of the page in the rendered version.
