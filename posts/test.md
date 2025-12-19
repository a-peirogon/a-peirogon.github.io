---
title: Post de Prueba - Todas las Funcionalidades
date: 2024-12-19
author: Test Author
tags: test, features, demo
toc-depth: 3
---

# Introducción

Este post demuestra todas las funcionalidades especiales de tu sitio Hakyll: **smallcaps**, resaltado de sintaxis con Pygments, diagramas TikZ, matemáticas con KaTeX, y tabla de contenidos.

## Small Caps Automáticos

Las palabras en MAYÚSCULAS se convierten automáticamente en versalitas. Por ejemplo:

- NASA es la agencia espacial
- El FBI investiga crímenes federales
- La ONU promueve la paz mundial
- HTML, CSS y JS son tecnologías web
- El MIT y la UCLA son universidades prestigiosas

Las palabras mixtas como CamelCase o palabras sueltas como A no se convierten.

## Resaltado de Sintaxis con Pygments

### Python

```python
def fibonacci(n):
    """Calcula el n-ésimo número de Fibonacci."""
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

# Ejemplo de uso
for i in range(10):
    print(f"F({i}) = {fibonacci(i)}")
```

### Haskell

```haskell
-- Quicksort en Haskell
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

main :: IO ()
main = print $ quicksort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3]
```

### JavaScript

```javascript
// Función async/await moderna
async function fetchUserData(userId) {
    try {
        const response = await fetch(`/api/users/${userId}`);
        const data = await response.json();
        return data;
    } catch (error) {
        console.error('Error fetching user:', error);
        throw error;
    }
}

// Uso con Promises
fetchUserData(42)
    .then(user => console.log(user))
    .catch(err => console.error(err));
```

## Matemáticas con KaTeX

### Inline Math

La fórmula de Euler es $e^{i\pi} + 1 = 0$, una de las más bellas en matemáticas.

El teorema de Pitágoras: $a^2 + b^2 = c^2$ es fundamental en geometría.

### Display Math

La ecuación de Schrödinger:

$$i\hbar\frac{\partial}{\partial t}\Psi(\mathbf{r},t) = \left[-\frac{\hbar^2}{2m}\nabla^2 + V(\mathbf{r},t)\right]\Psi(\mathbf{r},t)$$

Serie de Taylor para $e^x$:

$$e^x = \sum_{n=0}^{\infty} \frac{x^n}{n!} = 1 + x + \frac{x^2}{2!} + \frac{x^3}{3!} + \cdots$$

La integral de Gauss:

$$\int_{-\infty}^{\infty} e^{-x^2} dx = \sqrt{\pi}$$

## Diagramas TikZ

### Diagrama Básico

```{.tikzpicture caption="Diagrama de flujo simple"}
\begin{tikzpicture}[node distance=2cm]
\node (start) [circle, draw] {Inicio};
\node (process) [rectangle, draw, below of=start] {Proceso};
\node (decision) [diamond, draw, aspect=2, below of=process] {¿Decisión?};
\node (end1) [circle, draw, below left of=decision] {Fin A};
\node (end2) [circle, draw, below right of=decision] {Fin B};

\draw[->] (start) -- (process);
\draw[->] (process) -- (decision);
\draw[->] (decision) -- node[left] {No} (end1);
\draw[->] (decision) -- node[right] {Sí} (end2);
\end{tikzpicture}
```

### Grafo Dirigido

```{.tikzpicture width="400" caption="Grafo con nodos y aristas"}
\begin{tikzpicture}[
    node/.style={circle, draw, minimum size=1cm},
    edge/.style={->, >=stealth, thick}
]
\node[node] (A) at (0,0) {A};
\node[node] (B) at (2,1) {B};
\node[node] (C) at (2,-1) {C};
\node[node] (D) at (4,0) {D};

\draw[edge] (A) -- (B);
\draw[edge] (A) -- (C);
\draw[edge] (B) -- (D);
\draw[edge] (C) -- (D);
\draw[edge, bend left] (B) to (C);
\end{tikzpicture}
```

### Árbol Binario

```{.tikzpicture caption="Árbol binario de búsqueda"}
\begin{tikzpicture}[
    level distance=1.5cm,
    level 1/.style={sibling distance=3cm},
    level 2/.style={sibling distance=1.5cm},
    every node/.style={circle, draw, minimum size=0.8cm}
]
\node {8}
    child {node {3}
        child {node {1}}
        child {node {6}
            child {node {4}}
            child {node {7}}
        }
    }
    child {node {10}
        child[missing]
        child {node {14}
            child {node {13}}
            child[missing]
        }
    };
\end{tikzpicture}
```

## Notas al Pie

Aquí hay una nota al pie simple[^1]. Y aquí hay otra más elaborada[^2].

[^1]: Esta es una nota al pie básica con un poco de texto explicativo.

[^2]: Esta nota al pie es más compleja y puede contener:
    - Múltiples líneas
    - Listas con viñetas
    - Incluso código: `var x = 42;`

## Listas y Formato

### Lista sin ordenar

- Elemento principal uno
  - Subelemento A
  - Subelemento B
    - Sub-subelemento
- Elemento principal dos
- Elemento principal tres

### Lista ordenada

1. Primer paso del proceso
2. Segundo paso importante
   1. Subpaso 2.1
   2. Subpaso 2.2
3. Paso final

### Citas

> La imaginación es más importante que el conocimiento. El conocimiento es limitado, mientras que la imaginación no.
>
> — Albert Einstein

> En programación, como en la vida, la simplicidad es la máxima sofisticación.

## Tablas

| Lenguaje   | Paradigma       | Tipado   | Año  |
|------------|-----------------|----------|------|
| Python     | Multi-paradigma | Dinámico | 1991 |
| Haskell    | Funcional       | Estático | 1990 |
| JavaScript | Multi-paradigma | Dinámico | 1995 |
| Rust       | Imperativo      | Estático | 2010 |

## Código Inline

Usar `console.log()` en JavaScript, `print()` en Python, o `putStrLn` en Haskell para imprimir a la consola.

## Enlaces e Imágenes

Visita [Haskell.org](https://www.haskell.org/) para aprender más sobre Haskell.

Consulta la [documentación de Hakyll](https://jaspervdj.be/hakyll/) para construir sitios estáticos.

## Conclusión

Este post demuestra que todas las funcionalidades están integradas:

- ✓ Small caps para NASA, HTML, CSS, etc.
- ✓ Resaltado de sintaxis con Pygments
- ✓ Matemáticas con KaTeX ($\LaTeX$ hermoso)
- ✓ Diagramas TikZ compilados a SVG
- ✓ Tabla de contenidos automática
- ✓ Notas al pie
- ✓ Todas las características estándar de Markdown

¡Tu sitio Hakyll está completamente funcional!
