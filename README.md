# Oplot

Mathematical plotting library for ocaml

| 2D plot and LaTeX |  3D surface |
|-----|-----|
|![gamma](docs/oplot/Oplot/gamma.png)| ![surf3d](docs/oplot/Oplot/surf3d.png) |

+ 2D plots
  + function `y = f(x)`
  + parametric curves
  + can use mathematical functions from other libraries like `gsl`
  + can use LaTeX to display text and formulas
  + animations
  + Matrix or grid display
  + ...

+ 3D plots
  + parametric surfaces
  + 3D graphs `z = f(x,y)`
  + interactive 3D rotation and zoom
  + ...

+ Many renderers
  + GPU graphics (opengl/sdl)
  + Graphics package
  + high quality vector graphics in EPS or PDF files
  + xfig files
  + image screenshots

## Documentation

[Examples and API documentation](https://sanette.github.io/oplot/oplot/Oplot/index.html).

## GUI

An official standalone GUI for `oplot`:
[goplot](https://sanette.github.io/goplot/)

## Examples

The `top` directory contains examples that can be run from the ocaml
toplevel. See the [README](top/README.md) file.

## Dependencies

* For PDF output you need to install the `fig2dev` system package.

* For LaTeX display you need a working LaTeX installation; for
instance install the `texlive` system package.

* For using the Gnu Scientific Library, install the `gsl` opam package:
```
opam install gsl
```

* For rendering via the OCaml Graphics library (currently not
recommended), you need to use the
[`oplot-graphics`](https://github.com/sanette/oplot-graphics) opam
package.
