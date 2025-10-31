# 0.82 -- 2025/10/31

* back to functors

we save the "virtual modules" technique in a separate branch for when
the fix https://github.com/ocaml/dune/issues/12636#event-20554298282
reaches the ocaml.ci

Also I don't know if "virtual modules" is ideal, because in the
toplevel it requires #require "oplot.default" (one cannot use #require
"oplot", although dune does accept (libraries oplot))

# 0.81 -- 2025/10/27

* using dune "virtual modules" instead of parametric functor (for Graphics implementation)

# 0.80 -- 2025/09/27

* getting rid of lablgl; we use tgsl ang gl-legacy instead

# 0.71 -- 2022/01/09

* back to old version scheme. Opam was confused!
* fix filename for PDF export

# 0.7 -- 2022/12/22 -- (breaking change) remove Graphics dependency

* Graphics is now optional. Use the `oplot-graphics` package to restore it.
We use a functor signature for this.

* Slight breaking change: module `Internal` is now part of `Plt`
(sounder signature). Just use `Oplot.Plt.Internal` instead of
`Oplot.Internal` and everything should be fine.

Better Mac OS support:
* fix closing window problem from the toplevel
  (https://github.com/sanette/oplot/issues/3, thanks @anentropic)
* support for crisp graphics on retina screen

# 0.6 -- 2022/11/22 -- upgrade to SDL2

Drop the dependency on the old SDL12 library (and hence ocamlsdl),
switch to `tsdl`, `tsdl-image`, `tsdl-ttf`.

# 0.50 -- last version with SDL12

# 0.1, 0.2, 0.3, 0.4 -- 2007 (yes, that's old)
