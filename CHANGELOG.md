# 0.71

fix version number (0.7 was considered lower than 0.50)

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

# 0.60 -- 2022/11/22 -- upgrade to SDL2

Drop the dependency on the old SDL12 library (and hence ocamlsdl),
switch to `tsdl`, `tsdl-image`, `tsdl-ttf`.

# 0.50 -- last version with SDL12

# 0.1, 0.2, 0.3, 0.4 -- 2007 (yes, that's old)
