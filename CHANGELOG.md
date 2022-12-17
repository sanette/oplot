# 0.7-pre -- 2022/12/17 -- (breaking change) remove Graphics dependency

Graphics is now optional. Use the `oplot-graphics` package to restore it.

Slight breaking change: module `Internal` is now part of `Plt`
(sounder signature). Just use `Oplot.Plt.Internal` instead of
`Oplot.Internal` and everything should be fine.

# 0.6 -- 2022/11/22 -- upgrade to SDL2

Drop the dependency on the old SDL12 library (and hence ocamlsdl),
switch to `tsdl`, `tsdl-image`, `tsdl-ttf`.

# 0.5 -- last version with SDL12

# 0.1, 0.2, 0.3, 0.4 -- 2007
