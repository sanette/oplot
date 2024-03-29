# Running the examples interactively

Once the `oplot` library is installed, examples can be run in an ocaml
toplevel (for instance `utop`), or directly like this (*):

```
ocaml ./example.ml
```

(*): This might require `tsdl-ttf` version >= 0.4.

Some of the examples make use of the `gsl` library.  Make sure your
system has the `libgsl` package and add the `ocaml` bindings with
`opam install gsl`.

The `#thread` directive seems to be necessary for the `emacs` ocaml toplevel,
but not for `utop` for some reason...

`example.ml`:
![example](example.png)

`gamma.ml`:
![gamma](gamma.png)

`logistique.ml`:
![logistique](logistique.png)

`magnetic.ml`:
![magnetic](magnetic.png)

`matrix.ml`:
![matrix](matrix.png)

`surf3d.ml`:
![surf3d](surf3d.png)
