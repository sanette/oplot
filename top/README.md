# Running the examples

(This requires `tsdl-ttf` version >= 0.4.)

Once the `oplot` library is installed, examples can be run in an ocaml
toplevel (for instance `utop`), or directly like this:

```
ocaml ./example.ml
```

Some of the examples make use of the `gsl` library.  Make sure your
system has the `libgsl` package and add the `ocaml` bindings with
`opam install gsl`.

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
