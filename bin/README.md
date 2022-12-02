# Compilable examples

The examples in this dir are the same as those from the
[top](../top/README.md) directory, but they are prepared to be
compiled to executables.

(They are actually generated from the 'top/*' examples using the
`make_ml.sh` script)

For instance, you can compile and run `example.ml` using

```
dune exec ./example.exe
```

In order to run the examples requiring the `gsl` library, just
uncomment the relevent lines in the `dune` file. (And, of course,
`opam install gsl`).
