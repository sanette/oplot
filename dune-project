(lang dune 1.11)
(name oplot)
(version "0.50")
(generate_opam_files true)
(license GPL2)
(maintainers "Vu Ngoc San <san.vu-ngoc@laposte.net>")
(authors "Vu Ngoc San <san.vu-ngoc@laposte.net>")
(source (github sanette/oplot))

(package
 (name oplot)
 (synopsis "Mathematical plotter library for ocaml")
 (description "High-quality plotting of 2D usual mathematical functions, including animations and LaTeX formulas. Supports also 3D surfaces with interactive rotating.")
 (tags ("plot" "2D" "3D" "math"))
 (depends
  (ocamlsdl (>= 0.9.1))
  (lablgl (>= 1.06))
  (graphics (>= 4.05.0))
  (ocaml (>= 4.05.0))
  (dune (>= 1.11))))

