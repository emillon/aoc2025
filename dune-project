(lang dune 3.15)

(package (name aoc2025))

(dialect
 (name dots)
 (implementation
  (extension dots)
  (preprocess (run input/input.exe decode %{input-file} --format dots))))
