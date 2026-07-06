# -*- mode: janet; -*-
# Blame driven debugging

(import deft)

(deft/enable-checking true)

# entry blame - caller passes wrong type
(deft/deftfn greet [n :string] :string
  (string "Hello, " n))

(print (greet "nasty!\n"))

(try (greet 42)
     ([e] (printf "entry blame (caller)\n  -> %s \n" e)))


# return blame - function returns wrong type
(deft/deftfn broken [x :number] :string
  (+ x 1))

(try (broken 5)
     ([e] (printf "return blame (function)\n  -> %s \n" e)))
