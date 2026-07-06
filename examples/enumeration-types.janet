# -*- mode: janet; -*-
# Enumeration types

(import deft)

(deft/defenum :dayname {"Domhnach" 1 "Luan" 2 "Máirt" 3 "Céadaoin" 4
                        "Déardaoin" 5 "Aoine" 6 "Satharn" 7})

# (deft/defenum :dayname {"понедєлок" 1 "второк" 2 "срєда" 3 "четврток" 4
#                        "петок" 5 "субота" 6 "недєлја" 7})

(deft/define day-number [d :dayname] :number
  (in (deft/enum-table :dayname) d))

(defn print-day-number [d]
  (print (string d " → " (day-number d))))

(print-day-number "Luan")
(print-day-number "Aoine")
(print-day-number "Satharn")

# Error caught at runtime
(try (day-number "Midnight")
     ([e]
      (printf "'Midnight' is not a day of the week\n  -> %s" e)))

# some type introspection

(print "deft/type returns:")
(print "  \"Domhnach\" → " (deft/type "Domhnach") " (core: " (type "Domhnach") ")")
