# -*- mode: janet; -*-
# deft/init: module load order & deft-refs setup

(import ./dispatch :prefix "" :export true)
(import ./core :prefix "" :export true)
(import ./unify :prefix "" :export true)
(import ./inference :prefix "" :export true)
(import ./check :prefix "" :export true)
(import ./wolves :prefix "" :export true)
(import ./defines :prefix "" :export true)
(import ./pp :prefix "" :export true)

# Reference table for macroexpansion access to runtime declarations
(put deft-refs 'cast cast)
(put deft-refs 'tag-value tag-value)
(put deft-refs 'register-type register-type)
(put deft-refs 'register-guard register-guard)
(put deft-refs 'type-predicate type-predicate)
(put deft-refs 'pp-str pp-str)
(put deft-refs 'pp pp)
(put deft-refs '*enum-tables* (fn [] *enum-tables*))
(put deft-refs 'var-types (fn [] *var-types*))
(put deft-refs 'checking-enabled (fn [] *checking-enabled*))
(put deft-refs 'unregister-type unregister-type)
(put deft-refs 'untype untype)
(put deft-refs 'inferred-fn-types (fn [] *inferred-fn-types*))
