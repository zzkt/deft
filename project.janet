(declare-project
 :name "deft"
 :description "Gradual typing with inference, static analysis, and runtime checks."
 :author "nik gaffney <nik@fo.am>"
 :url "https://codeberg.org/zzkt/deft"
 :license "EUPL-1.2"
 :version "0.1.13"
 :source-paths ["deft"]
 :test-paths ["test"])

(declare-source
 :source ["deft/init.janet"
          "deft/core.janet"
          "deft/dispatch.janet"
          "deft/inference.janet"
          "deft/check.janet"
          "deft/wolves.janet"
          "deft/defines.janet"
          "deft/pp.janet"
          "deft/typecase.janet"
          "deft/unify.janet"]
 :prefix "deft")
