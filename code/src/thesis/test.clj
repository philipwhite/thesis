(def test-micusp-file "BIO.G0.01.1")
(def parses
     (thesis.parse/parse-sentences (thesis.data/load-micusp-file test-micusp-file)))
(def deps (thesis.parse/dependencies parses))
(def *k* (thesis.train-deps/count-reln deps "prep"))




