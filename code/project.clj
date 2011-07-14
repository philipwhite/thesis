(defproject thesis "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.2.0"]
		 [org.clojure/clojure-contrib "1.2.0"]
		 [org.clojars.gilesc/stanford-parser "1.6.7"]
		 [org.clojars.magnusrw/dependensee "1.4.2"]
		 [incanter "1.2.3"]
		 [com.leadtune/clj-ml "0.1.1-SNAPSHOT"]]
  :dev-dependencies [ [swank-clojure "1.3.0"] ]
  :jvm-opts ["-Xmx1g"])
