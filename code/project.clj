(defproject thesis "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.2.1"]
		 [org.clojure/clojure-contrib "1.2.0"]
		 [org.clojars.gilesc/stanford-parser "1.6.7"]
		 [org.clojars.magnusrw/dependensee "1.4.2"]
		 [incanter "1.3.0-SNAPSHOT"]
		 [com.leadtune/clj-ml "0.1.3"]
                 [org.clojars.processing-core/org.processing.itext "1.5.1"]]
  :dev-dependencies [ [swank-clojure "1.3.0"] ]
  :jvm-opts ["-Xmx4g"])
