{:user
 {
  :dependencies [
                 [lein-cljfmt "0.6.4"]
                 [javax.xml.bind/jaxb-api "2.4.0-b180830.0359"]
                 ]
  :dev {
        :dependencies [
                 [cider/piggieback "0.4.1"]
                       ]
        }
  :plugins [
      [lein-cljfmt "0.6.4"]
      [lein-ancient "0.6.15"]
      [cider/cider-nrepl "0.21.1"]
   ]
  :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}
  }
 }
