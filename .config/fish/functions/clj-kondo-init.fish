function clj-kondo-init
  clj-kondo --lint (echo \"(lein classpath)\") --cache
end
