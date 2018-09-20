# refactor-projects

A Clojure library designed to refactor Clojure projects.

## Usage

```clj
(refactor {:paths ["/home/jonas/prog/clojure"]
           :ops [[:rename "bluebell.utils.dsl" "bluebell.utils.dsl"]
                 [:rename "bluebell.utils.render-text" "bluebell.utils.render-text"]
                 [:rename "bluebell.utils.ebmd" "bluebell.utils.ebmd"]]
           :mode :execute})
```

## License

Copyright © 2018 Jonas Östlund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
