## Concurrency
  - In addition to the job model now doesn't fork as long as we're still in `hmc`
  - Prefers go's concurrency story
  - Not sure about coroutines yet
## Memory
  - GCed, python-like

## Module
  - I want to have a lua like module system, every name is stored in a dict. 
    - Easy to implement
    - Good for runtime reflection 

## OOP
  - Lua's metatable would probably be the way to go, we may use clojure's `meta` like functionality to store them, though.
