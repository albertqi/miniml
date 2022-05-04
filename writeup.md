# CS 51: Final Project
## Albert Qi
### May 4, 2022

### 1. Introduction

### 3. Lazy Expressions
```
<== let f = fun () -> print_endline "running..." in f (); f() ;;
running...
running...
==> ()
<== let f = lazy (print_endline "running...") in force f; force f ;;
running...
==> ()
```