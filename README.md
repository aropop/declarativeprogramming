# Project declarative programming

## How to run

Execute following command from shell (not directory should be in the root of the project)

```bash
prolog main.pl
```

In the Prolog shell use 

```prolog
consult('small_instance.txt').
```

to load the small instance data set. All data sets end with the `.txt` extension.

## Commands
* is_valid/1 is_valid(?S)
* cost/2 cost(+S, -C)
* find_optimal/1 find_optimal(-S)
* find_heuristically/1 find_heuristically(-S)
* pretty_print/1 pretty_print(+S)
