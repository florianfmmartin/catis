# catis

## Deps

- GCC
- Tup

## Built-ins

Look for `add_procedure` and `add_string_procedure`.

You can type `%defs` to see all defined procedures.

Procedures defined in catis it-self are inspectable via `unquote`.

```haskell
catis> 'map unquote
[{l f} $l len {s} 0 {i} [] [$i $s <] [$l $i @ $f up-eval <- $i 1 + {i}] while] 
```

## Future features

### Multi char variables


### Concurrency
Via joins: procedures that only run when all named inputs have been received.

```haskell
catis> [{a b} $a $b + print] (a b) 'add-printer join
catis> 4 'b add-printer
catis> 5 'a add-printer
9
catis>
```

### Virtual Machine

```haskell
catis> %vm
catis> 0 0 "red" %display
catis> [{x y} ["Mouse is at: " $x ", " $y] [prin] each print] (x y) '%mouse-moved join
```

