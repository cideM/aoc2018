- x 1
- y 1

### s 1

    for a in 0..s - 1

    a 0
    x + s, y + a -> 2 1
    x + a, y + s -> 1 2

    x + s, y + s -> 2 2

```
  1 2 3 4
1   x  
2 x x
3 
4
```

### s 2

    for a in 0..s - 2

    a 0
    x + s, y + a -> 3 1
    x + a, y + s -> 1 3

    a 1
    x + s, y + a -> 3 2
    x + a, y + s -> 2 3

    x + s, y + s -> 3 3

```
  1 2 3 4
1   . x  
2 . . x
3 x x x
4
```

### s 2

```
  1 2 3 4
1   . . x  
2 . . . x
3 . . . x
4 x x x x
```