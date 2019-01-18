```text
#ip 4

# a b c d ip e
[ 1 0 0 0 0  0]

0 addi 4 16 4 # GOTO 17
17 addi 5 2 5 # e += 2; e = 2; IP = 18
18 mulr 5 5 5 # e = e _ e; e = 4; IP = 19
19 mulr 4 5 5 # e = ip _ e; e = 76; IP = 20
20 muli 5 11 5 # e = e _ 11; e = 836; IP = 21
21 addi 2 4 2 # c += 4; c = 4; IP = 22
22 mulr 2 4 2 # c = c _ ip; c = 88; IP = 23
23 addi 2 5 2 # c = c + 5; c = 93; IP = 24
24 addr 5 2 5 # e = c + e; e = 929; IP = 25
25 addr 4 0 4 # GOTO 27
-- 26 seti 0 9 4
27 setr 4 2 2 # c = ip; c = 27; IP = 28
28 mulr 2 4 2 # c _= ip; c = 756; IP = 29
29 addr 4 2 2 # c += ip; c = 785; IP = 30
30 mulr 4 2 2 # c _= ip; c = 23550; IP = 31
31 muli 2 14 2 # c _= 14; c= 329700; IP = 32
32 mulr 2 4 2 # c _= ip; c = 10550400; IP = 33
33 addr 5 2 5 # e += c; e = 10551329; IP = 34
34 seti 0 0 0 # a = 0; IP = 35
35 seti 0 8 4 # IP = 0; GOTO 1; IP = 1
```

```text
  0 1 2        3 4  5
# a b c        d ip e
[ 0 0 10550400 0 0  10551329]
```

[1] D = 1
[2] B = 1

if (D\*B == 10551329) A += D
else B += 1

GOTO B > E ? 12 : 3

[12] D += 1

GOTO D > E ? EXIT : 2

// In words

Set B and D to 1. At first, D \* B won't be 10551329. So we increase B by 1 and as long as it's smaller than 10551329 we keep doing that. So initially we just increase B by 1. Eventually D \* B will be 10551329. Then we add D to A. If B is larger than E, we set it to 1 again, and now increase D.

Once again, D \* B won't be 10551329 for a while, so we increase B again. Note that we never reset D to 1. So this is really a nested for loop. And everytime D \_ B == 10551329 we add that to A. Sooooo we sum all factors of 10551329. Wolfram Alpha.
