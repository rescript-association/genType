# Termination Types

```
p ::= P | N
r ::= p | {Some:p, None:p}
t ::= * | t1->r[S]t2


-----------------
G, x:t |-p x:N[]t


    G, x:t1 |-N e:p[S]t2
-----------------------------
G|-p0 (x)=>e: p0[](t1->p[S]t2)


G|-p0 e1:p1[S1]t1  G|-p1 e2:p2[S2](t1->p[S]t2)
    p3=p2+p  S3=(p2=P ? S1+S2 : S1+S2+S)
----------------------------------------------
         G|-p0 e2(e1): p3[S3]t2


G, fi: ti->pi[Si,fi]ti', x: ti |-N ei: pi[Si]ti'
     G, fi: ti->pi[Si]ti' |-p0 e': p[S]t
              fi not in Si
------------------------------------------------
    G |-p0 let rec fi = (xi)=>ei; e : p[S]t

```

# Type Inference

```
s ::=
      S        Set Variable.
      Loop     May loop.
      f        May call f before making progress.
      p.s      If p==P then empty else s.
      s1+s2    Union.

G|-p0 e1:p1[S1]t1  G|-p1 e2:p2[S2](t1->p[S]t2)
----------------------------------------------
      G|-p0 e2(e1): (p2+p)[S1+S2+p2.S]t2
```
