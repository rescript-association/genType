# Termination Types

```
p ::= P | N
r ::= p | {Some:p, None:p}
t ::= * | t1->r[s]t2


-----------------
G, x:t |-p x:N[]t


    G, x:t1 |-N e:p[s]t2
-----------------------------
G|-p0 (x)=>e: p0[](t1->p[s]t2)


G|-p0 e1:p1[s1]t1  G|-p1 e2:p2[s2](t1->p[s]t2)
    p3=p2+p  s3=(p2=P ? s1+s2 : s1+s2+s)
----------------------------------------------
         G|-p0 e2(e1): p3[s3]t2


G, fi: ti->pi[si,fi]ti', x: ti |-N ei: pi[si]ti'
     G, fi: ti->pi[si]ti' |-p0 e': p[s]t
              fi not in si
------------------------------------------------
    G |-p0 let rec fi = (xi)=>ei; e : p[s]t

```

# Type Inference

```
s ::=
      S        Set Variable.
      Loop     May loop.
      f        May call f before making progress.
      p.s      If p==P then empty else s.
      s1+s2    Union.

G|-p0 e1:p1[s1]t1  G|-p1 e2:p2[s2](t1->p[s]t2)
----------------------------------------------
      G|-p0 e2(e1): (p2+p)[s1+s2+p2.s]t2
```
