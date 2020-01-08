# Termination Types

```
p ::= P | N
r ::= p | {Some:p, None:p}
t ::= * | t1->r[S]t2


-----------------
G, x:t |-p x:N[]t


    G, x:t1 |-N e:p[S]t1
-----------------------------
G|-p0 (x)=>e: p0[](t1->p[S]t2)


G|-p0 e1:p1[S1]t1  G|-p1 e2:p2[S2]t1->p[S]t2
p3=p2+p  S3=(p2=P ? S1+S2 : S1+S2+S3)
--------------------------------------------
G|-p0 e2(e1): p3[S3]t2


G, fi: ti->pi[Si,fi]t'i, x: ti |-N ei: pi[Si]t'i
G, fi: ti->pi[Si]t'i |-p0 e': p[S]t
fi not in Si
------------------------------------------------
G |-p0 let rec fi = (xi)=>ei; e : p[S]t

```
