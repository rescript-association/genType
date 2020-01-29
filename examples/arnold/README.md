# Termination Types

```
p ::= 0 | 1
r ::= p | {Some:p, None:p}
t ::= * | t1=>r[s]t2


-----------------
G, x:t |-p x:0[]t


    G, x:t1 |-0 e:p[s]t2
-----------------------------
G|-p0 (x)=>e: p0[](t1=>p[s]t2)


G|-p0 e1:p1[s1]t1  G|-p1 e2:p2[s2](t1=>p[s]t2)
    p3=p2+p  s3=(p2=1 ? s1+s2 : s1+s2+s)
----------------------------------------------
         G|-p0 e2(e1): p3[s3]t2


G, fi: ti=>pi[si,fi]ti', x: ti |-0 ei: pi[si]ti'
     G, fi: ti=>pi[si]ti' |-p0 e': p[s]t
              fi not in si
------------------------------------------------
    G |-p0 let rec fi = (xi)=>ei; e : p[s]t

```

# Type Inference

```
s ::=
      S        Set variable.
      Loop     May loop.
      f        May call f before making progress.
      p.s      If p==1 then empty else s.
      s1+s2    Union.

p ::=
      P        Progress variable.
      0        Does not make progress.
      1        Makes.
      p1+p2    Makes progress if either does.
      p1|p2    Makes progress if both do.

t ::=
      T           Type variable.
      *           Base type.
      t1=>p[s]t2  Function that calls s before making progress.


-----------------
G, x:t |-p x:0[]t


G, x:T1 |-0 e:p[s]t2  T1 fresh
------------------------------
G|-p0 (x)=>e: p0[](T1=>p[s]t2)


G|-p0 e1:p1[s1]t1  G|-p1 e2:p2[s2]t  P,S,T2 fresh
-------------------------------------------------
G|-p0 e2(e1): (p2+P)[s1+s2+p2.S]T2  t=t1=>P[S]T2


G, fi: Ti=>Pi[Si+fi]Ti', x: Ti |-0 ei: pi[si]ti'
             Ti,Ti',Pi,Si fresh
     G, fi: ti=>pi[si]ti' |-p0 e': p[s]t          
------------------------------------------------
    G |-p0 let rec fi = (xi)=>ei; e : p[s]t 
    pi=Pi si=Si ti'=Ti'  fi not in si

```

# Example Inference


```reason
let rec iter = (f, x) => { f(x); iter(f,x); };
```
