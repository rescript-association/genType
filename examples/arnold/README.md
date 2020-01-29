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
              fi not in si,ti'
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
    pi=Pi si=Si ti'=Ti'  fi not in si,ti'

```

Constraint equations:
```
  0+p=p  1+p=1  p1+p2=p2+p1  p1+(p2+p3)=(p1+p2)+p3
```

```
  0.s=s  1.s=[]
```

```
  f-f=Loop  s+Loop=Loop
  p.Loop ~~~> add  p=1
```


# Example Inference


```reason
let rec iter = (f, x) => { f(x); iter(f,x); };
```

```
iter:(*=>P[S]*)=>P1[S1+iter](*=>P2[S2]*), f:*=>P3[S3]*, x:* |-0 f(x) : ???

     |-0 f(x) : (0+P3)[0.S3]*
     |-0 f(x) : P3[S3]*
     
     |-P3 iter(f) : (P3+P1)[P3.(S1+iter)](*=>P2[S2]*)
       P3=P  S3=S
     |-P iter(f) : (P+P1)[P.(S1+iter)](*=>P2[S2]*)
     
     |-(P+P1) iter(f,x) : (P+P1+P2)[P.(S1+iter)+(P+P1).S2]*


     |-0 (x) => { f(x); iter(f,x); } : 0[](*=>(P+P1+P2)[S+P.(S1+iter)+(P+P1).S2]*)
       P1=0  S1=[]  P2=P+P1+P2  S2=S+P.(S1+iter)+(P+P1).S2
       iter not in S+P.(S1+iter)+(P+P1).S2  

       P2=P+P2  S2=S+P.iter+P.S2
       iter not in S+P.iter+P.S2  
```


Resolving "not in":

```
S+P.iter+P.S2 - iter =
S+P.Loop+P.S2 =  ---> add P=1
S

S2=S
P2=1
```

After Applying substitutions:

```
iter:(*=>1[S]*)=>0[](*=>1[S]*)
```

In words: `iter` expects as first parameter a function that: makes progress when called, and let `S` bet the set of functions it calls before making progress. When supplied the first argument, `iter` does not make progress. When supplied the second argument, it makes progress, and calls functions in set `S` before making progress.



