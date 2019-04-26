# Polynomial-Simplifier

A polynomial simplifier written in OCaml.

## Run

compile: `make`

run: `ocamlrun main.byte ./tests/[testfile]`

Will read polynomial expressions in the form of:
* ax+bx^n+c
* (a+bx)^n
* (ax+bx)*(cx+d)
and apply a fixed-point recursive function that will simplify the output until no progress had been made.

Notice multiplication is represented by an '*'.

Examples:
```
x*(x+2)
=>
((x*x)+(2*x))   (distribution)
(x^2+2x)
(x^2+2x)    <=== Final result
-------------------------------------------
(x+1) * (x+2) 
=> 
((x*(x+2))+(1*(x+2)))
((x*1)+(2*1)+(x*x)+(2*x))
(x^2+2x+x+2)
(x^2+3x+2)  <=== Final result
```
## Testing
Run the tests p1.txt, p2.txt, p3.txt, .. in the tests folder