# PisoLang

[Actively developped version](https://github.com/Suikaaah/pisolang)

Piso (pie-so) Lang is a reversible functional programming language based on [1].
Our main objective is to make reversible programming more accessible for all users.
PisoLang is designed to resemble OCaml, while being equipped with the built-in function
`inv : 'A -> ~'A` that _inverts_ any function.

| Objective                      | Status                                  |
| :-:                            | :-:                                     |
| Type inference - soundness     | :white_check_mark:                      |
| Type safety - preservation     | :white_check_mark:                      |
| Reversible Turing completeness | :white_check_mark:                      |
| Partial isomorphism*           | :white_check_mark:                      |
| Type inference - completeness  | Underway (highly likely to be complete) |
| Type safety - progress         | Never (due to partiality)               |

*: For any well-typed function `f`,
   `f v` $\rightarrow^\*$ `v'` if and only if `inv f v'` $\rightarrow^\*$ `v`.

## Build / Run

`dune exec pisolang -- examples/nat.piso`

## Introduction

Here is an example featuring the reversible function `add`,
which adds two numbers while preserving what has been added, i.e., `add (m, n) = (m + n, n)`.
Note that numeric literals are interpreted in the following way:
`0 := Z`, `1 := S Z`, `2 := S (S Z)`, ...

#### Input

```ocaml
type nat = Z | S of nat
;;

iso rec add = case
| (m, 0)   <-> (m, 0)
| (m, S n) <-> let (m', n') = add (S m, n) in (m', S n')
in

add (3, 4)
```

#### Output

```ocaml
inv (built-in) : 'A -> ~'A
add : nat * nat <-> nat * nat

(7, 4)
- : nat * nat
```

- The inferred type is printed upon generalizing a function.
- The details of the built-in function `inv` are provided in "Inversion".
- Type definitions and a term to evaluate are separated by a double-semicolon `;;`.
- The keyword `case` initiates pattern matching just like `function` in OCaml.

#### Restrictions

Any program that violates any of these restrictions gets rejected by the type checker,
preventing you from running it.

- The left-hand sides of the branches, `(m, 0)` and `(m, S n)` in this case,
  do not overlap each other.
- The right-hand sides of the branches, `(m, 0)` and `(m', S n')` in this case,
  do not overlap each other.
- Variables are used linearly* inside pattern matching except for ones bound to functions.

*: details provided in "Duplication / Equality Check"

## Types

PisoLang supports algebraic data types as well as first-order type constructors.

#### Example

```ocaml
(* simple sum type *)
type bool = False | True

(* type with data-containing variants *)
type bool_or_unit = Bool of bool | Unit of unit

(* recursive type *)
type nat = Z | S of nat
(* special syntax for nat:
     0 := Z
     1 := S Z
     2 := S (S Z)
     ... *)

(* type with a type variable *)
type 'a list = Nil | Cons of 'a * 'a list
(* special syntax for list:
         x :: xs := Cons (x, xs)
   [x1; ...; xn] := x1 :: ... :: xn :: Nil *)

(* another type with a variable *)
type 'a option = None | Some of 'a

(* type with variables *)
type ('l, 'r) either = Left of 'l | Right of 'r
(* Note: variables must be wrapped in a tuple because
   the type would no longer be first-order otherwise. *)
```

## Terms and _Isos_

There are two kinds of types: _base_ and _iso_.
You can think of base types as non-functional types,
including the unit type and product types, for instance.
Iso types, on the other hand, are types given to functions. Iso types are of the form
`<iso_type> ::= <base_type> <-> <base_type> | <iso_type> -> <iso_type>`.
The left-right-arrow `<->` represents the type of pattern matching.
The right-arrow `->` represents that of a higher-order function.

For example, terms such as `(3, 5)` and `False` have base types.
Isos such as `case True <-> False | False <-> True` have iso types
(`bool <-> bool` in this particular case).

You never write type annotations as types are automatically inferred
by a modified version of Algorithm W.

#### Example

```ocaml
type nat       = Z | S of nat
type 'a list   = Nil | Cons of 'a * 'a list
type bool      = False | True
type 'a option = None | Some of 'a
;;

(* myunit : unit *)
let myunit = () in

(* mynone : 'a option *)
let mynone = None in
(* Note: types get generalized as much as possible upon each let-binding.
   I.e., `mynone` has type `'a option` for any base type 'a,
   which enables something like the following: *)

(* mylists : nat option list * bool option list *)
let mylists = ([Some 0; mynone], [Some False; mynone]) in

(* reversible function *)
iso not = case False <-> True | True <-> False in

(* higher-order function *)
iso map_option = fun f -> case
| None   <-> None
| Some x <-> Some (f x)
in

(* same but with syntactic sugar *)
iso map_option f = case
| None   <-> None
| Some x <-> Some (f x)
in

iso map_fst f = case (x, y) <-> (f x, y) in

(* same but with syntactic sugar *)
iso map_fst f (x, y) = (f x, y) in

(* Note: you sometimes need to be explicit like the following:
   (iso f  g  = ...) := (iso f = fun  g  -> ...)
   (iso f (g) = ...) := (iso f = case g <-> ...) *)

(* recursive function using the keyword `rec` *)
(* len l = (l, <length of l>) *)
iso rec len = case
| []      <-> ([], 0)
| x :: xs <-> let (l, n) = len xs in (x :: l, S n)
in
(* Note: as you can imagine, it is impossible to
   have an injective function that only produces the length.
   This fact corresponds to the linear use of variables. *)

(* ([True; False; False], 3) : bool list * nat *)
len [True; False; False]
```

## Inversion

#### Types
Variables indicating iso types are displayed in uppercase with a preceding tick:
`'A, ..., 'Z, 'AA, ..., 'AZ, 'BA, ...`.
The inverse type of an iso type `'A` is denoted by `~'A`, which is defined as follows:
`~('a <-> 'b) := 'b <-> 'a` and `~('A -> 'B) := ~'A -> ~'B`.

#### Terms

Any well-typed function can be inverted using the built-in function `inv`.
`inv f` evaluates to `f^(-1)`.

#### Definition of `f^(-1)`

```
                         x^(-1)    :=  x
              (fun x -> f)^(-1)    :=  fun x -> f^(-1)
              (fix x -> f)^(-1)    :=  fix x -> f^(-1)
                     (f g)^(-1)    :=  f^(-1) g^(-1)
          (case | p1 <-> e1            case | (p1 <-> e1)^(-1)
                | ...              :=       | ...
                | pn <-> en)^(-1)           | (pn <-> en)^(-1)

where (p <-> let p1 = f1 p'1 in        p' <-> let p'n = fn^(-1) pn in
             ...                   :=         ...
             let pn = fn p'n in               let p'1 = f1^(-1) p1 in
             p')^(-1)                         p
```

Fact: `f : 'A` if and only if `inv f : ~'A`.

#### Example

```ocaml
type nat       = Z | S of nat
type 'a list   = Nil | Cons of 'a * 'a list
type bool      = False | True
type 'a option = None | Some of 'a
;;

(* splits a list of pairs into two lists *)
(* split : ('a * 'b) list <-> 'a list * 'b list *)
iso rec split = case
| []           <-> ([], [])
| (x, y) :: zs <->
      let (xs, ys) = split zs in
      (x :: xs, y :: ys)
in

(* combine : 'a list * 'b list <-> ('a * 'b) list *)
iso combine = inv split in

iso map_option f = case
| None   <-> None
| Some x <-> Some (f x)
in

iso decr_if_some = inv (map_option (case x <-> S x)) in

(* same but with syntactic sugar *)
iso decr_if_some = map_option (case x <-> S x) |> inv in

let some_8 = Some 9 |> decr_if_some in

(* this one goes crazy *)
iso silly f0 f1 f2 f3 = f3 (case (x, y) <-> (f1 y, inv f2 f0 x)) (inv f2) in

()
```

## Duplication / Equality Check

Reversible Turing completeness depends on the ability to perform duplication
and its inverse, i.e., an equality check. Both can be easily done via patterns.

#### Example

```ocaml
type nat  = Z | S of nat
type bool = False | True
;;

iso dup (x) = (x, x) in

(* let (a, a) = (3, 4) would fail *)
let (a, a) = (3, 3) in

(* inv dup (2, 3) would fail *)
let two = inv dup (2, 2) in

(* (3, (False, False), 2) *)
(a, dup False, two)

```

Speaking of the linearity inside pattern matching,
any number of occurrences of a variable in a single pattern counts as one.

```ocaml
case x <->
    let (a, b) = (x, x) in
    (a, b) (* (a, b, x) would fail because x is already used *)
```

## Miscellaneous

```ocaml
(* in order from highest precedence to lowest precedence *)
f_1 . ... . f_n := case x <-> f_1 (... (f_n x))
f_1 * ... * f_n := case (x_1, ..., x_n) <-> (f_1 x_1, ..., f_n x_n)
(* left-associative *)
f + g := case Left x <-> Left (f x) | Right x <-> Right (g x)
```

## Contents of `examples`

- `rtm.piso`: a reversible Turing machine that computes bitwise negations
- `run_length.piso`: polymorphic run-length compression
- `msort.piso`: reversible polymorphic merge sort
- `isort.piso`: reversible polymorphic insertion sort
- `list.piso`: polymorphic operations on lists
- `nat.piso`: operations on natural numbers
- `misc.piso`: random stuff
- `tree.piso`: operations on trees
- `exists.piso`: O(n) search

## Contents of `vim`

- `syntax/piso.vim`: syntax highlighting
- `ftdetect/piso.vim`: file for vim to recognize the `.piso` file extension

## Reference(s)

[1] Chardonnet, K., Lemonnier, L., Valiron, B.: Semantics for a Turing-Complete Reversible
Programming Language with Inductive Types. vol. 299, pp. 19:1–19:19.
Schloss Dagstuhl – Leibniz-Zentrum für Informatik (2024).
https://doi.org/10.4230/LIPICS.FSCD.2024.19

