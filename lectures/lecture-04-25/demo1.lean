/- 2024-04-25 -/
/- Snippets of *FUNCTIONAL PROGRAMMING* in Lean -/

import Library.Basic

/- # EVALUATING EXPRESSIONS -/

#eval 3 + 4
#eval String.append "Hello " "World!"
#eval String.append "it is " (if 1 > 2 then "yes" else "no")
#eval String.append "A" (String.append "B" "C")
#eval String.append (String.append "A" "B") "C"

/- # CHECKING TYPES OF EXPRESSIONS AND EVALUATING THEM -/

#check 1 + 2
#eval  1 + 2
#check 1 - 2
#eval  1 - 2
#check 1 - 2
#check (1 - 2 : ℤ)
#eval (1 - 2 : ℤ)

/- # FUNCTIONS AND DEFINITIONS -/

def maximum1 (n : ℕ) (k : ℕ) : ℕ :=
  if n < k then k else n
def maximum2 (n k : ℕ) : ℕ :=
  if n < k then k else n
def maximum3 (n k : ℕ) :=
  if n < k then k else n

/- # DEFINING TYPES -/

inductive myList (α : Type) : Type
  | nil : myList α
  | cons : α → myList α → myList α

inductive myNat where
  | zero : myNat
  | succ (n : myNat) : myNat

#check myNat.zero
#check myNat.succ
def isZero1 (n : myNat) : Bool :=
  match n with
  | myNat.zero => true
  | myNat.succ k => false
def pred1 (n : myNat) : myNat :=
  match n with
  | myNat.zero => myNat.zero
  | myNat.succ k => k

open myNat -- open namespace `myNat`

#check zero
#check succ
def isZero2 (n : myNat) : Bool :=
  match n with
  | zero => true
  | succ k => false
def pred2 (n : myNat) : myNat :=
  match n with
  | zero => zero
  | succ k => k

/- # DEFINING RECURSIVE FUNCTIONS -/

def even (n : myNat) : Bool :=
  match n with
  | zero => true
  | succ k => not (even k)

/- # Watch out for differences between OCaml and Lean:

For example, *lists* in `Lean` and `OCaml` may be
defined as follows, respectively:

inductive myList (α : Type) : Type
  | nil : myList α
  | cons : α → myList α → myList α

type intlist = Nil | Cons of int * intlist ;;
type 'a list = []  | (::) of 'a * 'a list ;;

-/
