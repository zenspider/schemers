(* chapter 6 *)

(* Sixth Moral: As datatype definitions get more complicated, so do
the functions over them. *)

(* Control.Print.printDepth := 20; *)

datatype fruit = Peach | Apple | Pear | Lemon | Fig ;

datatype tree =
         Bud
       | Flat of fruit * tree
       | Split of tree * tree ;

fun flat_only(Bud)         = true
  | flat_only(Flat(f, t))  = flat_only(t)
  | flat_only(Split(a, b)) = false ;

(flat_only : tree -> bool);

fun split_only(Bud)         = true
  | split_only(Flat(f, t))  = false
  | split_only(Split(l, r)) = split_only(l) andalso split_only(r) ;

(split_only : tree -> bool);

fun contains_fruit(Bud)         = false
  | contains_fruit(Flat(f, t))  = true
  | contains_fruit(Split(l, r)) = contains_fruit(l) orelse contains_fruit(r);

(contains_fruit : tree -> bool);

fun larger_of(a, b) = if a > b
                      then a
                      else b;

(larger_of : int * int -> int);

fun height(Bud) = 0
  | height(Flat(f, t)) = 1 + height(t)
  | height(Split(l, r)) = 1 + larger_of(height(l), height(r));

(height : tree -> int);

height(Split(Bud, Bud));

fun eq_fruit(Peach, Peach) = true
  | eq_fruit(Apple, Apple) = true
  | eq_fruit(Pear, Pear)   = true
  | eq_fruit(Lemon, Lemon) = true
  | eq_fruit(Fig, Fig)     = true
  | eq_fruit(a, b)         = false;

(eq_fruit : fruit * fruit -> bool);

fun subst_in_tree(n, a, Bud)
    = Bud
  | subst_in_tree(n, a, Flat(f, t))
    = if eq_fruit(f, a)
      then Flat(n, subst_in_tree(n, a, t))
      else Flat(a, subst_in_tree(n, a, t))
  | subst_in_tree(n, a, Split(l, r))
    = Split(subst_in_tree(n, a, l),
            subst_in_tree(n, a, r));

(subst_in_tree : fruit * fruit * tree -> tree);

fun occurs(a, Bud) = 0
  | occurs(a, Flat(f, t)) = if eq_fruit(f, a)
                            then 1 + occurs(a, t)
                            else occurs(a, t)
  | occurs(a, Split(l, r)) = occurs(a, l) + occurs(a, r);

(occurs : fruit * tree -> int);

datatype 'a slist = Empty | Scons of (('a sexp) * ('a slist))
and 'a sexp  = An_atom of 'a | A_slist of ('a slist);

fun occurs_in_slist(a, Empty)
    = 0
  | occurs_in_slist(a, Scons(s, y))
    = occurs_in_sexp(a, s) + occurs_in_slist(a, y)
and
    occurs_in_sexp(a, An_atom(b))
    = if eq_fruit(a, b)
      then 1
      else 0
  | occurs_in_sexp(a, A_slist(b))
    = occurs_in_slist(a, b);

(occurs_in_slist : fruit * fruit slist -> int);
(occurs_in_sexp  : fruit * fruit sexp -> int);

fun subst_in_slist(n, a, Empty)
    = Empty
  | subst_in_slist(n, a, Scons(s, y))
    = Scons(subst_in_sexp (n, a, s),
            subst_in_slist(n, a, y))
and
    subst_in_sexp(n, a, An_atom(b))
    = if eq_fruit(a, b)
      then An_atom(n)
      else An_atom(b)
    | subst_in_sexp(n, a, A_slist(b))
      = A_slist(subst_in_slist(n, a, b));

(subst_in_slist : fruit * fruit * fruit slist -> fruit slist);
(subst_in_sexp : fruit * fruit * fruit sexp -> fruit sexp);

(* at this point, I'm terribly bored... I think I get the point, but
I'm not willing to work through 5 more pages. *)
