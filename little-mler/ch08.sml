(* chapter 8 *)

(* Eighth Moral: Replace stars by arrows to reduce the number of
                 values consumed and to increase the generality of the
                 function defined. *)

Control.Print.printDepth := 20;

datatype 'a list = Empty
                 | Cons of 'a * 'a list

datatype orapl = Orange | Apple

fun eq_orapl(Orange, Orange) = true
  | eq_orapl(Apple, Apple)   = true
  | eq_orapl _               = false;

(eq_orapl : orapl * orapl -> bool);

fun subst_int (n,a,Empty)     = Empty
  | subst_int (n,a,Cons(e,t)) = if a = e
                                then Cons(n, subst_int(n, a, t))
                                else Cons(e, subst_int(n, a, t));

(subst_int : int * int * int list -> int list);

fun subst_orapl(n, a, Empty)      = Empty
  | subst_orapl(n, a, Cons(e, t)) = if eq_orapl(a, e)
                                    then Cons(n, subst_orapl(n, a, t))
                                    else Cons(e, subst_orapl(n, a, t));

fun subst(rel, n, a, Empty)      = Empty
  | subst(rel, n, a, Cons(e, t)) = if rel(a, e)
                                   then Cons(n, subst(rel, n, a, t))
                                   else Cons(e, subst(rel, n, a, t));

(subst : ('b * 'a -> bool) * 'a * 'b * 'a list -> 'a list);

fun eq_int (a : int, b : int) = a = b;

subst(eq_int, 11, 15,
      Cons(15, Cons(6, Cons(15, Cons(17, Cons(15, Cons(8, Empty)))))));

(* pg 114 *)

fun less_than(a, b) = a < b;

fun in_range((small, large), x) =
    if less_than(small, x)
    then less_than(x, large)
    else false

fun subst_pred(pred, n, Empty)      = Empty
  | subst_pred(pred, n, Cons(e, t)) = if pred e
                                      then Cons(n, subst_pred(pred, n, t))
                                      else Cons(e, subst_pred(pred, n, t));

(subst_pred : ('a -> bool) * 'a * 'a list -> 'a list);

fun is_15(n) = eq_int(n, 15);

subst_pred(is_15, 11,
           Cons(15, Cons(6, Cons(15, Cons(17, Cons(15, Cons(8, Empty)))))));

fun less_than_15(x) = less_than(x, 15);

(* pg 118 *)

subst_pred(less_than_15, 11,
           Cons(15, Cons(6, Cons(15, Cons(17, Cons(15, Cons(8, Empty)))))));

fun in_range_11_16 n = less_than(11, n) andalso less_than(n, 16);

subst_pred(in_range_11_16, 22,
           Cons(15, Cons(6, Cons(15, Cons(17, Cons(15, Cons(8, Empty)))))));

fun in_range_c(min, max)(n) = less_than(min, n) andalso less_than(n, max);

(in_range_c : int * int -> int -> bool);

subst_pred(in_range_c(11, 16), 22,
           Cons(15, Cons(6, Cons(15, Cons(17, Cons(15, Cons(8, Empty)))))));

fun subst_c(pred)(n, Empty) = Empty
  | subst_c(pred)(n, Cons(e, t)) = if pred(e)
                                   then Cons(n, subst_c pred (n, t))
                                   else Cons(e, subst_c pred (n, t));

(subst_c : ('a -> bool) -> 'a * 'a list -> 'a list);

fun combine(Empty, l) = l
  | combine(Cons(a, l1), l2) = Cons(a, combine(l1, l2));

(combine : 'a list * 'a list -> 'a list);

fun combine_c(Empty)(l) = l
  | combine_c(Cons(a, l1))(l2) = Cons(a, combine_c l1 l2);

(* pg 128 *)

(* so confused at this point *)

fun base l2 = l2

fun combine_s(Empty) = base
  | combine_s(Cons(x, l)) =
    make_cons(x, combine_s(l))
and make_cons(x, f)(l2) = Cons(x, f(l2));

(* the whole deal with exstensionally vs intensionally equal is... confusing *)
