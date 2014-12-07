(* chapter 9 *)

(* Ninth Moral: Some functions produce exceptions instead of values;
                some don't produce anything. Handle raised exceptions
                carefully. *)

(* Control.Print.printDepth := 20; *)

datatype 'a list = Empty | Cons of 'a * 'a list;

datatype bacon_or_index = Bacon | Index of int;

val l = Cons(Index 5, Cons(Index 13, Cons(Bacon, Cons(Index 8, Empty))));

fun is_bacon Bacon = true
  | is_bacon _ = false;

fun where_is Empty = 0
  | where_is(Cons(boi, tail)) =
    if is_bacon(boi)
    then 1
    else 1 + where_is(tail);

fun where_is Empty              = 0
  | where_is(Cons(Bacon, tail)) = 1
  | where_is(Cons(_, tail))     = 1 + where_is(tail);

val x = 3 = where_is l;

val x = false =
        (0 = where_is(Cons(Index 5, Cons(Index 13, Cons(Index 8, Empty)))));

exception No_bacon of int;

fun where_is Empty              = raise No_bacon 0
  | where_is(Cons(Bacon, tail)) = 1
  | where_is(Cons(_, tail))     = 1 + where_is(tail);

val x = 0 = ((where_is(Cons(Index 5, Cons(Index 13, Cons(Index 8, Empty)))))
             handle No_bacon(n) => n);

(* pg 138 *)

val boxen = Cons(Index 5, Cons(Index 4, Cons(Bacon, Cons(Index 2, Cons(Index 3, Empty)))));

val x = 3 = (where_is boxen handle No_bacon(n) => n);

exception Out_of_range;

fun list_item(n, Empty) = raise Out_of_range
  | list_item(n, Cons(abox, rest)) = if 1 = n
                                     then abox
                                     else list_item(n-1, rest);

(*
 * fun find(n, boxen) = check(n, boxen, list_item(n, boxen))
 * and check(n, boxen, Bacon) = n
 *   | check(n, boxen, Index i) = find(i, boxen);
 *)

fun find(n, boxen) = case list_item(n, boxen)
                      of Bacon => n
                       | Index n => find(n, boxen)

val x = 3 = find(1, boxen);

val bad_boxen = Cons(Index 5, Cons(Index 4, Cons(Bacon, Cons(Index 2, Cons(Index 7, Empty)))));

val x = ~1 = (find(1, bad_boxen) handle Out_of_range => ~1);

fun find(n, boxen) =
    check(n, boxen, list_item(n, boxen)) handle Out_of_range => find(n div 2, boxen)
and check(n, boxen, Bacon) = n
  | check(n, boxen, Index i) = find(i, boxen);

val x = 3 = find(1, bad_boxen);

fun path(n, boxen) = Cons(n, (check(boxen, list_item(n, boxen))
                              handle Out_of_range => path(n div 2, boxen)))
and check(boxen, Bacon) = Empty
  | check(boxen, Index n) = path(n, boxen);

val x = path(1, bad_boxen) = Cons(1, Cons(5, Cons(7, Cons(3, Empty))));

