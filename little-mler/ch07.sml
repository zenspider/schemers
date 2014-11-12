(* chapter 7 *)

(* Seventh Moral: Some functions consume valuse of arrow-type; some
produce values of arrow-type. *)

(* Control.Print.printDepth := 20; *)

fun eq_int (a : int, b : int) = a = b;
(eq_int : int*int -> bool);

fun identity(x) = x;

(identity : 'a -> 'a);

fun true_maker(x) = true;

(true_maker : 'a -> bool);

datatype bool_or_int = Hot of bool | Cold of int

fun hot_maker(x) = Hot;

(hot_maker : 'a -> (bool -> bool_or_int));

fun help(f) =
    Hot(true_maker(if true_maker(5)
                   then f
                   else true_maker));

(help : ('a -> bool) -> bool_or_int);

datatype chain = Link of (int * (int -> chain));

fun ints n =
    Link(n + 1, ints);

(ints : int -> chain);

ints 0;
ints 5;

fun skips n =
    Link(n+2, skips);

(skips : int -> chain);

fun divides_evenly (n, c) =
    eq_int((n mod c), 0)

fun is_mod_5_or_7 n =
    divides_evenly (n, 5) orelse divides_evenly (n, 7)

fun some_ints n =
    if is_mod_5_or_7 (n+1)
    then Link(n+1, some_ints)
    else some_ints (n+1)

fun chain_item (n, Link(i, f)) =
    if eq_int(n, 1)
    then i
    else chain_item(n-1, f(i));

(* pg 101 *)

chain_item(1, some_ints(0));
chain_item(6, some_ints(0));
chain_item(37, some_ints(0));

fun is_prime n =
    let fun has_no_divisors(n, c) =
            if eq_int(c, 1)
            then true
            else
                if divides_evenly(n, c)
                then false
                else has_no_divisors(n, c-1)
    in
        has_no_divisors(n, n-1)
    end;

(is_prime : int -> bool);

fun primes n =
    if is_prime(n+1)
    then Link(n+1, primes)
    else primes(n+1);

chain_item(12, primes(1));

fun fibs n m =
    Link(n + m, fibs m);
(fibs : int -> int -> chain);

Link(0, fibs(1));

fibs 1 2;

