(* chapter 4 *)

(* Fourth Moral: Some functions consume values of star type; some produce values of star type. *)

(* Control.Print.printDepth := 20; *)

datatype meza    = Shrimp | Calimari | Escagots | Hummus ;
datatype main    = Steak | Ravioli | Chicken | Eggplant ;
datatype salad   = Green | Cucumber | Greek ;
datatype dessert = Sundae | Mousse | Torte ;

(Calimari, Ravioli, Greek, Sundae); (* : meza * main * salad * dessert *)

fun add_a_steak(Shrimp)
    = (Shrimp, Steak)
  | add_a_steak(Calimari)
    = (Calimari, Steak)
  | add_a_steak(Escagots)
    = (Escagots, Steak)
  | add_a_steak(Hummus)
    = (Hummus, Steak);

add_a_steak(Hummus);

fun add_a_steak(x)
    = (x, Steak);

(add_a_steak : meza -> (meza * main));

add_a_steak(Hummus);

fun eq_main(Steak, Steak)       = true
  | eq_main(Ravioli, Ravioli)   = true
  | eq_main(Chicken, Chicken)   = true
  | eq_main(Eggplant, Eggplant) = true
  | eq_main(a, b)               = false;

(eq_main : main * main -> bool);

fun has_steak(a_meza, Steak, a_dessert)  = true
  | has_steak(a_meza, a_main, a_dessert) = false;

(has_steak : meza * main * dessert -> bool);

fun has_steak(a:meza, Steak, d:dessert)  = true
  | has_steak(a:meza, a_main, d:dessert) = false;

fun add_a_steak(x:meza) : (meza * main)
    = (x, Steak);
