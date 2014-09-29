(* chapter 5 *)

(* Fifth Moral: Write the first draft of the function following all the morals. When it is correct and no sooner, simplify. *)

(* Control.Print.printDepth := 20; *)

datatype fish = Anchovy | Lox | Tuna ;

datatype 'a pizza =
         Bottom
         | Topping of ('a * ('a pizza));

Topping(Anchovy,
        Topping(Tuna,
                Topping(Anchovy, Bottom)));

fun rem_anchovy(Bottom)              = Bottom
  | rem_anchovy(Topping(Anchovy, p)) = rem_anchovy(p)
  | rem_anchovy(Topping(Tuna, p))    = Topping(Tuna, rem_anchovy(p))
  | rem_anchovy(Topping(Lox, p))     = Topping(Lox, rem_anchovy(p));

(rem_anchovy : (fish pizza) -> (fish pizza));

fun rem_anchovy(Bottom)              = Bottom
  | rem_anchovy(Topping(Anchovy, p)) = rem_anchovy(p)
  | rem_anchovy(Topping(t, p))       = Topping(t, rem_anchovy(p));

(rem_anchovy : (fish pizza) -> (fish pizza));

(* p 59 *)

fun rem_tuna(Bottom)           = Bottom
  | rem_tuna(Topping(Tuna, p)) = rem_tuna(p)
  | rem_tuna(Topping(t, p))    = Topping(t, rem_tuna(p));

(rem_tuna : (fish pizza) -> (fish pizza));

(* p 62 *)

fun rem_fish(f, Bottom)                    = Bottom
  | rem_fish(Tuna,    Topping(Tuna,    p)) = rem_fish(Tuna, p)
  | rem_fish(Anchovy, Topping(Anchovy, p)) = rem_fish(Anchovy, p)
  | rem_fish(Lox,     Topping(Lox,     p)) = rem_fish(Lox, p)
  | rem_fish(a,       Topping(b,       p)) = Topping(b, rem_fish(a, p));

(rem_fish : (fish * (fish pizza)) -> (fish pizza));

rem_fish(Tuna, Topping(Anchovy, Topping(Tuna, Topping(Lox, Bottom))))
= Topping (Anchovy,Topping (Lox,Bottom));

(* p 64 *)

fun eq_fish(Anchovy, Anchovy) = true
  | eq_fish(Tuna, Tuna)       = true
  | eq_fish(Lox, Lox)         = true
  | eq_fish(a, b)             = false;

fun rem_fish(x, Bottom) = Bottom
  | rem_fish(x, Topping(t, p))
    = if eq_fish(t, x)
      then rem_fish(x, p)
      else Topping(t, rem_fish(x, p));

(rem_fish : (fish * (fish pizza)) -> (fish pizza));

rem_fish(Tuna, Topping(Anchovy, Topping(Tuna, Topping(Lox, Bottom))))
= Topping (Anchovy,Topping (Lox,Bottom));

(* p 68 *)

fun eq_int(n:int, m:int) = (n = m);

fun rem_int(x, Bottom) = Bottom
  | rem_int(x, Topping(t, p))
    = if eq_int(t, x)
      then rem_int(x, p)
      else Topping(t, rem_int(x, p));

rem_int(3, Topping(2, Topping(3, Topping(2, Bottom))));

(* p 69 *)

fun subst_fish(n, a, Bottom) = Bottom
  | subst_fish(n, a, Topping(t, p))
    = if eq_fish(t, a)
      then Topping(n, subst_fish(n, a, p))
      else Topping(t, subst_fish(n, a, p));

(subst_fish : (fish * fish * (fish pizza)) -> (fish pizza));

