(* chapter 2 *)

datatype shish_kebab = Skewer
                     | Onion of shish_kebab
                     | Lamb of shish_kebab
                     | Tomato of shish_kebab;

Skewer;
Onion(Skewer);
Onion(Lamb(Onion(Skewer)));

fun only_onions(Skewer)
    = true
  | only_onions(Onion(x))
    = only_onions(x)
  | only_onions(Lamb(x))
    = false
  | only_onions(Tomato(x))
    = false;

(only_onions : shish_kebab -> bool);

only_onions(Skewer);
only_onions(Onion(Skewer));
only_onions(Onion(Onion(Onion(Skewer))));
only_onions(Onion(Lamb(Onion(Skewer))));

fun is_vegetarian(Skewer)
    = true
  | is_vegetarian(Onion(x))
    = is_vegetarian(x)
  | is_vegetarian(Tomato(x))
    = is_vegetarian(x)
  | is_vegetarian(Lamb(x))
    = false;

(is_vegetarian : shish_kebab -> bool);

is_vegetarian(Skewer);
is_vegetarian(Onion(Skewer));
is_vegetarian(Onion(Onion(Onion(Skewer))));
is_vegetarian(Onion(Lamb(Onion(Skewer))));

datatype 'a shish = Bottom of 'a
                  | Onion  of 'a shish
                  | Lamb   of 'a shish
                  | Tomato of 'a shish;

datatype rod = Dagger
             | Fork
       | Sword;

datatype plate = Gold_plate
               | Silver_plate
       | Brass_plate;

Onion(Tomato(Bottom(Dagger)));
Onion(Tomato(Bottom(Gold_plate)));

fun is_veggie(Bottom(x))
    = true
  | is_veggie(Onion(x))
    = is_veggie(x)
  | is_veggie(Tomato(x))
    = is_veggie(x)
  | is_veggie(Lamb(x))
    = false;

(is_veggie : 'a shish -> bool);

is_veggie(Onion(Tomato(Bottom(Dagger))));
is_veggie(Onion(Tomato(Bottom(Gold_plate))));
is_veggie(Onion(Lamb(Bottom(Gold_plate))));
is_veggie(Onion(Tomato(Bottom(52))));

fun what_bottom(Bottom(x))
    = x
  | what_bottom(Onion(x))
    = what_bottom(x)
  | what_bottom(Tomato(x))
    = what_bottom(x)
  | what_bottom(Lamb(x))
    = what_bottom(x);

(what_bottom : 'a shish -> 'a);

what_bottom(Bottom(52));
what_bottom(Tomato(Onion(Lamb(Bottom(52)))));

