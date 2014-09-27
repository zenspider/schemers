(* chapter 3 *)

(* Third Moral: Functions that produce values of a datatype must use the associated constrctors to build data of that type. *)

Control.Print.printDepth := 20;

datatype pizza = Crust
               | Cheese of pizza
               | Onion of pizza
               | Anchovy of pizza
               | Sausage of pizza;

Anchovy(Onion(Anchovy(Anchovy(Cheese(Crust)))));

fun remove_anchovy(Crust)
    = Crust
  | remove_anchovy(Cheese(x))
    = Cheese(remove_anchovy(x))
  | remove_anchovy(Onion(x))
    = Onion(remove_anchovy(x))
  | remove_anchovy(Anchovy(x))
    = remove_anchovy(x)
  | remove_anchovy(Sausage(x))
    = Sausage(remove_anchovy(x));

(remove_anchovy : pizza -> pizza);

remove_anchovy(Anchovy(Onion(Anchovy(Anchovy(Cheese(Crust))))));

fun top_anchovy_with_cheese(Crust)
    = Crust
  | top_anchovy_with_cheese(Cheese(x))
    = Cheese(top_anchovy_with_cheese(x))
  | top_anchovy_with_cheese(Onion(x))
    = Onion(top_anchovy_with_cheese(x))
  | top_anchovy_with_cheese(Anchovy(x))
    = Cheese(Anchovy(top_anchovy_with_cheese(x)))
  | top_anchovy_with_cheese(Sausage(x))
    = Sausage(top_anchovy_with_cheese(x));

top_anchovy_with_cheese(Onion(Cheese(Sausage(Crust))));
top_anchovy_with_cheese(Onion(Anchovy(Cheese(Anchovy(Crust)))));

fun subst_anchovy_by_cheese(Crust)
    = Crust
  | subst_anchovy_by_cheese(Cheese(x))
    = Cheese(subst_anchovy_by_cheese(x))
  | subst_anchovy_by_cheese(Onion(x))
    = Onion(subst_anchovy_by_cheese(x))
  | subst_anchovy_by_cheese(Anchovy(x))
    = Cheese(subst_anchovy_by_cheese(x))
  | subst_anchovy_by_cheese(Sausage(x))
    = Sausage(subst_anchovy_by_cheese(x));

subst_anchovy_by_cheese(Onion(Anchovy(Cheese(Anchovy(Crust)))));

