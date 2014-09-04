(* chapter 1 *)

(* this is a comment *)

val x = 34;

val y = 17;

val z = (x + y) + (y + 2);

val q = z + 1;

val abs_of_z = if z < 0 then 0 - z else z;

datatype seasoning = Salt | Pepper;

datatype num = Zero | One_more_than of num;

Zero;

One_more_than(Zero);

One_more_than(One_more_than(Zero));

One_more_than Zero;

(* One_more_than One_more_than Zero; *)

datatype 'a open_faced_sandwich = Bread of 'a
                                | Slice of 'a open_faced_sandwich;

Bread(0);

Bread(One_more_than(Zero));

Bread(Bread(0));

Bread(Bread(One_more_than(Zero)));
