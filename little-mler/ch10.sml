(* chapter 10 *)

(* Tenth Moral: Real programs consist of many components. Specify the
                dependencies among these components using signatures
                and functors. *)

(* Control.Print.printDepth := 20; *)

(* arith over ints *)

fun bad_is_zero n = 0 = n
fun bad_succ n = n + 1
exception Bad_Too_small
fun bad_pred n = if bad_is_zero n
                 then raise Bad_Too_small
                 else n - 1

fun bad_plus(n, m) = if bad_is_zero(n)
                     then m
                     else bad_succ(bad_plus(bad_pred(n), m))

(* arith over peano numbers *)

datatype num = Zero | One_more_than of num

fun is_zero(Zero) = true
  | is_zero(_)    = false

fun succ n = One_more_than n
fun pred Zero = raise Bad_Too_small
  | pred(One_more_than(n)) = n

fun peano_to_int Zero = 0
  | peano_to_int(One_more_than(n)) = 1 + peano_to_int(n)

val three = succ(succ(succ(Zero)))

val test_01 = 3 = peano_to_int(three)

fun plus(n, m) = if is_zero n
                 then m
                 else succ(plus(pred(n), m))

val test_02 = 6 = peano_to_int(plus(three, three))

signature PeanoNumbers =
sig
    type number
    exception TooSmall
    val is_zero : number -> bool
    val succ : number -> number
    val pred : number -> number
end

functor NumberAsNum () : PeanoNumbers =
struct
    datatype num = Zero | One_more_than of num
    type number = num
    exception TooSmall
    fun is_zero(Zero) = true
      | is_zero(_)    = false
    fun succ n = One_more_than n
    fun pred Zero = raise TooSmall
      | pred(One_more_than(n)) = n
end

functor NumberAsInt () : PeanoNumbers =
struct
    type number = int
    exception TooSmall
    fun is_zero n = n=0
    fun succ n = n + 1
    fun pred n = if is_zero n
                 then raise TooSmall
                 else n - 1
end

structure IntStruct = NumberAsInt()

structure NumStruct = NumberAsNum()

signature PlusOverNumber =
sig
    type number
    val plus : (number * number) -> number
end

functor PlusOverNumber(structure a_N : PeanoNumbers) : PlusOverNumber =
struct
type number = a_N.number
fun plus(n, m) = if a_N.is_zero(n)
                 then m
                 else a_N.succ(plus(a_N.pred(n), m))
end

structure IntArith = PlusOverNumber(structure a_N = IntStruct)
structure NumArith = PlusOverNumber(structure a_N = NumStruct)

(*
 * IDGI: the book is saying this is nonsense because we don't know the
 * args externally, but the code totally evaluates:
 *
 * val n = 5 = IntArith.plus(2, 3)
 *)

signature NumbersWithConcealReveal =
sig
    type number
    exception TooSmall
    val conceal : int -> number
    val is_zero : number -> bool
    val succ : number -> number
    val pred : number -> number
    val reveal : number -> int
end

functor NumberAsInt () : NumbersWithConcealReveal =
struct
    type number = int
    exception TooSmall
    fun conceal n = n
    fun is_zero n = n=0
    fun succ n = n + 1
    fun pred n = if is_zero n
                 then raise TooSmall
                 else n - 1
    fun reveal n = n
end

functor NumberAsNum() : NumbersWithConcealReveal =
struct
    datatype num = Zero | One_more_than of num
    type number = num
    exception TooSmall
    fun conceal n = if n=0
                    then Zero
                    else One_more_than(conceal(n - 1))
    fun is_zero(Zero) = true
      | is_zero(_)    = false
    fun succ n = One_more_than n
    fun pred Zero = raise TooSmall
      | pred(One_more_than(n)) = n
    fun reveal Zero = 0
      | reveal(One_more_than(n)) = 1 + reveal(n)
end

structure IntStruct = NumberAsInt()
structure IntArith = PlusOverNumber(structure a_N = IntStruct)
structure NumStruct = NumberAsNum()
structure NumArith = PlusOverNumber(structure a_N = NumStruct)

val test_03 = 1 = NumStruct.reveal(NumStruct.succ(NumStruct.conceal(0)))
val test_04 = 1 = IntStruct.reveal(IntStruct.succ(IntStruct.conceal(0)))

(*
 * The book is saying this is still nonsense even tho it evaluates just fine:
 *
 * val n = IntStruct.reveal(IntArith.plus(IntStruct.conceal(2), IntStruct.conceal(3)))
 *)

functor PlusOverNumber(structure a_N : PeanoNumbers)
        :> PlusOverNumber where type number = a_N.number =
struct
    type number = a_N.number
    fun plus(n, m) = if a_N.is_zero(n)
                     then m
                     else a_N.succ(plus(a_N.pred(n), m))
end

structure IntArith = PlusOverNumber(structure a_N = IntStruct)
structure NumArith = PlusOverNumber(structure a_N = NumStruct)

val test_05 = 5 = IntStruct.reveal(IntArith.plus(IntStruct.conceal(2), IntStruct.conceal(3)))

functor NumberAsInt2 () : PeanoNumbers where type number = int =
struct
    type number = int
    exception TooSmall
    fun conceal n = n
    fun is_zero n = n=0
    fun succ n = n + 1
    fun pred n = if is_zero n
                 then raise TooSmall
                 else n - 1
    fun reveal n = n
end

structure IntStruct2 = NumberAsInt2()
structure IntArith2 = PlusOverNumber(structure a_N = IntStruct2)

val test_06 = 5 = IntArith2.plus(2, 3)

(* nonsense:
 *
 * functor NumberAsNum2() : PeanoNumbers where type number = num =
 * struct
 *     datatype num = Zero | One_more_than of num
 *     type number = num
 *     exception TooSmall
 *     fun is_zero(Zero) = true
 *       | is_zero(_)    = false
 *     fun succ n = One_more_than n
 *     fun pred Zero = raise TooSmall
 *       | pred(One_more_than(n)) = n
 * end
 *)

(* holy fuck I don't care anymore *)

signature J =
sig
    val new_plus : int * int -> int
end

functor NewPlus(structure a_N : NumbersWithConcealReveal
                structure a_P : PlusOverNumber
                sharing type a_N.number = a_P.number) : J =
struct
    fun new_plus(x, y) = a_N.reveal(a_P.plus(a_N.conceal(x),
                                             a_N.conceal(y)))
end

structure NewPlusStruct = NewPlus(structure a_N = NumStruct
                                  structure a_P = PlusOverNumber(structure a_N = a_N))

signature TimesOverNumber =
sig
    type number
    val times : number * number -> number
end

functor TimesOverNumber(structure a_N : PeanoNumbers
                        structure a_P : PlusOverNumber
                        sharing type a_N.number = a_P.number)
        : TimesOverNumber where type number = a_N.number =
struct
    type number = a_N.number
    fun times(n, m) = if a_N.is_zero(m)
                      then m
                      else a_P.plus(n, times(n, a_N.pred(m)))
end

structure Times = TimesOverNumber(structure a_N = NumStruct
                                  structure a_P = PlusOverNumber(structure a_N = a_N))

val test_07 = 5 = NewPlusStruct.new_plus(2, 3)

(* still broke... don't care at this point *)
(* val test_08 = Times.times(2, 3) *)
