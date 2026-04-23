#include "prelude/HATS/prelude_dats.hats"
#include "prelude/HATS/prelude_JS_dats.hats"
fun quicksort (xs: list0(nint)): list0(nint) =
  case+ xs of
  | list0_nil() => list0_nil()
  | list0_cons(pivot, rest) => 
      let
        val lesser = list0_filter<nint>(rest, lam(x) => x <= pivot)
        val greater = list0_filter<nint>(rest, lam(x) => x > pivot)
      in
        list0_append<nint>(quicksort(lesser), list0_cons(pivot, quicksort(greater)))
      end


fun insert_helper (x: nint, ys: list0(nint)): list0(nint) =
  case+ ys of
  | list0_nil() => list0_cons(x, list0_nil())
  | list0_cons(y, ys_tail) =>
      if x <= y then 
        list0_cons(x, ys)
      else 
        list0_cons(y, insert_helper(x, ys_tail))

fun insertsort (xs: list0(nint)): list0(nint) =
  case+ xs of
  | list0_nil() => list0_nil()
  | list0_cons(x, xs_tail) => insert_helper(x, insertsort(xs_tail))


fun bubble_pass (xs: list0(nint)): list0(nint) =
  case+ xs of
  | list0_nil() => list0_nil()
  | list0_cons(x, tail1) =>
      (case+ tail1 of
       | list0_nil() => list0_cons(x, list0_nil())
       | list0_cons(y, tail2) =>
           if x > y then 
             list0_cons(y, bubble_pass(list0_cons(x, tail2)))
           else 
             list0_cons(x, bubble_pass(tail1)))

fun bubblesort_loop (xs: list0(nint), n: int): list0(nint) =
  if n <= 1 then 
    xs
  else 
    bubblesort_loop(bubble_pass(xs), n - 1)

fun bubblesort (xs: list0(nint)): list0(nint) =
  bubblesort_loop(xs, list0_length<nint>(xs))