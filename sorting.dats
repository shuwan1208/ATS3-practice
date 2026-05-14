(* ****** ****** *)
(*
HX-2026-05-14:
Merge Sort on FnList
*)

#extern
fun
list_mergesort
(xs: list(sint)): list(sint)

#implfun
list_mergesort(xs) =
(
case+ xs of
| list_nil() => list_nil()
| list_cons(x, list_nil()) => list_cons(x, list_nil())
| list_cons(_, _) =>
  let
    val (xs1, xs2) = list_split(xs)
  in
    list_merge(list_mergesort(xs1), list_mergesort(xs2))
  end
) where
{
  fun list_split(xs: list(sint)): (list(sint), list(sint)) =
  (
    case+ xs of
    | list_nil() => (list_nil(), list_nil())
    | list_cons(x, list_nil()) => (list_cons(x, list_nil()), list_nil())
    | list_cons(x, list_cons(y, xs)) =>
      let
        val (xs1, xs2) = list_split(xs)
      in
        (list_cons(x, xs1), list_cons(y, xs2))
      end
  )

  fun list_merge(xs: list(sint), ys: list(sint)): list(sint) =
  (
    case+ (xs, ys) of
    | (list_nil(), _) => ys
    | (_, list_nil()) => xs
    | (list_cons(x, xs1), list_cons(y, ys1)) =>
      if x <= y then list_cons(x, list_merge(xs1, ys))
      else list_cons(y, list_merge(xs, ys1))
  )
}

(* ****** ****** *)
(*
HX-2026-05-14:
Selection Sort put smallest on the front
*)

#extern
fun
list_selectionsort
(xs: list(sint)): list(sint)

#implfun
list_selectionsort(xs) =
(
case+ xs of
| list_nil() => list_nil()
| list_cons(_, _) =>
  let
    val (x, xs) = list_remove_min(xs)
  in
    list_cons(x, list_selectionsort(xs))
  end
) where
{
  fun list_remove_min(xs: list(sint)): (sint, list(sint)) =
  (
    case+ xs of
    | list_nil() => (~1, list_nil()) // should not happen
    | list_cons(x, list_nil()) => (x, list_nil())
    | list_cons(x, xs) =>
      let
        val (y, xs) = list_remove_min(xs)
      in
        if x <= y then (x, xs) else (y, list_cons(x, xs))
      end
  )
}

val () = printsln("\
mergesort(1,2,3,4,5,1,2,3,4,5) = ",
list_mergesort(list@(1,2,3,4,5)\append(list@(1,2,3,4,5))))

val () = printsln("\
selectionsort(1,2,3,4,5,1,2,3,4,5) = ",
list_selectionsort(list@(1,2,3,4,5)\append(list@(1,2,3,4,5))))

(* ****** ****** *)

(* ****** ****** *)
(*
HX-2026-05-14:
Merge Sort on FnList
*)

#extern
fun
list_mergesort
(xs: list(sint)): list(sint)

#implfun
list_mergesort(xs) =
(
case+ xs of
| list_nil() => list_nil()
| list_cons(x, list_nil()) => list_cons(x, list_nil())
| list_cons(_, _) =>
  let
    val (xs1, xs2) = list_split(xs)
  in
    list_merge(list_mergesort(xs1), list_mergesort(xs2))
  end
) where
{
  fun list_split(xs: list(sint)): (list(sint), list(sint)) =
  (
    case+ xs of
    | list_nil() => (list_nil(), list_nil())
    | list_cons(x, list_nil()) => (list_cons(x, list_nil()), list_nil())
    | list_cons(x, list_cons(y, xs)) =>
      let
        val (xs1, xs2) = list_split(xs)
      in
        (list_cons(x, xs1), list_cons(y, xs2))
      end
  )

  fun list_merge(xs: list(sint), ys: list(sint)): list(sint) =
  (
    case+ (xs, ys) of
    | (list_nil(), _) => ys
    | (_, list_nil()) => xs
    | (list_cons(x, xs1), list_cons(y, ys1)) =>
      if x <= y then list_cons(x, list_merge(xs1, ys))
      else list_cons(y, list_merge(xs, ys1))
  )
}

(* ****** ****** *)
(*
HX-2026-05-14:
Selection Sort put smallest on the front
*)

#extern
fun
list_selectionsort
(xs: list(sint)): list(sint)

#implfun
list_selectionsort(xs) =
(
case+ xs of
| list_nil() => list_nil()
| list_cons(_, _) =>
  let
    val (x, xs) = list_remove_min(xs)
  in
    list_cons(x, list_selectionsort(xs))
  end
) where
{
  fun list_remove_min(xs: list(sint)): (sint, list(sint)) =
  (
    case+ xs of
    | list_nil() => (~1, list_nil()) // should not happen
    | list_cons(x, list_nil()) => (x, list_nil())
    | list_cons(x, xs) =>
      let
        val (y, xs) = list_remove_min(xs)
      in
        if x <= y then (x, xs) else (y, list_cons(x, xs))
      end
  )
}

val () = printsln("\
mergesort(1,2,3,4,5,1,2,3,4,5) = ",
list_mergesort(list@(1,2,3,4,5)\append(list@(1,2,3,4,5))))

val () = printsln("\
selectionsort(1,2,3,4,5,1,2,3,4,5) = ",
list_selectionsort(list@(1,2,3,4,5)\append(list@(1,2,3,4,5))))

(* ****** ****** *)