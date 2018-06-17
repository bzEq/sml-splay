(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

open SMLUnit

val total = 1048576

structure IntSet = SplaySetFn(
  struct
  type ord_key = int
  val compare = Int.compare
  end
)

val _ = let
  val i = ref 0
  val n = ref 0
  val set = ref IntSet.empty
  val rnd = Random.rand (0, total)
in
  while !i < total do (
    n := Random.randInt rnd;
    set := IntSet.add (!set, !n);
    EXPECT_TRUE (Option.isSome (IntSet.find (fn x => x = (!n)) (!set)));
    i := (!i) + 1
  )
end
