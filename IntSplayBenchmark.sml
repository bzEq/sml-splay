(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

open IntSplay
open SMLUnit

val _ = let
  val total = 1048576
  val i = ref 0
  val root = ref NIL
  val n = ref 0
  val rng = Random.rand (0, 1048576)
in
  while !i < total do (
    n := Random.randInt rng;
    root := Insert (!root) (!n);
    EXPECT_TRUE (Contains (!root) (!n));
    i := (!i) + 1
  )
end
