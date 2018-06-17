(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

open IntSplay
open SMLUnit

val total = 1048576

val _ = let
  val i = ref 0
  val n = ref 0
  val root = ref NIL
  val rnd = Random.rand (0, total)
in
  while !i < total do (
    n := Random.randInt rnd;
    root := Insert (!root) (!n);
    i := (!i) + 1
  )
end
