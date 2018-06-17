(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure IntSplayBenchmark = struct

open IntSplay
open SMLUnit

val total = 1048576

fun run () = let
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

val _ = run ()

end
