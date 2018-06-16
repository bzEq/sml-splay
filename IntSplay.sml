(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure IntSplay = SplayTreeFn(
  struct
  type t = int
  fun Compare a b = Int.compare (a, b)
  val toString = Int.toString
  end
)
