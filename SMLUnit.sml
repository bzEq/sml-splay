(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure SMLUnit = struct

exception Fail of string

fun println s = print (s ^ "\n")

fun AssertTrue true _ = ()
  | AssertTrue false message = raise (Fail message)

type Test = {
  exec : unit -> unit,
  desc : string
}

val tests : Test list ref = ref []

fun AddTest desc exec = (
    tests := {
      exec = exec,
      desc = desc
    }::(!tests);
    (!tests)
)

fun RunAll () = let
  val i = ref 0
  val all = List.rev (!tests)
  val l = List.length all
in
  while !i < l do (
    let
      val t = List.nth (all , !i)
    in
      (#exec t) ()
      handle (exc as (Fail message)) => (
             println ((#desc t) ^ ": " ^ message);
             raise exc
      );
      println ((#desc t) ^ " -> Ok");
      i := (!i) + 1
    end
  )
end

end
