(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure SMLUnit = struct

exception Fail of string
exception Unexpected of string

fun println s = print (s ^ "\n")

fun ASSERT_TRUE true _ = ()
  | ASSERT_TRUE false message = raise (Fail message)

fun EXPECT_TRUE true _ = ()
  | EXPECT_TRUE false message = raise (Unexpected message)

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
      val ok = ref true
    in
      println ("Running " ^ (#desc t));
      (#exec t) ()
      handle (exc as (Fail message)) =>
             (
               println ((#desc t) ^ ": " ^ message);
               raise exc
             )
           | (exc as (Unexpected message)) =>
             (
               ok := false
             );
      if !ok = true then
        println ((#desc t) ^ " -> Ok")
      else
        println ((#desc t) ^ " -> Fail");
      i := (!i) + 1
    end
  )
end

end
