(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure IntSplayTest = struct
open IntSplay
open SMLUnit

val n = Node

fun BuildTestName name = "IntSplayTest:" ^ name

val _ = AddTest
            (BuildTestName "SelfEqual")
            (fn () => let
               val a =
                   n{
                     value = 0,
                     child = (
                       n{
                         value = 1,
                         child = (NIL, NIL)
                       },
                       n{
                         value=2,
                         child = (NIL, NIL)
                       }
                     )
                   }
               val b = a
             in
               AssertTrue (Equals a b) "failed"
             end
            )

val _ = AddTest
            (BuildTestName "Equals")
            (fn () => let
               val a = n{
                     value = 0,
                     child = (
                       n{
                         value = 1,
                         child = (NIL, NIL)
                       },
                       n{
                         value = 2,
                         child = (NIL, NIL)
                       }
                     )
                   }
               val b = n{
                     value = 0,
                     child = (
                       n{
                         value = 1,
                         child = (NIL, NIL)
                       },
                       n{
                         value = 2,
                         child = (NIL, NIL)
                       }
                     )
                   }
             in
               AssertTrue (Equals a b) "failed"
             end
            )

val _ = AddTest
            (BuildTestName "NotEqual")
            (fn () => let
               val a = n{
                     value = 0,
                     child = (
                       n{
                         value = 1,
                         child = (NIL, NIL)
                       },
                       NIL
                     )
                   }
               val b = NIL
             in
               AssertTrue (not (Equals a b)) "failed"
             end
            )

val _ = AddTest
            (BuildTestName "NotEqual1")
            (fn () => let
               val a = n{
                     value = 0,
                     child = (
                       n{
                         value = 1,
                         child = (NIL, NIL)
                       },
                       n{
                         value = 2,
                         child = (NIL, NIL)
                       }
                     )
                   }
               val b = n{
                     value = 0,
                     child = (
                       n{
                         value = 1,
                         child = (NIL, NIL)
                       },
                       NIL
                     )
                   }
             in
               AssertTrue (not (Equals a b)) "failed"
             end
            )

end
