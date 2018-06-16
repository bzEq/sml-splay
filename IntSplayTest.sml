(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure IntSplayTest = struct
open IntSplay
open SMLUnit

val n = Node (* For convenience *)

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
             EXPECT_TRUE (Equals a b) "failed"
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
             EXPECT_TRUE (Equals a b) "failed"
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
             EXPECT_TRUE (not (Equals a b)) "failed"
           end
          )

val _ = AddTest
          (BuildTestName "NotEqual-1")
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
             EXPECT_TRUE (not (Equals a b)) "failed"
           end
          )

val _ = AddTest
          (BuildTestName "Left-Rotate")
          (fn () => let
             val a = n{
                   value = 0,
                   child = (
                     (CreateLeaf 1),
                     n{
                       value = 2,
                       child = (
                         (CreateLeaf 3),
                         (CreateLeaf 4)
                       )
                     }
                   )
                 }
             val b = Rotate a LEFT
             val expected = n{
                   value = 2,
                   child = (
                     n{
                       value = 0,
                       child = (
                         (CreateLeaf 1),
                         (CreateLeaf 3)
                       )
                     },
                     (CreateLeaf 4)
                   )
                 }
           in
             EXPECT_TRUE (Equals b expected) "failed"
           end
          )

val _ = AddTest
          (BuildTestName "Right-Rotate")
          (fn () => let
             val a = n{
                   value = 0,
                   child = (
                     n{
                       value = 1,
                       child = (
                         (CreateLeaf 2),
                         (CreateLeaf 3)
                       )
                     },
                     (CreateLeaf 4)
                   )
                 }
             val b = Rotate a RIGHT
             val expected = n{
                   value = 1,
                   child = (
                     (CreateLeaf 2),
                     n{
                       value = 0,
                       child = (
                         (CreateLeaf 3),
                         (CreateLeaf 4)
                       )
                     }
                   )
                 }
           in
             EXPECT_TRUE (Equals b expected) "failed"
           end
          )

val _ = AddTest
          (BuildTestName "Rotate-All-Left")
          (fn () => let
             val a = n{
                   value = 0,
                   child = (
                     NIL,
                     n{
                       value = 1,
                       child = (
                         NIL,
                         (CreateLeaf 2)
                       )
                     }
                   )
                 }
             val b = RotateAllToLeft a
             val expected = n{
                   value = 2,
                   child = (
                     n{
                       value = 1,
                       child = (
                         (CreateLeaf 0),
                         NIL
                       )
                     },
                     NIL
                   )
                 }
           in
             EXPECT_TRUE (Equals b expected) "failed"
           end
          )

val _ = AddTest
          (BuildTestName "Rotate-All-Left-1")
          (fn () => let
             val a = n{
                   value = 0,
                   child = (
                     NIL,
                     n{
                       value = 1,
                       child = (
                         (CreateLeaf 2),
                         (CreateLeaf 3)
                       )
                     }
                   )
                 }
             val b = RotateAllToLeft a
             val expected = n{
                   value = 3,
                   child = (
                     n{
                       value = 1,
                       child = (
                         n{
                           value = 0,
                           child = (
                             NIL,
                             (CreateLeaf 2)
                           )
                         },
                         NIL
                       )
                     },
                     NIL
                   )
                 }
           in
             EXPECT_TRUE (Equals b expected) "failed"
           end
          )

val _ = AddTest
          (BuildTestName "Count")
          (fn () => let
             val a = n{
                   value = 0,
                   child = (
                     NIL,
                     n{
                       value = 1,
                       child = (
                         (CreateLeaf 2),
                         (CreateLeaf 3)
                       )
                     }
                   )
                 }
           in
             EXPECT_TRUE ((Count a) = 4) "failed"
           end
          )

val _ = AddTest
          (BuildTestName "InsertAndCount")
          (fn () => let
             val total = 1048576
             val i = ref 0
             val root = ref NIL
           in
             while !i < total do (
               root := Insert (!root) (!i);
               i := (!i) + 1
             );
             EXPECT_TRUE ((Count (!root)) = total) "failed"
           end
          )

val _ = AddTest
          (BuildTestName "UpsertAndCount")
          (fn () => let
             val total = 1048576
             val i = ref 0
             val root = ref NIL
           in
             while !i < total do (
               root := Upsert (!root) (!i);
               i := (!i) + 1
             );
             EXPECT_TRUE ((Count (!root)) = total) "failed"
           end
          )

val _ = AddTest
          (BuildTestName "InsertAndCheckExistence")
          (fn () => let
             val total = 1048576
             val i = ref 0
             val root = ref NIL
           in
             while !i < total do (
               root := Insert (!root) (!i);
               i := (!i) + 1
             );
             i := 0;
             while !i < total do (
               EXPECT_TRUE (Contains (!root) (!i))
                           ((Int.toString (!i)) ^ " is not in the tree");
               root := Splay (!root) (!i);
               i := (!i) + 1
             )
           end
          )

end
