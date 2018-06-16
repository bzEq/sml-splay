(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

functor SplayTreeFn(Key : COMPARABLE) = struct

exception Unreachable

datatype 'a tree =
         Node of {
           value : 'a,
           child : 'a tree * 'a tree
         }
       | NIL

type Tree = Key.t tree

datatype Side = LEFT | RIGHT

fun CreateLeaf v = Node{
      value = v,
      child = (NIL, NIL)
    }

fun OtherSide LEFT = RIGHT
  | OtherSide RIGHT = LEFT

fun GetChild NIL _ = NIL
  | GetChild (node as Node{child=(a,_),...}) LEFT = a
  | GetChild (node as Node{child=(_,b),...}) RIGHT = b

fun SetChild NIL _ _ = NIL
  | SetChild (node as Node{value=value,child=(_,b)}) LEFT a =
    Node{
      value = value,
      child = (a, b)
    }
  | SetChild (node as Node{value=value,child=(a,_)}) RIGHT b =
    Node{
      value = value,
      child = (a, b)
    }

fun IsZigZig LEFT LEFT = true
  | IsZigZig RIGHT RIGHT = true
  | IsZigZig _ _ = false

fun IsZigZag a b = not (IsZigZig a b)

fun Rotate NIL _ = NIL
  | Rotate (
      node as Node{
        value=value,
        child=(
          a,
          Node{
            value=value',
            child=(a',b')
        })
    }) LEFT = Node{
      value = value',
      child = (
        Node{
          value = value,
          child = (a, a')
        },
        b'
      )
    }
  | Rotate (node as Node{
               value=value,
               child=(
                 Node{
                   value=value',
                   child=(a',b')
                 },
                 b
               )
           }) RIGHT = Node{
      value = value',
      child = (
        a',
        Node{
          value = value,
          child = (b', b)
        }
      )
    }
  | Rotate (node as Node{...}) _ = node

fun GetSideViaOrder LESS = LEFT
  | GetSideViaOrder _ = RIGHT

fun Splay (root : Tree) (v : Key.t) = let
  fun Splay' NIL = NIL
    | Splay' (node as Node{value,...}) =
      (case (Key.Compare v value) of
           EQUAL => node
         | ord => Zig node (GetSideViaOrder ord)
      )
  and Zig (node as Node{value,...}) (side : Side) =
      (case (GetChild node side) of
           NIL => node
         | (son as Node{value=value',...}) =>
           (case (Key.Compare v value') of
                EQUAL => Rotate node (OtherSide side)
              | ord =>
                (case (IsZigZig (GetSideViaOrder ord) side) of
                     true => (* zigzig *)
                     (case (GetChild son side) of
                          NIL => Rotate node (OtherSide side)
                        | n => let
                          val a' = Splay' n
                          val son' = SetChild son side a'
                          val node' = SetChild node side son'
                        in
                          Rotate (Rotate node' (OtherSide side)) (OtherSide side)
                        end
                     )
                   | false => (* zigzag *)
                     (case (GetChild son (OtherSide side)) of
                          NIL => Rotate node (OtherSide side)
                        | n => let
                          val b' = Splay' n
                          val son' = Rotate (SetChild son (OtherSide side) b') side
                          val node' = SetChild node side son'
                        in
                          Rotate node' (OtherSide side)
                        end)
                )
           )
      )
    | Zig NIL _ = raise Unreachable
in
  Splay' root
end

fun Insert' NIL v = CreateLeaf v
  | Insert' (node as Node{value,child}) v =
    (case (Key.Compare v value) of
         (* NOTE: Update value due to implementation of map on splay *)
         EQUAL => Node {
                   value = v,
                   child = child
                 }
       | ord => let
         val side = GetSideViaOrder ord
       in
         SetChild node side (Insert' (GetChild node side) v)
       end
    )

fun Insert root v = Splay (Insert' root v)

fun Contains NIL _ = false
  | Contains (root as Node{...}) v =
    case (Splay root v) of
        NIL => raise Unreachable
      | (Node{value,...}) => (Key.Compare value v) = EQUAL

fun GetHeight NIL = 0
  | GetHeight (Node{child=(a,b),...}) =
    1 + (Int.max ((GetHeight a), (GetHeight b)))

fun Equals NIL NIL = true
  | Equals NIL (Node{...}) = false
  | Equals (Node{...}) NIL = false
  | Equals (a as Node{value=v,...}) (b as Node{value=v',...}) =
    (Key.Compare v v')= EQUAL andalso
    (Equals (GetChild a LEFT) (GetChild b LEFT)) andalso
    (Equals (GetChild a RIGHT) (GetChild b RIGHT))

fun RotateAllToLeft NIL = NIL
  | RotateAllToLeft (node as Node{child=(_,NIL),...}) = node
  | RotateAllToLeft (node as Node{child=(_,_),...}) =
    RotateAllToLeft (Rotate node LEFT)

fun Remove NIL _ = NIL
  | Remove (root as Node{...}) (v : Key.t) =
    (case (Splay root v) of
         NIL => raise Unreachable
      | (root' as Node{value,child=(a,b)}) =>
        (case (Key.Compare v value) of
             EQUAL =>
             (case a of
                  NIL => b
                | Node{...} => SetChild (RotateAllToLeft a) RIGHT b
             )
           | _ => root'
        )
    )
end
