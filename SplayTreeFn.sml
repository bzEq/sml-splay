(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

functor SplayTreeFn(Key : COMPARABLE) = struct

open SMLUnit

exception Unreachable

datatype 'a tree =
         Node of {
           value : 'a,
           child : 'a tree * 'a tree
         }
       | NIL

type Tree = Key.t tree

datatype Side = LEFT | RIGHT

fun Count NIL = 0
  | Count (Node{child=(a,b),...}) =
    1 + (Count a) + (Count b)

fun CreateLeaf v = Node{
      value = v,
      child = (NIL, NIL)
    }

fun OtherSide LEFT = RIGHT
  | OtherSide RIGHT = LEFT

fun GetChild NIL _ = NIL
  | GetChild (node as Node{child=(a,_),...}) LEFT : Tree = a
  | GetChild (node as Node{child=(_,b),...}) RIGHT : Tree = b

fun Equals NIL NIL = true
  | Equals NIL (Node{...}) = false
  | Equals (Node{...}) NIL = false
  | Equals (a as Node{value=v,child=(a',a'')}) (b as Node{value=v',child=(b',b'')}) =
    (Key.Compare v v')= EQUAL andalso
    (Equals a' b') andalso
    (Equals a'' b'')

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

fun GetHeight NIL = 0
  | GetHeight (Node{child=(a,b),...}) =
    1 + (Int.max ((GetHeight a), (GetHeight b)))

fun IsZigZig LEFT LEFT = true
  | IsZigZig RIGHT RIGHT = true
  | IsZigZig _ _ = false

fun IsZigZag a b = not (IsZigZig a b)

fun CreateChild LEFT a b = (a, b)
  | CreateChild RIGHT a b  = (b, a)

fun ZigZig (root as Node{value,...} : Tree)
           (son as Node{value=value',...} : Tree)
           (grandson as Node{value=value'',...} : Tree)
           (side : Side) = Node{
      value = value'',
      child =
      (CreateChild
         side
         (GetChild grandson side)
         (Node{
             value = value',
             child = (
               CreateChild side
                           (GetChild grandson (OtherSide side))
                           (Node{
                               value = value,
                               child = (
                                 CreateChild side
                                             (GetChild son (OtherSide side))
                                             (GetChild root (OtherSide side))
                               )
                           })
             )
         })
      )
    }
  | ZigZig _ _ _ _ = raise Unreachable

fun ZigZag (root as Node{value,...} : Tree)
           (son as Node{value=value',...} : Tree)
           (grandson as Node{value=value'',...} : Tree)
           (side : Side) = Node{
      value = value'',
      child =
      (CreateChild side
                   (Node{
                       value = value',
                       child =
                       (CreateChild side
                                    (GetChild son side)
                                    (GetChild grandson side)
                       )
                   })
                   (Node{
                       value = value,
                       child =
                       (CreateChild side
                                    (GetChild grandson (OtherSide side))
                                    (GetChild root (OtherSide side))
                       )
                   })

      )
    }
  | ZigZag _ _ _ _ = raise Unreachable

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
         | ord => let
           val side = GetSideViaOrder ord
         in
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
                             | n =>
                               ZigZig node son (Splay' n) side
                          )
                        | false => (* zigzag *)
                          (case (GetChild son (OtherSide side)) of
                               NIL => Rotate node (OtherSide side)
                             | n =>
                               ZigZag node son (Splay' n) side
                          )
                     )
                )
           )
         end
      )
in
  Splay' root
end

fun Insert' NIL v _ = CreateLeaf v
  | Insert' (node as Node{value,child}) v {upsert=upsert} =
    (case (Key.Compare v value) of
         EQUAL => if upsert then
                    Node {
                      value = v,
                      child = child
                    }
                  else
                    node
       | ord => let
         val side = GetSideViaOrder ord
         val a = Insert' (GetChild node side) v {upsert=upsert}
       in
         SetChild node side a
       end
    )

fun Insert root v : Tree = Splay (Insert' (Splay root v) v {upsert=false}) v

fun Upsert root v : Tree = Splay (Insert' (Splay root v) v {upsert=true}) v

fun Contains NIL _ = false
  | Contains (root as Node{...}) v =
    case (Splay root v) of
        NIL => raise Unreachable
      | (Node{value,...}) => (Key.Compare value v) = EQUAL

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
