(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

signature COMPARABLE = sig

type t

val Compare : t -> t -> order

val toString : t -> string

end
