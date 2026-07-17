(*
 * typeface.ml
 * -----------
 * Copyright : (c) 2023 - 2026, smaji.org
 * Copyright : (c) 2023 - 2026, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_gsd.
 *)

type path= string

type t=
  | Black
  | Ming
  | Path of path

let of_string= function
  | "black" | "Black" -> Black
  | "ming" | "Ming" -> Ming
  | path -> Path path
