(*
 * utils.ml
 * -----------
 * Copyright : (c) 2023 - 2025, smaji.org
 * Copyright : (c) 2023 - 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_gsd.
 *)

let string_of_float f=
  let str= string_of_float f in
  if String.ends_with ~suffix:"." str then str ^ "0" else str

