(*
 * stroke_def.ml
 * -----------
 * Copyright : (c) 2023 - 2025, smaji.org
 * Copyright : (c) 2023 - 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_gsd.
 *)


module Tag = struct
  type t =
    | S_h     (* Horizontal *)
    | S_sh    (* Slanted Horizontal *)
    | S_u     (* Upward horizontal *)
    | S_du    (* Dot – Upward horizontal *)
    | S_v     (* Vertical *)
    | S_sv    (* Slanted Vertical *)
    | S_rsv   (* Right Slanted Vertical *)
    | S_t     (* Throw *)
    | S_ft    (* Flat Throw *)
    | S_wt    (* Wilted Throw *)
    | S_d     (* Dot *)
    | S_ed    (* Extended Dot *)
    | S_ld    (* Left Dot *)
    | S_wd    (* Wilted Dot *)
    | S_p     (* Press *)
    | S_up    (* Upward horizontal – Press *)
    | S_hp    (* Horizontal – Press *)
    | S_fp    (* Flat Press *)
    | S_ufp   (* Upward horizontal – Flat Press *)
    | S_c     (* Clockwise curve *)
    | S_a     (* Anticlockwise curve *)
    | S_o     (* Oval *)
    | S_hj    (* Horizontal – J hook *)
    | S_uj    (* Upward horizontal – J hook *)
    | S_ht    (* Horizontal – Throw *)
    | S_hsv   (* Horizontal – Slanted Vertical *)
    | S_hv    (* Horizontal – Vertical *)
    | S_hvj   (* Horizontal – Vertical – J hook *)
    | S_htj   (* Horizontal – Throw – J hook *)
    | S_utj   (* Upward horizontal – Throw – J hook *)
    | S_hvh   (* Horizontal – Vertical – Horizontal *)
    | S_hvu   (* Horizontal – Vertical – Upward horizontal *)
    | S_ha    (* Horizontal – Anticlockwise curve *)
    | S_haj   (* Horizontal – Anticlockwise curve – J hook *)
    | S_hpj   (* Horizontal – Press – J hook *)
    | S_htaj  (* Horizontal – Throw – Anticlockwise curve – J hook *)
    | S_htc   (* Horizontal – Throw – Clockwise curve *)
    | S_htht  (* Horizontal – Throw – Horizontal – Throw *)
    | S_htcj  (* Horizontal – Throw – Clockwise curve – J hook *)
    | S_hvhv  (* Horizontal – Vertical – Horizontal – Vertical *)
    | S_hthtj (* Horizontal – Throw – Horizontal – Throw – J hook *)
    | S_vu    (* Vertical – Upward horizontal *)
    | S_vh    (* Vertical – Horizontal *)
    | S_va    (* Vertical – Anticlockwise curve *)
    | S_vaj   (* Vertical – Anticlockwise curve – J hook *)
    | S_vhv   (* Vertical – Horizontal – Vertical *)
    | S_vht   (* Vertical – Horizontal – Throw *)
    | S_vhtj  (* Vertical – Horizontal – Throw – J hook *)
    | S_vj    (* Vertical – J hook *)
    | S_vc    (* Vertical – Clockwise curve *)
    | S_vcj   (* Vertical – Clockwise curve – J hook *)
    | S_tu    (* Throw – Upward horizontal *)
    | S_th    (* Throw – Horizontal *)
    | S_td    (* Throw – Dot *)
    | S_wtd   (* Wilted Throw – Dot *)
    | S_tht   (* Throw – Horizontal – Throw *)
    | S_thtj  (* Throw – Horizontal – Throw – J hook *)
    | S_tj    (* Throw – J hook *)
    | S_cj    (* Clockwise curve – J hook *)
    | S_fpj   (* Flat Press – J hook *)
    | S_pj    (* Press – J hook *)
    | S_thtaj (* Throw – Horizontal – Throw – Anticlockwise curve – J hook *)
    | S_tod   (* Throw – Oval – Dot *)

  let t_of_string= function
    | "a"-> S_a
    | "cj"-> S_cj
    | "c"-> S_c
    | "d"-> S_d
    | "du"-> S_du
    | "ed"-> S_ed
    | "fpj"-> S_fpj
    | "fp"-> S_fp
    | "ft"-> S_ft
    | "haj"-> S_haj
    | "ha"-> S_ha
    | "hj"-> S_hj
    | "hpj"-> S_hpj
    | "hp"-> S_hp
    | "h"-> S_h
    | "hsv"-> S_hsv
    | "htaj"-> S_htaj
    | "htcj"-> S_htcj
    | "htc"-> S_htc
    | "hthtj"-> S_hthtj
    | "htht"-> S_htht
    | "htj"-> S_htj
    | "ht"-> S_ht
    | "hvh"-> S_hvh
    | "hvhv"-> S_hvhv
    | "hvj"-> S_hvj
    | "hv"-> S_hv
    | "hvu"-> S_hvu
    | "ld"-> S_ld
    | "o"-> S_o
    | "pj"-> S_pj
    | "p"-> S_p
    | "rsv"-> S_rsv
    | "sh"-> S_sh
    | "sv"-> S_sv
    | "td"-> S_td
    | "th"-> S_th
    | "thtaj"-> S_thtaj
    | "thtj"-> S_thtj
    | "tht"-> S_tht
    | "tj"-> S_tj
    | "tod"-> S_tod
    | "t"-> S_t
    | "tu"-> S_tu
    | "ufp"-> S_ufp
    | "uj"-> S_uj
    | "up"-> S_up
    | "u"-> S_u
    | "utj"-> S_utj
    | "vaj"-> S_vaj
    | "va"-> S_va
    | "vcj"-> S_vcj
    | "vc"-> S_vc
    | "vh"-> S_vh
    | "vhtj"-> S_vhtj
    | "vht"-> S_vht
    | "vhv"-> S_vhv
    | "vj"-> S_vj
    | "v"-> S_v
    | "vu"-> S_vu
    | "wd"-> S_wd
    | "wtd"-> S_wtd
    | "wt"-> S_wt
    | _-> failwith "tag_of_string"

  let t_to_string= function
    | S_a->     "a"
    | S_cj->    "cj"
    | S_c->     "c"
    | S_d->     "d"
    | S_du->    "du"
    | S_ed->    "ed"
    | S_fpj->   "fpj"
    | S_fp->    "fp"
    | S_ft->    "ft"
    | S_haj->   "haj"
    | S_ha->    "ha"
    | S_hj->    "hj"
    | S_hpj->   "hpj"
    | S_hp->    "hp"
    | S_h->     "h"
    | S_hsv->   "hsv"
    | S_htaj->  "htaj"
    | S_htcj->  "htcj"
    | S_htc->   "htc"
    | S_hthtj-> "hthtj"
    | S_htht->  "htht"
    | S_htj->   "htj"
    | S_ht->    "ht"
    | S_hvh->   "hvh"
    | S_hvhv->  "hvhv"
    | S_hvj->   "hvj"
    | S_hv->    "hv"
    | S_hvu->   "hvu"
    | S_ld->    "ld"
    | S_o->     "o"
    | S_pj->    "pj"
    | S_p->     "p"
    | S_rsv->   "rsv"
    | S_sh->    "sh"
    | S_sv->    "sv"
    | S_td->    "td"
    | S_th->    "th"
    | S_thtaj-> "thtaj"
    | S_thtj->  "thtj"
    | S_tht->   "tht"
    | S_tj->    "tj"
    | S_tod->   "tod"
    | S_t->     "t"
    | S_tu->    "tu"
    | S_ufp->   "ufp"
    | S_uj->    "uj"
    | S_up->    "up"
    | S_u->     "u"
    | S_utj->   "utj"
    | S_vaj->   "vaj"
    | S_va->    "va"
    | S_vcj->   "vcj"
    | S_vc->    "vc"
    | S_vh->    "vh"
    | S_vhtj->  "vhtj"
    | S_vht->   "vht"
    | S_vhv->   "vhv"
    | S_vj->    "vj"
    | S_v->     "v"
    | S_vu->    "vu"
    | S_wd->    "wd"
    | S_wtd->   "wtd"
    | S_wt->    "wt"

  let compare= compare
end

type tag= Tag.t
let tag_of_string= Tag.t_of_string
let tag_to_string= Tag.t_to_string

module GlyphPath = Smaji_glyph_path
module Point = GlyphPath.Path.Point
type point= Point.t

type 'a adjust=
  | Auto
  | Specify of 'a

let get_adjust ~f= function
  | Auto-> f ()
  | Specify v->  v

let adjust_to_opt= function
  | Auto-> None
  | Specify v->  Some v

type h= {
  h_start: point;
  length: float;
} (* Horizontal *)
  (* 1. the length is at least 4 times its weight *)

type sh= {
  sh_start: point;
  end_: point;
} (* Slanted Horizontal *)
  (* 1. the length is at least 4 times its weight *)
  (* 2. the angle is limited in 10 degrees *)

type u= {
  u_start: point;
  end_: point;
} (* Upward horizontal *)
  (* 1. the length is at least 4 times its weight *)
  (* 2. the angle is limited between 10 to 80 degrees *)

type du= {
  du_start: point;
  dot: point adjust;
  end_: point;
} (* Dot – Upward horizontal *)
  (* 1. the length is at least 4 times its weight *)
  (* 2. the angle is limited between 10 to 45 degrees *)

type v= {
  v_start: point;
  length: float;
} (* Vertical *)
  (* 1. the length is at least 4 times its weight *)

type sv= {
  sv_start: point;
  length: float;
  width: float;
} (* Slanted Vertical *)
  (* 1. the length is at least 4 times its weight *)
  (* 2. the end point is to the bottom left of sv_start *)
  (* 3. the angle is limited between 5 to 30 degrees *)

type rsv= {
  rsv_start: point;
  length: float;
  width: float;
} (* Right Slanted Vertical *)
  (* 1. the length is at least 4 times its weight *)
  (* 2. the end point is to the bottom right of rsv_start *)
  (* 3. the angle is limited between 5 to 30 degrees *)

type t'= {
  t'_start: point;
  ctrl1: point adjust;
  ctrl2: point adjust;
  end_: point;
} (* Throw *)
  (* 1. the length is at least 4 times its weight *)
  (* 2. the angle is limited between 40 to 50 degrees *)
  (* 3. ctrl1 is to the bottom left of t_start *)
  (* 4. ctrl2 is to the top right of end_ *)

type ft= {
  ft_start: point;
  ctrl1: point adjust;
  ctrl2: point adjust;
  end_: point;
} (* Flat Throw *)
  (* 1. the length is at least 4 times its weight *)
  (* 2. the angle is limited between 5 to 40 degrees *)
  (* 3. ctrl1 is to the bottom left of t_start *)
  (* 4. ctrl2 is to the top right of end_ *)

type wt= {
  wt_start: point;
  v_length: float adjust;
  ctrl1: point adjust;
  ctrl2: point adjust;
  end_: point;
} (* Wilted Throw *)
  (* 1. the length is at least 4 times its weight *)
  (* 2. the v_length is at least 2 times its weight *)
  (* 3. the angle is limited between 50 to 85 degrees *)
  (* 4. ctrl1 is to the bottom left of t_start *)
  (* 5. ctrl2 is to the top right of end_ *)

type d= {
  d_start: point;
  end_: point adjust;
} (* Dot *)
  (* 1. the length is at least 1 times its weight *)
  (* 2. the length is at most 2 times its weight *)
  (* 5. end is to the bottom right of d_start *)

type ed= {
  ed_start: point;
  end_: point adjust;
} (* Extended Dot *)
  (* 1. the length is at least 3 times its weight *)
  (* 2. the length is at most 5 times its weight *)
  (* 5. end is to the bottom right of d_start *)

type ld= {
  ld_start: point;
  end_: point adjust;
} (* Left Dot *)
  (* 1. the length is at least 1 times its weight *)
  (* 2. the length is at most 2 times its weight *)
  (* 5. end is to the bottom left of d_start *)

type wd= {
  wd_start: point;
  length: float adjust;
} (* Wilted Dot *)
  (* 1. the length is at least 1 times its weight *)
  (* 2. the length is at most 2 times its weight *)

type p= {
  p_start: point;
  ctrl1: point adjust;
  ctrl2: point adjust;
  end_: point;
} (* Press *)
  (* 1. the length is at least 4 times its weight *)
  (* 2. the angle is limited between 40 to 50 degrees *)
  (* 3. ctrl1 is to the bottom right of t_start *)
  (* 4. ctrl2 is to the top right of end_ *)

type up= {
  up_start: point adjust;
  p_start: point;
  ctrl1: point adjust;
  ctrl2: point adjust;
  end_: point;
} (* Upward horizontal – Press *)
  (* 1. the length is at least 4 times its weight *)
  (* 2. the angle is limited between 40 to 50 degrees *)
  (* 3. ctrl1 is to the bottom right of t_start *)
  (* 4. ctrl2 is to the top right of end_ *)
  (* 5. p_start is to the bottom left of p_start *)

type hp= {
  hp_start: point;
  length: float;
  ctrl1: point adjust;
  ctrl2: point adjust;
  end_: point;
} (* Horizontal – Press *)
  (* 1. the length of the horizontal is at least 3 times its weight *)
  (* 2. the length of the press is at least 4 times its weight *)
  (* 3. the angle is limited between 40 to 50 degrees *)
  (* 4. ctrl1 is to the bottom right of the start of the p stroke *)
  (* 5. ctrl2 is to the top right of end_ *)

type fp= {
  fp_start: point;
  ctrl1: point adjust;
  ctrl2: point adjust;
  end_: point;
} (* Flat Press *)
  (* 1. the length is at least 4 times its weight *)
  (* 2. the clockwise angle is limited between 5 to 30 degrees *)
  (* 3. ctrl1 is to the bottom right of t_start *)
  (* 4. ctrl2 is to the top right of end_ *)

type ufp= {
  ufp_start: point;
  p_start: point;
  ctrl1: point adjust;
  ctrl2: point adjust;
  end_: point;
} (* Upward horizontal – Flat Press *)
  (* 1. the length is at least 4 times its weight *)
  (* 2. the clockwise angle is limited between 5 to 30 degrees *)
  (* 3. ctrl1 is to the bottom right of t_start *)
  (* 4. ctrl2 is to the top right of end_ *)
  (* 5. p_start is to the bottom left of p_start *)

type c= {
  c_start: point;
  length: float;
  width: float;
  ctrl1: float adjust;
  ctrl2: float adjust;
} (* Clockwise curve *)

type a= {
  a_start: point;
  length: float;
  width: float;
  ctrl1: float adjust;
  ctrl2: float adjust;
} (* Anticlockwise curve *)

type o= {
  o_start: point;
  length: float;
  width: float;
  ctrl_h: float adjust;
  ctrl_v: float adjust;
} (* Oval *)

type hj= {
  hj_start: point;
  length: float;
  end_: point adjust;
} (* Horizontal – J hook *)

type uj= {
  uj_start: point;
  u_end: point;
  end_: point adjust;
} (* Upward horizontal – J hook *)

type ht= {
  ht_start: point;
  length: float;
  ctrl1: point adjust;
  ctrl2: point adjust;
  end_: point;
} (* Horizontal – Throw *)
  (* 1. the length is at least 4 times its weight *)
  (* 2. the angle is limited between 40 to 50 degrees *)
  (* 3. ctrl1 is to the bottom left of t_start *)
  (* 4. ctrl2 is to the top right of end_ *)

type hsv= {
  hsv_start: point;
  h_length: float;
  end_: point adjust;
} (* Horizontal – Slanted Vertical *)
  (* 1. the h_length is at least 4 times its weight *)
  (* 2. the angle is limited between 40 to 85 degrees *)
  (* 3. the length of the sv stroke is at least 4 times its weight *)

type hv= {
  hv_start: point;
  h_length: float;
  v_length: float adjust;
} (* Horizontal – Vertical *)
  (* 1. the h_length is at least 4 times its weight *)
  (* 2. the v_length is at least 4 times its weight *)

type hvj= {
  hvj_start: point;
  h_length: float;
  v_length: float adjust;
  end_: point adjust;
} (* Horizontal – Vertical – J hook *)
  (* 1. the h_length is at least 4 times its weight *)
  (* 2. the v_length is at least 4 times its weight *)
  (* 3. the v_length is at least 4 times its weight *)
  (* 4. end_ is to the top left of the end of the v stroke *)

type htj= {
  htj_start: point;
  length: float;
  ctrl1: point adjust;
  ctrl2: point adjust;
  t_end: point;
  end_: point adjust;
} (* Horizontal – Throw – J hook *)
  (* 1. end_ is to the top left of t_end *)

type utj= {
  utj_start: point;
  t_start: point;
  ctrl1: point adjust;
  ctrl2: point adjust;
  t_end: point;
  end_: point adjust;
} (* Upward horizontal – Throw – J hook *)
  (* 1. end_ is to the top left of t_end *)

type hvh= {
  hvh_start: point;
  h1_length: float;
  v_length: float;
  h2_length: float;
} (* Horizontal – Vertical – Horizontal *)

type hvu= {
  hvu_start: point;
  h_length: float;
  v_length: float;
  end_: point adjust;
} (* Horizontal – Vertical – Upward horizontal *)

type ha= {
  ha_start: point;
  h1_length: float;
  v_length: float;
  h2_length: float;
  a_radius: float adjust;
} (* Horizontal – Anticlockwise curve *)

type haj= {
  haj_start: point;
  h1_length: float;
  v_length: float;
  h2_length: float;
  a_radius: float adjust;
  end_: point adjust;
} (* Horizontal – Anticlockwise curve – J hook *)

type hpj= {
  hpj_start: point;
  length: float;
  ctrl1: point adjust;
  ctrl2: point adjust;
  p_end: point;
  end_: point adjust;
} (* Horizontal – Press – J hook *)
  (* 1. the length of the horizontal is at least 3 times its weight *)
  (* 2. the length of the press is at least 4 times its weight *)
  (* 3. the angle is limited between 40 to 50 degrees *)
  (* 4. ctrl1 is to the bottom right of the start of the p stroke *)
  (* 5. ctrl2 is to the top right of p_end *)

type htaj= {
  htaj_start: point;
  h1_length: float;
  t_end: point;
  a_radius: float adjust;
  h2_length: float;
  end_: point adjust;
} (* Horizontal – Throw – Anticlockwise curve – J hook *)

type htc= {
  htc_start: point;
  h_length: float;
  t_ctrl1: point adjust;
  t_ctrl2: point adjust;
  t_end: point;
  c_ctrl1: point adjust;
  c_ctrl2: point adjust;
  end_: point
} (* Horizontal – Throw – Clockwise curve *)

type htht= {
  htht_start: point;
  h1_length: float;
  t1_ctrl1: point adjust;
  t1_ctrl2: point adjust;
  t1_end: point;
  h2_length: float;
  t2_ctrl1: point adjust;
  t2_ctrl2: point adjust;
  end_: point;
} (* Horizontal – Throw – Horizontal – Throw *)

type htcj= {
  htcj_start: point;
  h_length: float;
  t_ctrl1: point adjust;
  t_ctrl2: point adjust;
  t_end: point;
  c_ctrl1: point adjust;
  c_ctrl2: point adjust;
  c_end: point;
  end_: point adjust;
} (* Horizontal – Throw – Clockwise curve – J hook *)

type hvhv= {
  hvhv_start: point;
  h1_length: float;
  v1_length: float;
  h2_length: float;
  v2_length: float;
} (* Horizontal – Vertical – Horizontal – Vertical *)

type hthtj= {
  hthtj_start: point;
  h1_length: float;
  t1_ctrl1: point adjust;
  t1_ctrl2: point adjust;
  t1_end: point;
  h2_length: float;
  t2_ctrl1: point adjust;
  t2_ctrl2: point adjust;
  t2_end: point;
  end_: point adjust;
} (* Horizontal – Throw – Horizontal – Throw – J hook *)

type vu= {
  vu_start: point;
  length: float;
  end_: point adjust;
} (* Vertical – Upward horizontal *)

type vh= {
  vh_start: point;
  v_length: float;
  h_length: float;
} (* Vertical – Horizontal *)

type va= {
  va_start: point;
  v_length: float;
  h_length: float;
  a_radius: float adjust;
} (* Vertical – Anticlockwise curve *)

type vaj= {
  vaj_start: point;
  v_length: float;
  h_length: float;
  a_radius: float adjust;
  end_: point adjust;
} (* Vertical – Anticlockwise curve – J hook *)

type vhv= {
  vhv_start: point;
  v1_length: float;
  h_length: float;
  v2_length: float;
} (* Vertical – Horizontal – Vertical *)

type vht= {
  vht_start: point;
  v_length: float;
  h_length: float;
  ctrl1: point adjust;
  ctrl2: point adjust;
  end_: point;
} (* Vertical – Horizontal – Throw *)

type vhtj= {
  vhtj_start: point;
  v_length: float;
  h_length: float;
  ctrl1: point adjust;
  ctrl2: point adjust;
  t_end: point;
  end_: point adjust;
} (* Vertical – Horizontal – Throw – J hook *)

type vj= {
  vj_start: point;
  length: float;
  end_: point adjust;
} (* Vertical – J hook *)

type vc= {
  vc_start: point;
  v_length: float;
  h_length: float;
  a_radius: float adjust;
} (* Vertical – Clockwise curve *)

type vcj= {
  vcj_start: point;
  v_length: float;
  h_length: float;
  a_radius: float adjust;
  end_: point adjust;
} (* Vertical – Clockwise curve – J hook *)

type tu= {
  tu_start: point;
  ctrl1: point adjust;
  ctrl2: point adjust;
  t_end: point;
  end_: point adjust;
} (* Throw – Upward horizontal *)

type th= {
  th_start: point;
  ctrl1: point adjust;
  ctrl2: point adjust;
  t_end: point;
  length: float adjust;
} (* Throw – Horizontal *)

type td= {
  td_start: point;
  ctrl1: point adjust;
  ctrl2: point adjust;
  t_end: point;
  end_: point adjust;
} (* Throw – Dot *)

type wtd= {
  wtd_start: point;
  v_length: float adjust;
  ctrl1: point adjust;
  ctrl2: point adjust;
  t_end: point;
  end_: point adjust;
} (* Wilted Throw – Dot *)

type tht= {
  tht_start: point;
  t1_ctrl1: point adjust;
  t1_ctrl2: point adjust;
  t1_end: point;
  length: float;
  t2_ctrl1: point adjust;
  t2_ctrl2: point adjust;
  end_: point;
} (* Throw – Horizontal – Throw *)

type thtj= {
  thtj_start: point;
  t1_ctrl1: point adjust;
  t1_ctrl2: point adjust;
  t1_end: point;
  length: float;
  t2_ctrl1: point adjust;
  t2_ctrl2: point adjust;
  t2_end: point;
  end_: point adjust;
} (* Throw – Horizontal – Throw – J hook *)

type tj= {
  tj_start: point;
  ctrl1: point adjust;
  ctrl2: point adjust;
  t_end: point;
  end_: point adjust;
} (* Throw – J hook *)
  (* 1. end_ is to the bottom left of tj_start *)

type cj= {
  cj_start: point;
  ctrl1: point adjust;
  ctrl2: point adjust;
  t_end: point;
  end_: point adjust;
} (* Clockwise curve – J hook *)
  (* 1. end_ is to the bottom right of cj_start *)

type fpj= {
  fpj_start: point;
  ctrl1: point adjust;
  ctrl2: point adjust;
  t_end: point;
  end_: point adjust;
} (* Flat Press – J hook *)
  (* 1. the angle is limited between 5 to 40 degrees *)

type pj= {
  pj_start: point;
  ctrl1: point adjust;
  ctrl2: point adjust;
  t_end: point;
  end_: point adjust;
} (* Press – J hook *)
  (* 1. the angle is limited between 45 to 85 degrees *)

type thtaj= {
  thtaj_start: point;
  ctrl1: point adjust;
  ctrl2: point adjust;
  t1_end: point;
  h1_length: float;
  t2_end: point;
  a_radius: float adjust;
  h2_length: float;
  end_: point adjust;
} (* Throw – Horizontal – Throw – Anticlockwise curve – J hook *)

type tod= {
  tod_start: point;
  ctrl1: point adjust;
  ctrl2: float adjust;
  bottom: point;
  ctrl3: float adjust;
  ctrl4: float adjust;
  left: point;
  end_: point adjust;
} (* Throw – Oval – Dot *)


type t=
  | H of h         (* Horizontal *)
  | Sh of sh       (* Slanted Horizontal *)
  | U of u         (* Upward horizontal *)
  | Du of du       (* Dot – Upward horizontal *)
  | V of v         (* Vertical *)
  | Sv of sv       (* Slanted Vertical *)
  | Rsv of rsv     (* Right Slanted Vertical *)
  | T of t'         (* Throw *)
  | Ft of ft       (* Flat Throw *)
  | Wt of wt       (* Wilted Throw *)
  | D of d         (* Dot *)
  | Ed of ed       (* Extended Dot *)
  | Ld of ld       (* Left Dot *)
  | Wd of wd       (* Wilted Dot *)
  | P of p         (* Press *)
  | Up of up       (* Upward horizontal – Press *)
  | Hp of hp       (* Horizontal – Press *)
  | Fp of fp       (* Flat Press *)
  | Ufp of ufp     (* Upward horizontal – Flat Press *)
  | C of c         (* Clockwise curve *)
  | A of a         (* Anticlockwise curve *)
  | O of o         (* Oval *)
  | Hj of hj       (* Horizontal – J hook *)
  | Uj of uj       (* Upward horizontal – J hook *)
  | Ht of ht       (* Horizontal – Throw *)
  | Hsv of hsv     (* Horizontal – Slanted Vertical *)
  | Hv of hv       (* Horizontal – Vertical *)
  | Hvj of hvj     (* Horizontal – Vertical – J hook *)
  | Htj of htj     (* Horizontal – Throw – J hook *)
  | Utj of utj     (* Upward horizontal – Throw – J hook *)
  | Hvh of hvh     (* Horizontal – Vertical – Horizontal *)
  | Hvu of hvu     (* Horizontal – Vertical – Upward horizontal *)
  | Ha of ha       (* Horizontal – Anticlockwise curve *)
  | Haj of haj     (* Horizontal – Anticlockwise curve – J hook *)
  | Hpj of hpj     (* Horizontal – Press – J hook *)
  | Htaj of htaj   (* Horizontal – Throw – Anticlockwise curve – J hook *)
  | Htc of htc     (* Horizontal – Throw – Clockwise curve *)
  | Htht of htht   (* Horizontal – Throw – Horizontal – Throw *)
  | Htcj of htcj   (* Horizontal – Throw – Clockwise curve – J hook *)
  | Hvhv of hvhv   (* Horizontal – Vertical – Horizontal – Vertical *)
  | Hthtj of hthtj (* Horizontal – Throw – Horizontal – Throw – J hook *)
  | Vu of vu       (* Vertical – Upward horizontal *)
  | Vh of vh       (* Vertical – Horizontal *)
  | Va of va       (* Vertical – Anticlockwise curve *)
  | Vaj of vaj     (* Vertical – Anticlockwise curve – J hook *)
  | Vhv of vhv     (* Vertical – Horizontal – Vertical *)
  | Vht of vht     (* Vertical – Horizontal – Throw *)
  | Vhtj of vhtj   (* Vertical – Horizontal – Throw – J hook *)
  | Vj of vj       (* Vertical – J hook *)
  | Vc of vc       (* Vertical – Clockwise curve *)
  | Vcj of vcj     (* Vertical – Clockwise curve – J hook *)
  | Tu of tu       (* Throw – Upward horizontal *)
  | Th of th       (* Throw – Horizontal *)
  | Td of td       (* Throw – Dot *)
  | Wtd of wtd     (* Wilted Throw – Dot *)
  | Tht of tht     (* Throw – Horizontal – Throw *)
  | Thtj of thtj   (* Throw – Horizontal – Throw – J hook *)
  | Tj of tj       (* Throw – J hook *)
  | Cj of cj       (* Clockwise curve – J hook *)
  | Fpj of fpj     (* Flat Press – J hook *)
  | Pj of pj       (* Press – J hook *)
  | Thtaj of thtaj (* Throw – Horizontal – Throw – Anticlockwise curve – J hook *)
  | Tod of tod     (* Throw – Oval – Dot *)

let tag_of_t t=
  let open Tag in
  match t with
  | H _-> S_h
  | Sh _-> S_sh
  | U _-> S_u
  | Du _-> S_du
  | V _-> S_v
  | Sv _-> S_sv
  | Rsv _-> S_rsv
  | T _-> S_t
  | Ft _-> S_ft
  | Wt _-> S_wt
  | D _-> S_d
  | Ed _-> S_ed
  | Ld _-> S_ld
  | Wd _-> S_wd
  | P _-> S_p
  | Up _-> S_up
  | Hp _-> S_hp
  | Fp _-> S_fp
  | Ufp _-> S_ufp
  | C _-> S_c
  | A _-> S_a
  | O _-> S_o
  | Hj _-> S_hj
  | Uj _-> S_uj
  | Ht _-> S_ht
  | Hsv _-> S_hsv
  | Hv _-> S_hv
  | Hvj _-> S_hvj
  | Htj _-> S_htj
  | Utj _-> S_utj
  | Hvh _-> S_hvh
  | Hvu _-> S_hvu
  | Ha _-> S_ha
  | Haj _-> S_haj
  | Hpj _-> S_hpj
  | Htaj _-> S_htaj
  | Htc _-> S_htc
  | Htht _-> S_htht
  | Htcj _-> S_htcj
  | Hvhv _-> S_hvhv
  | Hthtj _-> S_hthtj
  | Vu _-> S_vu
  | Vh _-> S_vh
  | Va _-> S_va
  | Vaj _-> S_vaj
  | Vhv _-> S_vhv
  | Vht _-> S_vht
  | Vhtj _-> S_vhtj
  | Vj _-> S_vj
  | Vc _-> S_vc
  | Vcj _-> S_vcj
  | Tu _-> S_tu
  | Th _-> S_th
  | Td _-> S_td
  | Wtd _-> S_wtd
  | Tht _-> S_tht
  | Thtj _-> S_thtj
  | Tj _-> S_tj
  | Cj _-> S_cj
  | Fpj _-> S_fpj
  | Pj _-> S_pj
  | Thtaj _-> S_thtaj
  | Tod _-> S_tod

let compare= compare

let to_string stroke=
  tag_to_string (tag_of_t stroke)

module Transform = struct
  open Point

  let point_translate ~d p=
    match p with
    | Auto-> Auto
    | Specify p-> Specify (p+d)

  let h_translate ~d h= { h with h_start= h.h_start + d }
  let sh_translate ~d sh=
    { sh_start= sh.sh_start + d; end_= sh.end_ + d; }
  let u_translate ~d u=
    { u_start= u.u_start + d; end_= u.end_ + d; }
  let du_translate ~d du=
    {
      du_start= du.du_start + d;
      dot= point_translate ~d du.dot;
      end_= du.end_ + d;
    }
  let v_translate ~d v= { v with v_start= v.v_start + d }
  let sv_translate ~d sv= { sv with sv_start= sv.sv_start + d }
  let rsv_translate ~d rsv= { rsv with rsv_start= rsv.rsv_start + d }
  let t'_translate ~d t'=
    {
      t'_start= t'.t'_start + d;
      ctrl1= point_translate ~d t'.ctrl1;
      ctrl2= point_translate ~d t'.ctrl2;
      end_= t'.end_ + d;
  }
  let ft_translate ~d ft=
    {
      ft_start= ft.ft_start + d;
      ctrl1= point_translate ~d ft.ctrl1;
      ctrl2= point_translate ~d ft.ctrl2;
      end_= ft.end_ + d;
    }
  let wt_translate ~d wt=
    { wt with
      wt_start= wt.wt_start + d;
      ctrl1= point_translate ~d wt.ctrl1;
      ctrl2= point_translate ~d wt.ctrl2;
      end_= wt.end_ + d;
    }
  let d_translate ~d d'=
    {
      d_start= d'.d_start + d;
      end_= point_translate ~d d'.end_;
    }
  let ed_translate ~d ed=
    {
      ed_start= ed.ed_start + d;
      end_= point_translate ~d ed.end_;
    }
  let ld_translate ~d ld=
    {
      ld_start= ld.ld_start + d;
      end_= point_translate ~d ld.end_;
    }
  let wd_translate ~d wd= { wd with wd_start= wd.wd_start + d }
  let p_translate ~d (p:p)=
    {
      p_start= p.p_start + d;
      ctrl1= point_translate ~d p.ctrl1;
      ctrl2= point_translate ~d p.ctrl2;
      end_= p.end_ + d;
  }
  let up_translate ~d up=
    {
      up_start= point_translate ~d up.up_start;
      p_start= up.p_start + d;
      ctrl1= point_translate ~d up.ctrl1;
      ctrl2= point_translate ~d up.ctrl2;
      end_= up.end_ + d;
    }
  let hp_translate ~d hp=
    { hp with
      hp_start= hp.hp_start + d;
      ctrl1= point_translate ~d hp.ctrl1;
      ctrl2= point_translate ~d hp.ctrl2;
      end_= hp.end_ + d;
    }
  let fp_translate ~d fp=
    {
      fp_start= fp.fp_start + d;
      ctrl1= point_translate ~d fp.ctrl1;
      ctrl2= point_translate ~d fp.ctrl2;
      end_= fp.end_ + d;
    }
  let ufp_translate ~d ufp=
    {
      ufp_start= ufp.ufp_start + d;
      p_start= ufp.p_start + d;
      ctrl1= point_translate ~d ufp.ctrl1;
      ctrl2= point_translate ~d ufp.ctrl2;
      end_= ufp.end_ + d;
    }
  let c_translate ~d c= { c with c_start= c.c_start + d }
  let a_translate ~d a= { a with a_start= a.a_start + d }
  let o_translate ~d o= { o with o_start= o.o_start + d }
  let hj_translate ~d hj=
    { hj with
      hj_start= hj.hj_start + d;
      end_= point_translate ~d hj.end_;
  }
  let uj_translate ~d uj=
    {
      uj_start= uj.uj_start + d;
      u_end= uj.u_end + d;
      end_= point_translate ~d uj.end_;
    }
  let ht_translate ~d ht=
    { ht with
      ht_start= ht.ht_start + d;
      ctrl1= point_translate ~d ht.ctrl1;
      ctrl2= point_translate ~d ht.ctrl2;
      end_ = ht.end_ + d;
    }
  let hsv_translate ~d hsv=
    { hsv with
      hsv_start= hsv.hsv_start + d;
      end_= point_translate ~d hsv.end_;
    }
  let hv_translate ~d hv= { hv with hv_start= hv.hv_start + d }
  let hvj_translate ~d hvj=
    { hvj with
      hvj_start= hvj.hvj_start + d;
      end_= point_translate ~d hvj.end_;
  }
  let htj_translate ~d htj=
    let pt= point_translate ~d in
    { htj with
      htj_start= htj.htj_start + d;
      ctrl1= pt htj.ctrl1;
      ctrl2= pt htj.ctrl2;
      t_end= htj.t_end + d;
      end_= pt htj.end_;
    }
  let utj_translate ~d utj=
    let pt= point_translate ~d in
    {
      utj_start= utj.utj_start + d;
      t_start= utj.t_start + d;
      ctrl1= pt utj.ctrl1;
      ctrl2= pt utj.ctrl2;
      t_end= utj.t_end + d;
      end_= pt utj.end_;
    }
  let hvh_translate ~d hvh= { hvh with hvh_start= hvh.hvh_start + d }
  let hvu_translate ~d hvu=
    { hvu with
      hvu_start= hvu.hvu_start + d;
      end_= point_translate ~d hvu.end_;
  }
  let ha_translate ~d ha= { ha with ha_start= ha.ha_start + d }
  let haj_translate ~d haj=
    { haj with
      haj_start= haj.haj_start + d;
      end_= point_translate ~d haj.end_;
  }
  let hpj_translate ~d hpj=
    let pt= point_translate ~d in
    { hpj with
      hpj_start= hpj.hpj_start + d;
      ctrl1= pt hpj.ctrl1;
      ctrl2= pt hpj.ctrl2;
      p_end= hpj.p_end + d;
      end_= pt hpj.end_;
    }
  let htaj_translate ~d htaj=
    { htaj with
      htaj_start= htaj.htaj_start + d;
      t_end= htaj.t_end + d;
      end_= point_translate ~d htaj.end_;
    }
  let htc_translate ~d htc=
    let pt= point_translate ~d in
    { htc with
      htc_start= htc.htc_start + d;
      t_ctrl1= pt htc.t_ctrl1;
      t_ctrl2= pt htc.t_ctrl2;
      t_end= htc.t_end + d;
      c_ctrl1= pt htc.c_ctrl1;
      c_ctrl2= pt htc.c_ctrl2;
    }
  let htht_translate ~d htht=
    let pt= point_translate ~d in
    { htht with
      htht_start= htht.htht_start + d;
      t1_ctrl1= pt htht.t1_ctrl1;
      t1_ctrl2= pt htht.t1_ctrl2;
      t1_end= htht.t1_end + d;
      t2_ctrl1= pt htht.t2_ctrl1;
      t2_ctrl2= pt htht.t2_ctrl2;
      end_= htht.end_ + d;
    }
  let htcj_translate ~d htcj=
    let pt= point_translate ~d in
    { htcj with
      htcj_start= htcj.htcj_start + d;
      t_ctrl1= pt htcj.t_ctrl1;
      t_ctrl2= pt htcj.t_ctrl2;
      t_end= htcj.t_end + d;
      c_ctrl1= pt htcj.c_ctrl1;
      c_ctrl2= pt htcj.c_ctrl2;
      end_= pt htcj.end_;
    }
  let hvhv_translate ~d hvhv= { hvhv with hvhv_start= hvhv.hvhv_start + d }
  let hthtj_translate ~d hthtj=
    let pt= point_translate ~d in
    { hthtj with
      hthtj_start= hthtj.hthtj_start + d;
      t1_ctrl1= pt hthtj.t1_ctrl1;
      t1_ctrl2= pt hthtj.t1_ctrl2;
      t1_end= hthtj.t1_end + d;
      t2_ctrl1= pt hthtj.t2_ctrl1;
      t2_ctrl2= pt hthtj.t2_ctrl2;
      t2_end= hthtj.t2_end + d;
      end_= pt hthtj.end_;
  }
  let vu_translate ~d vu=
    { vu with
      vu_start= vu.vu_start + d;
      end_= point_translate ~d vu.end_;
    }
  let vh_translate ~d vh= { vh with vh_start= vh.vh_start + d }
  let va_translate ~d va= { va with va_start= va.va_start + d }
  let vaj_translate ~d vaj=
    { vaj with
      vaj_start= vaj.vaj_start + d;
      end_= point_translate ~d vaj.end_;
  }
  let vhv_translate ~d vhv= { vhv with vhv_start= vhv.vhv_start + d }
  let vht_translate ~d vht=
    let pt= point_translate ~d in
    { vht with
      vht_start= vht.vht_start + d;
      ctrl1= pt vht.ctrl1;
      ctrl2= pt vht.ctrl1;
      end_= vht.end_ + d;
  }
  let vhtj_translate ~d vhtj=
    let pt= point_translate ~d in
    { vhtj with
      vhtj_start= vhtj.vhtj_start + d;
      ctrl1= pt vhtj.ctrl1;
      ctrl2= pt vhtj.ctrl1;
      t_end= vhtj.t_end + d;
      end_= pt vhtj.end_;
    }
  let vj_translate ~d vj=
    { vj with
      vj_start= vj.vj_start + d;
      end_= point_translate ~d vj.end_;
    }
  let vc_translate ~d vc= { vc with vc_start= vc.vc_start + d }
  let vcj_translate ~d vcj=
    { vcj with
      vcj_start= vcj.vcj_start + d;
      end_= point_translate ~d vcj.end_;
  }
  let tu_translate ~d tu=
    let pt= point_translate ~d in
    {
      tu_start= tu.tu_start + d;
      ctrl1= pt tu.ctrl1;
      ctrl2= pt tu.ctrl2;
      t_end= tu.t_end + d;
      end_= pt tu.end_;
    }
  let th_translate ~d th=
    let pt= point_translate ~d in
    { th with
      th_start= th.th_start + d;
      ctrl1= pt th.ctrl1;
      ctrl2= pt th.ctrl2;
      t_end= th.t_end + d;
    }
  let td_translate ~d td=
    let pt= point_translate ~d in
    {
      td_start= td.td_start + d;
      ctrl1= pt td.ctrl1;
      ctrl2= pt td.ctrl2;
      t_end= td.t_end + d;
      end_= pt td.end_;
    }
  let wtd_translate ~d wtd=
    let pt= point_translate ~d in
    { wtd with
      wtd_start= wtd.wtd_start + d;
      ctrl1= pt wtd.ctrl1;
      ctrl2= pt wtd.ctrl2;
      t_end= wtd.t_end + d;
      end_= pt wtd.end_;
    }
  let tht_translate ~d tht=
    let pt= point_translate ~d in
    { tht with
      tht_start= tht.tht_start + d;
      t1_ctrl1= pt tht.t1_ctrl1;
      t1_ctrl2= pt tht.t1_ctrl2;
      t1_end= tht.t1_end + d;
      t2_ctrl1= pt tht.t2_ctrl1;
      t2_ctrl2= pt tht.t2_ctrl2;
      end_= tht.end_ + d;
    }
  let thtj_translate ~d thtj=
    let pt= point_translate ~d in
    { thtj with
      thtj_start= thtj.thtj_start + d;
      t1_ctrl1= pt thtj.t1_ctrl1;
      t1_ctrl2= pt thtj.t1_ctrl2;
      t1_end= thtj.t1_end + d;
      t2_ctrl1= pt thtj.t2_ctrl1;
      t2_ctrl2= pt thtj.t2_ctrl2;
      t2_end= thtj.t2_end + d;
      end_= pt thtj.end_;
    }
  let tj_translate ~d tj=
    let pt= point_translate ~d in
    {
      tj_start= tj.tj_start + d;
      ctrl1= pt tj.ctrl1;
      ctrl2= pt tj.ctrl2;
      t_end= tj.t_end + d;
      end_= pt tj.end_;
    }
  let cj_translate ~d cj=
    let pt= point_translate ~d in
    {
      cj_start= cj.cj_start;
      ctrl1= pt cj.ctrl1;
      ctrl2= pt cj.ctrl2;
      t_end= cj.t_end + d;
      end_= pt cj.end_;
    }
  let fpj_translate ~d fpj=
    let pt= point_translate ~d in
    {
      fpj_start= fpj.fpj_start + d;
      ctrl1= pt fpj.ctrl1;
      ctrl2= pt fpj.ctrl2;
      t_end= fpj.t_end + d;
      end_= pt fpj.end_;
    }
  let pj_translate ~d pj=
    let pt= point_translate ~d in
    {
      pj_start= pj.pj_start + d;
      ctrl1= pt pj.ctrl1;
      ctrl2= pt pj.ctrl2;
      t_end= pj.t_end + d;
      end_= pt pj.end_;
    }
  let thtaj_translate ~d thtaj=
    let pt= point_translate ~d in
    { thtaj with
      thtaj_start= thtaj.thtaj_start + d;
      ctrl1= pt thtaj.ctrl1;
      ctrl2= pt thtaj.ctrl1;
      t1_end= thtaj.t1_end + d;
      t2_end= thtaj.t2_end + d;
      end_= pt thtaj.end_;
    }
  let tod_translate ~d tod=
    let pt= point_translate ~d in
    { tod with
      tod_start= tod.tod_start;
      ctrl1= pt tod.ctrl1;
      bottom= tod.bottom + d;
      left= tod.left + d;
      end_= pt tod.end_;
    }

  let diagonal r=
    let open Float in
    pow r.x 2. +. pow r.y 2. |> sqrt

  let point_scale ?(origin={x=0.;y=0.}) ~r p=
    origin + (p - origin) * r

  let point_adjust_scale ?(origin={x=0.;y=0.}) ~r p=
    match p with
    | Auto-> Auto
    | Specify p-> Specify (point_scale ~origin ~r p)

  let scalar_adjust_scale ~r s=
    match s with
    | Auto-> Auto
    | Specify s-> Specify (s *. r)

  let h_scale ?origin ~r h= {
    h_start= point_scale ?origin ~r h.h_start;
    length= h.length *. r.x;
  }
  let sh_scale ?origin ~r sh=
    {
      sh_start= point_scale ?origin ~r sh.sh_start;
      end_= point_scale ?origin ~r sh.end_;
  }
  let u_scale ?origin ~r u=
    {
      u_start= point_scale ?origin ~r u.u_start;
      end_= point_scale ?origin ~r u.end_;
    }
  let du_scale ?origin ~r du=
    {
      du_start= point_scale ?origin ~r du.du_start;
      dot= point_adjust_scale ?origin ~r du.dot;
      end_= point_scale ?origin ~r du.end_;
    }
  let v_scale ?origin ~r v= {
    v_start= point_scale ?origin ~r v.v_start;
    length= v.length *. r.x;
    }
  let sv_scale ?origin ~r sv= {
    sv_start= point_scale ?origin ~r sv.sv_start;
    length= sv.length *. r.y;
    width= sv.width *. r.x;
  }
  let rsv_scale ?origin ~r rsv= {
    rsv_start= point_scale ?origin ~r rsv.rsv_start;
    length= rsv.length *. r.y;
    width= rsv.width *. r.x;
  }
  let t'_scale ?origin ~r t'=
    {
      t'_start= point_scale ?origin ~r t'.t'_start;
      ctrl1= point_adjust_scale ?origin ~r t'.ctrl1;
      ctrl2= point_adjust_scale ?origin ~r t'.ctrl2;
      end_= point_scale ?origin ~r t'.end_;
  }
  let ft_scale ?origin ~r ft=
    {
      ft_start= point_scale ?origin ~r ft.ft_start;
      ctrl1= point_adjust_scale ?origin ~r ft.ctrl1;
      ctrl2= point_adjust_scale ?origin ~r ft.ctrl2;
      end_= point_scale ?origin ~r ft.end_;
    }
  let wt_scale ?origin ~r wt=
    {
      wt_start= point_scale ?origin ~r wt.wt_start;
      v_length= scalar_adjust_scale ~r:r.y wt.v_length;
      ctrl1= point_adjust_scale ?origin ~r wt.ctrl1;
      ctrl2= point_adjust_scale ?origin ~r wt.ctrl2;
      end_= point_scale ?origin ~r wt.end_;
    }
  let d_scale ?origin ~r d'=
    {
      d_start= point_scale ?origin ~r d'.d_start;
      end_= point_adjust_scale ?origin ~r d'.end_;
    }
  let ed_scale ?origin ~r ed=
    {
      ed_start= point_scale ?origin ~r ed.ed_start;
      end_= point_adjust_scale ?origin ~r ed.end_;
    }
  let ld_scale ?origin ~r ld=
    {
      ld_start= point_scale ?origin ~r ld.ld_start;
      end_= point_adjust_scale ?origin ~r ld.end_;
    }
  let wd_scale ?origin ~r wd= {
    wd_start= point_scale ?origin ~r wd.wd_start;
    length= scalar_adjust_scale ~r:r.y wd.length;
    }
  let p_scale ?origin ~r (p:p)=
    {
      p_start= point_scale ?origin ~r p.p_start;
      ctrl1= point_adjust_scale ?origin ~r p.ctrl1;
      ctrl2= point_adjust_scale ?origin ~r p.ctrl2;
      end_= point_scale ?origin ~r p.end_;
  }
  let up_scale ?origin ~r up=
    {
      up_start= point_adjust_scale ?origin ~r up.up_start;
      p_start= point_scale ?origin ~r up.p_start;
      ctrl1= point_adjust_scale ?origin ~r up.ctrl1;
      ctrl2= point_adjust_scale ?origin ~r up.ctrl2;
      end_= point_scale ?origin ~r up.end_;
    }
  let hp_scale ?origin ~r hp=
    {
      hp_start= point_scale ?origin ~r hp.hp_start;
      length= hp.length *. r.x;
      ctrl1= point_adjust_scale ?origin ~r hp.ctrl1;
      ctrl2= point_adjust_scale ?origin ~r hp.ctrl2;
      end_= point_scale ?origin ~r hp.end_;
    }
  let fp_scale ?origin ~r fp=
    {
      fp_start= point_scale ?origin ~r fp.fp_start;
      ctrl1= point_adjust_scale ?origin ~r fp.ctrl1;
      ctrl2= point_adjust_scale ?origin ~r fp.ctrl2;
      end_= point_scale ?origin ~r fp.end_;
    }
  let ufp_scale ?origin ~r ufp=
    {
      ufp_start= point_scale ?origin ~r ufp.ufp_start;
      p_start= point_scale ?origin ~r ufp.p_start;
      ctrl1= point_adjust_scale ?origin ~r ufp.ctrl1;
      ctrl2= point_adjust_scale ?origin ~r ufp.ctrl2;
      end_= point_scale ?origin ~r ufp.end_;
    }
  let c_scale ?origin ~r c= {
    c_start= point_scale ?origin ~r c.c_start ;
      length= c.length *. r.y;
      width= c.width *. r.x;
      ctrl1= scalar_adjust_scale ~r:r.x c.ctrl1;
      ctrl2= scalar_adjust_scale ~r:r.y c.ctrl2;
    }
  let a_scale ?origin ~r a= {
    a_start= point_scale ?origin ~r a.a_start ;
      length= a.length *. r.y;
      width= a.width *. r.x;
      ctrl1= scalar_adjust_scale ~r:r.x a.ctrl1;
      ctrl2= scalar_adjust_scale ~r:r.y a.ctrl2;
  }
  let o_scale ?origin ~r o= {
    o_start= point_scale ?origin ~r o.o_start ;
      length= o.length *. r.y;
      width= o.width *. r.x;
      ctrl_h= scalar_adjust_scale ~r:r.x o.ctrl_h;
      ctrl_v= scalar_adjust_scale ~r:r.y o.ctrl_v;
  }
  let hj_scale ?origin ~r hj=
    {
      hj_start= point_scale ?origin ~r hj.hj_start;
      length= hj.length *. r.x;
      end_= point_adjust_scale ?origin ~r hj.end_;
  }
  let uj_scale ?origin ~r uj=
    {
      uj_start= point_scale ?origin ~r uj.uj_start;
      u_end= point_scale ?origin ~r uj.u_end;
      end_= point_adjust_scale ?origin ~r uj.end_;
    }
  let ht_scale ?origin ~r ht=
    {
      ht_start= point_scale ?origin ~r ht.ht_start;
      length= ht.length *. r.x;
      ctrl1= point_adjust_scale ?origin ~r ht.ctrl1;
      ctrl2= point_adjust_scale ?origin ~r ht.ctrl2;
      end_ = point_scale ?origin ~r ht.end_;
    }
  let hsv_scale ?origin ~r hsv=
    {
      hsv_start= point_scale ?origin ~r hsv.hsv_start;
      h_length= hsv.h_length *. r.x;
      end_= point_adjust_scale ?origin ~r hsv.end_;
    }
  let hv_scale ?origin ~r hv= {
    hv_start= point_scale ?origin ~r hv.hv_start;
    h_length= hv.h_length *. r.x;
    v_length= scalar_adjust_scale ~r:r.y hv.v_length;
    }
  let hvj_scale ?origin ~r hvj=
    {
      hvj_start= point_scale ?origin ~r hvj.hvj_start;
    h_length= hvj.h_length *. r.x;
    v_length= scalar_adjust_scale ~r:r.y hvj.v_length;
      end_= point_adjust_scale ?origin ~r hvj.end_;
  }
  let htj_scale ?origin ~r htj=
    let pt= point_adjust_scale ?origin ~r in
    {
      htj_start= point_scale ?origin ~r htj.htj_start;
      length= htj.length *. r.x;
      ctrl1= pt htj.ctrl1;
      ctrl2= pt htj.ctrl2;
      t_end= point_scale ?origin ~r htj.t_end;
      end_= pt htj.end_;
    }
  let utj_scale ?origin ~r utj=
    let pt= point_adjust_scale ?origin ~r in
    {
      utj_start= point_scale ?origin ~r utj.utj_start;
      t_start= point_scale ?origin ~r utj.t_start;
      ctrl1= pt utj.ctrl1;
      ctrl2= pt utj.ctrl2;
      t_end= point_scale ?origin ~r utj.t_end;
      end_= pt utj.end_;
    }
  let hvh_scale ?origin ~r hvh= {
    hvh_start= point_scale ?origin ~r hvh.hvh_start;
    h1_length= hvh.h1_length *. r.x;
    v_length= hvh.v_length *. r.y;
    h2_length= hvh.h2_length *. r.x;
    }
  let hvu_scale ?origin ~r hvu=
    {
      hvu_start= point_scale ?origin ~r hvu.hvu_start;
      h_length= hvu.h_length *. r.x;
      v_length= hvu.v_length *. r.y;
      end_= point_adjust_scale ?origin ~r hvu.end_;
  }
  let ha_scale ?origin ~r ha= {
    ha_start= point_scale ?origin ~r ha.ha_start;
    h1_length= ha.h1_length *. r.x;
    v_length= ha.v_length *. r.y;
    h2_length= ha.h2_length *. r.x;
    a_radius= scalar_adjust_scale ~r:(diagonal r) ha.a_radius;
    }
  let haj_scale ?origin ~r haj=
    {
      haj_start= point_scale ?origin ~r haj.haj_start;
      h1_length= haj.h1_length *. r.x;
      v_length= haj.v_length *. r.y;
      h2_length= haj.h2_length *. r.x;
      a_radius= scalar_adjust_scale ~r:(diagonal r) haj.a_radius;
      end_= point_adjust_scale ?origin ~r haj.end_;
  }
  let hpj_scale ?origin ~r hpj=
    let pt= point_adjust_scale ?origin ~r in
    {
      hpj_start= point_scale ?origin ~r hpj.hpj_start;
      length= hpj.length *. r.x;
      ctrl1= pt hpj.ctrl1;
      ctrl2= pt hpj.ctrl2;
      p_end= point_scale ?origin ~r hpj.p_end;
      end_= pt hpj.end_;
    }
  let htaj_scale ?origin ~r htaj=
    {
      htaj_start= point_scale ?origin ~r htaj.htaj_start;
      h1_length= htaj.h1_length *. r.x;
      t_end= point_scale ?origin ~r htaj.t_end;
      h2_length= htaj.h2_length *. r.x;
      a_radius= scalar_adjust_scale ~r:(diagonal r) htaj.a_radius;
      end_= point_adjust_scale ?origin ~r htaj.end_;
    }
  let htc_scale ?origin ~r htc=
    let pt= point_adjust_scale ?origin ~r
    and ps= point_scale ?origin ~r in
    {
      htc_start= ps htc.htc_start;
      h_length= htc.h_length *. r.x;
      t_ctrl1= pt htc.t_ctrl1;
      t_ctrl2= pt htc.t_ctrl2;
      t_end= ps htc.t_end;
      c_ctrl1= pt htc.c_ctrl1;
      c_ctrl2= pt htc.c_ctrl2;
      end_= ps htc.end_;
    }
  let htht_scale ?origin ~r htht=
    let pt= point_adjust_scale ?origin ~r in
    {
      htht_start= point_scale ?origin ~r htht.htht_start;
      h1_length= htht.h1_length *. r.x;
      t1_ctrl1= pt htht.t1_ctrl1;
      t1_ctrl2= pt htht.t1_ctrl2;
      t1_end= point_scale ?origin ~r htht.t1_end;
      h2_length= htht.h2_length *. r.x;
      t2_ctrl1= pt htht.t2_ctrl1;
      t2_ctrl2= pt htht.t2_ctrl2;
      end_= point_scale ?origin ~r htht.end_;
    }
  let htcj_scale ?origin ~r htcj=
    let pt= point_adjust_scale ?origin ~r
    and ps= point_scale ?origin ~r in
    {
      htcj_start= ps htcj.htcj_start;
      h_length= htcj.h_length *. r.x;
      t_ctrl1= pt htcj.t_ctrl1;
      t_ctrl2= pt htcj.t_ctrl2;
      t_end= ps htcj.t_end;
      c_ctrl1= pt htcj.c_ctrl1;
      c_ctrl2= pt htcj.c_ctrl2;
      c_end= ps htcj.c_end;
      end_= pt htcj.end_;
    }
  let hvhv_scale ?origin ~r hvhv= {
    hvhv_start= point_scale ?origin ~r hvhv.hvhv_start;
    h1_length= hvhv.h1_length *. r.x;
    v1_length= hvhv.v1_length *. r.y;
    h2_length= hvhv.h2_length *. r.x;
    v2_length= hvhv.v2_length *. r.y;
    }
  let hthtj_scale ?origin ~r hthtj=
    let pt= point_adjust_scale ?origin ~r in
    {
      hthtj_start= point_scale ?origin ~r hthtj.hthtj_start;
      h1_length= hthtj.h1_length *. r.x;
      t1_ctrl1= pt hthtj.t1_ctrl1;
      t1_ctrl2= pt hthtj.t1_ctrl2;
      t1_end= point_scale ?origin ~r hthtj.t1_end;
      h2_length= hthtj.h2_length *. r.x;
      t2_ctrl1= pt hthtj.t2_ctrl1;
      t2_ctrl2= pt hthtj.t2_ctrl2;
      t2_end= point_scale ?origin ~r hthtj.t2_end;
      end_= pt hthtj.end_;
  }
  let vu_scale ?origin ~r vu=
    {
      vu_start= point_scale ?origin ~r vu.vu_start;
      length= vu.length *. r.y;
      end_= point_adjust_scale ?origin ~r vu.end_;
    }
  let vh_scale ?origin ~r vh= {
    vh_start= point_scale ?origin ~r vh.vh_start;
    v_length= vh.v_length *. r.y;
    h_length= vh.h_length *. r.x;
    }
  let va_scale ?origin ~r va= {
    va_start= point_scale ?origin ~r va.va_start;
    v_length= va.v_length *. r.y;
    h_length= va.h_length *. r.x;
    a_radius= scalar_adjust_scale ~r:(diagonal r) va.a_radius;
  }
  let vaj_scale ?origin ~r vaj=
    {
      vaj_start= point_scale ?origin ~r vaj.vaj_start;
      v_length= vaj.v_length *. r.y;
      h_length= vaj.h_length *. r.x;
      a_radius= scalar_adjust_scale ~r:(diagonal r) vaj.a_radius;
      end_= point_adjust_scale ?origin ~r vaj.end_;
  }
  let vhv_scale ?origin ~r vhv= {
    vhv_start= point_scale ?origin ~r vhv.vhv_start;
    v1_length= vhv.v1_length *. r.y;
    h_length= vhv.h_length *. r.x;
    v2_length= vhv.v2_length *. r.y;
    }
  let vht_scale ?origin ~r vht=
    let pt= point_adjust_scale ?origin ~r in
    {
      vht_start= point_scale ?origin ~r vht.vht_start;
      v_length= vht.v_length *. r.y;
      h_length= vht.h_length *. r.x;
      ctrl1= pt vht.ctrl1;
      ctrl2= pt vht.ctrl1;
      end_= point_scale ?origin ~r vht.end_;
  }
  let vhtj_scale ?origin ~r vhtj=
    let pt= point_adjust_scale ?origin ~r in
    {
      vhtj_start= point_scale ?origin ~r vhtj.vhtj_start;
      v_length= vhtj.v_length *. r.y;
      h_length= vhtj.h_length *. r.x;
      ctrl1= pt vhtj.ctrl1;
      ctrl2= pt vhtj.ctrl1;
      t_end= point_scale ?origin ~r vhtj.t_end;
      end_= pt vhtj.end_;
    }
  let vj_scale ?origin ~r vj=
    {
      vj_start= point_scale ?origin ~r vj.vj_start;
      length= vj.length *. r.y;
      end_= point_adjust_scale ?origin ~r vj.end_;
    }
  let vc_scale ?origin ~r vc= {
    vc_start= point_scale ?origin ~r vc.vc_start;
    v_length= vc.v_length *. r.y;
    h_length= vc.h_length *. r.x;
    a_radius= scalar_adjust_scale ~r:(diagonal r) vc.a_radius;
    }
  let vcj_scale ?origin ~r vcj=
    {
      vcj_start= point_scale ?origin ~r vcj.vcj_start;
      v_length= vcj.v_length *. r.y;
      h_length= vcj.h_length *. r.x;
      a_radius= scalar_adjust_scale ~r:(diagonal r) vcj.a_radius;
      end_= point_adjust_scale ?origin ~r vcj.end_;
  }
  let tu_scale ?origin ~r tu=
    let pt= point_adjust_scale ?origin ~r in
    {
      tu_start= point_scale ?origin ~r tu.tu_start;
      ctrl1= pt tu.ctrl1;
      ctrl2= pt tu.ctrl2;
      t_end= point_scale ?origin ~r tu.t_end;
      end_= pt tu.end_;
    }
  let th_scale ?origin ~r th=
    let pt= point_adjust_scale ?origin ~r in
    {
      th_start= point_scale ?origin ~r th.th_start;
      ctrl1= pt th.ctrl1;
      ctrl2= pt th.ctrl2;
      t_end= point_scale ?origin ~r th.t_end;
      length= scalar_adjust_scale ~r:r.x th.length;
    }
  let td_scale ?origin ~r td=
    let pt= point_adjust_scale ?origin ~r in
    {
      td_start= point_scale ?origin ~r td.td_start;
      ctrl1= pt td.ctrl1;
      ctrl2= pt td.ctrl2;
      t_end= point_scale ?origin ~r td.t_end;
      end_= pt td.end_;
    }
  let wtd_scale ?origin ~r wtd=
    let pt= point_adjust_scale ?origin ~r in
    {
      wtd_start= point_scale ?origin ~r wtd.wtd_start;
      v_length= scalar_adjust_scale ~r:r.y wtd.v_length;
      ctrl1= pt wtd.ctrl1;
      ctrl2= pt wtd.ctrl2;
      t_end= point_scale ?origin ~r wtd.t_end;
      end_= pt wtd.end_;
    }
  let tht_scale ?origin ~r tht=
    let pt= point_adjust_scale ?origin ~r in
    {
      tht_start= point_scale ?origin ~r tht.tht_start;
      t1_ctrl1= pt tht.t1_ctrl1;
      t1_ctrl2= pt tht.t1_ctrl2;
      t1_end= point_scale ?origin ~r tht.t1_end;
      length= tht.length *. r.x;
      t2_ctrl1= pt tht.t2_ctrl1;
      t2_ctrl2= pt tht.t2_ctrl2;
      end_= point_scale ?origin ~r tht.end_;
    }
  let thtj_scale ?origin ~r thtj=
    let pt= point_adjust_scale ?origin ~r in
    {
      thtj_start= point_scale ?origin ~r thtj.thtj_start;
      t1_ctrl1= pt thtj.t1_ctrl1;
      t1_ctrl2= pt thtj.t1_ctrl2;
      t1_end= point_scale ?origin ~r thtj.t1_end;
      length= thtj.length *. r.x;
      t2_ctrl1= pt thtj.t2_ctrl1;
      t2_ctrl2= pt thtj.t2_ctrl2;
      t2_end= point_scale ?origin ~r thtj.t2_end;
      end_= pt thtj.end_;
    }
  let tj_scale ?origin ~r tj=
    let pt= point_adjust_scale ?origin ~r in
    {
      tj_start= point_scale ?origin ~r tj.tj_start;
      ctrl1= pt tj.ctrl1;
      ctrl2= pt tj.ctrl2;
      t_end= point_scale ?origin ~r tj.t_end;
      end_= pt tj.end_;
    }
  let cj_scale ?origin ~r cj=
    let pt= point_adjust_scale ?origin ~r in
    {
      cj_start= cj.cj_start;
      ctrl1= pt cj.ctrl1;
      ctrl2= pt cj.ctrl2;
      t_end= point_scale ?origin ~r cj.t_end;
      end_= pt cj.end_;
    }
  let fpj_scale ?origin ~r fpj=
    let pt= point_adjust_scale ?origin ~r in
    {
      fpj_start= point_scale ?origin ~r fpj.fpj_start;
      ctrl1= pt fpj.ctrl1;
      ctrl2= pt fpj.ctrl2;
      t_end= point_scale ?origin ~r fpj.t_end;
      end_= pt fpj.end_;
    }
  let pj_scale ?origin ~r pj=
    let pt= point_adjust_scale ?origin ~r in
    {
      pj_start= point_scale ?origin ~r pj.pj_start;
      ctrl1= pt pj.ctrl1;
      ctrl2= pt pj.ctrl2;
      t_end= point_scale ?origin ~r pj.t_end;
      end_= pt pj.end_;
    }
  let thtaj_scale ?origin ~r thtaj=
    let pt= point_adjust_scale ?origin ~r in
    {
      thtaj_start= point_scale ?origin ~r thtaj.thtaj_start;
      ctrl1= pt thtaj.ctrl1;
      ctrl2= pt thtaj.ctrl1;
      t1_end= point_scale ?origin ~r thtaj.t1_end;
      h1_length= thtaj.h1_length *. r.x;
      t2_end= point_scale ?origin ~r thtaj.t2_end;
      h2_length= thtaj.h2_length *. r.x;
      a_radius= scalar_adjust_scale ~r:(diagonal r) thtaj.a_radius;
      end_= pt thtaj.end_;
    }
  let tod_scale ?origin ~r tod=
    let pt= point_adjust_scale ?origin ~r in
    {
      tod_start= tod.tod_start;
      ctrl1= pt tod.ctrl1;
      ctrl2= scalar_adjust_scale ~r:r.x tod.ctrl2;
      bottom= point_scale ?origin ~r tod.bottom;
      ctrl3= scalar_adjust_scale ~r:r.x tod.ctrl3;
      ctrl4= scalar_adjust_scale ~r:r.y tod.ctrl4;
      left= point_scale ?origin ~r tod.left;
      end_= pt tod.end_;
    }

  let translate ~d stroke=
    match stroke with
    | H h-> H (h_translate ~d h)
    | Sh sh-> Sh (sh_translate ~d sh)
    | U u-> U (u_translate ~d u)
    | Du du-> Du (du_translate ~d du)
    | V v-> V (v_translate ~d v)
    | Sv sv-> Sv (sv_translate ~d sv)
    | Rsv rsv-> Rsv (rsv_translate ~d rsv)
    | T t'-> T (t'_translate ~d t')
    | Ft ft-> Ft (ft_translate ~d ft)
    | Wt wt-> Wt (wt_translate ~d wt)
    | D d'-> D (d_translate ~d d')
    | Ed ed-> Ed (ed_translate ~d ed)
    | Ld ld-> Ld (ld_translate ~d ld)
    | Wd wd-> Wd (wd_translate ~d wd)
    | P p-> P (p_translate ~d p)
    | Up up-> Up (up_translate ~d up)
    | Hp hp-> Hp (hp_translate ~d hp)
    | Fp fp-> Fp (fp_translate ~d fp)
    | Ufp ufp-> Ufp (ufp_translate ~d ufp)
    | C c-> C (c_translate ~d c)
    | A a-> A (a_translate ~d a)
    | O o-> O (o_translate ~d o)
    | Hj hj-> Hj (hj_translate ~d hj)
    | Uj uj-> Uj (uj_translate ~d uj)
    | Ht ht-> Ht (ht_translate ~d ht)
    | Hsv hsv-> Hsv (hsv_translate ~d hsv)
    | Hv hv-> Hv (hv_translate ~d hv)
    | Hvj hvj-> Hvj (hvj_translate ~d hvj)
    | Htj htj-> Htj (htj_translate ~d htj)
    | Utj utj-> Utj (utj_translate ~d utj)
    | Hvh hvh-> Hvh (hvh_translate ~d hvh)
    | Hvu hvu-> Hvu (hvu_translate ~d hvu)
    | Ha ha-> Ha (ha_translate ~d ha)
    | Haj haj-> Haj (haj_translate ~d haj)
    | Hpj hpj-> Hpj (hpj_translate ~d hpj)
    | Htaj htaj-> Htaj (htaj_translate ~d htaj)
    | Htc htc-> Htc (htc_translate ~d htc)
    | Htht htht-> Htht (htht_translate ~d htht)
    | Htcj htcj-> Htcj (htcj_translate ~d htcj)
    | Hvhv hvhv-> Hvhv (hvhv_translate ~d hvhv)
    | Hthtj hthtj-> Hthtj (hthtj_translate ~d hthtj)
    | Vu vu-> Vu (vu_translate ~d vu)
    | Vh vh-> Vh (vh_translate ~d vh)
    | Va va-> Va (va_translate ~d va)
    | Vaj vaj-> Vaj (vaj_translate ~d vaj)
    | Vhv vhv-> Vhv (vhv_translate ~d vhv)
    | Vht vht-> Vht (vht_translate ~d vht)
    | Vhtj vhtj-> Vhtj (vhtj_translate ~d vhtj)
    | Vj vj-> Vj (vj_translate ~d vj)
    | Vc vc-> Vc (vc_translate ~d vc)
    | Vcj vcj-> Vcj (vcj_translate ~d vcj)
    | Tu tu-> Tu (tu_translate ~d tu)
    | Th th-> Th (th_translate ~d th)
    | Td td-> Td (td_translate ~d td)
    | Wtd wtd-> Wtd (wtd_translate ~d wtd)
    | Tht tht-> Tht (tht_translate ~d tht)
    | Thtj thtj-> Thtj (thtj_translate ~d thtj)
    | Tj tj-> Tj (tj_translate ~d tj)
    | Cj cj-> Cj (cj_translate ~d cj)
    | Fpj fpj-> Fpj (fpj_translate ~d fpj)
    | Pj pj-> Pj (pj_translate ~d pj)
    | Thtaj thtaj-> Thtaj (thtaj_translate ~d thtaj)
    | Tod tod-> Tod (tod_translate ~d tod)

  let scale ?origin ~r stroke=
    match stroke with
    | H h-> H (h_scale ?origin ~r h)
    | Sh sh-> Sh (sh_scale ?origin ~r sh)
    | U u-> U (u_scale ?origin ~r u)
    | Du du-> Du (du_scale ?origin ~r du)
    | V v-> V (v_scale ?origin ~r v)
    | Sv sv-> Sv (sv_scale ?origin ~r sv)
    | Rsv rsv-> Rsv (rsv_scale ?origin ~r rsv)
    | T t'-> T (t'_scale ?origin ~r t')
    | Ft ft-> Ft (ft_scale ?origin ~r ft)
    | Wt wt-> Wt (wt_scale ?origin ~r wt)
    | D d'-> D (d_scale ?origin ~r d')
    | Ed ed-> Ed (ed_scale ?origin ~r ed)
    | Ld ld-> Ld (ld_scale ?origin ~r ld)
    | Wd wd-> Wd (wd_scale ?origin ~r wd)
    | P p-> P (p_scale ?origin ~r p)
    | Up up-> Up (up_scale ?origin ~r up)
    | Hp hp-> Hp (hp_scale ?origin ~r hp)
    | Fp fp-> Fp (fp_scale ?origin ~r fp)
    | Ufp ufp-> Ufp (ufp_scale ?origin ~r ufp)
    | C c-> C (c_scale ?origin ~r c)
    | A a-> A (a_scale ?origin ~r a)
    | O o-> O (o_scale ?origin ~r o)
    | Hj hj-> Hj (hj_scale ?origin ~r hj)
    | Uj uj-> Uj (uj_scale ?origin ~r uj)
    | Ht ht-> Ht (ht_scale ?origin ~r ht)
    | Hsv hsv-> Hsv (hsv_scale ?origin ~r hsv)
    | Hv hv-> Hv (hv_scale ?origin ~r hv)
    | Hvj hvj-> Hvj (hvj_scale ?origin ~r hvj)
    | Htj htj-> Htj (htj_scale ?origin ~r htj)
    | Utj utj-> Utj (utj_scale ?origin ~r utj)
    | Hvh hvh-> Hvh (hvh_scale ?origin ~r hvh)
    | Hvu hvu-> Hvu (hvu_scale ?origin ~r hvu)
    | Ha ha-> Ha (ha_scale ?origin ~r ha)
    | Haj haj-> Haj (haj_scale ?origin ~r haj)
    | Hpj hpj-> Hpj (hpj_scale ?origin ~r hpj)
    | Htaj htaj-> Htaj (htaj_scale ?origin ~r htaj)
    | Htc htc-> Htc (htc_scale ?origin ~r htc)
    | Htht htht-> Htht (htht_scale ?origin ~r htht)
    | Htcj htcj-> Htcj (htcj_scale ?origin ~r htcj)
    | Hvhv hvhv-> Hvhv (hvhv_scale ?origin ~r hvhv)
    | Hthtj hthtj-> Hthtj (hthtj_scale ?origin ~r hthtj)
    | Vu vu-> Vu (vu_scale ?origin ~r vu)
    | Vh vh-> Vh (vh_scale ?origin ~r vh)
    | Va va-> Va (va_scale ?origin ~r va)
    | Vaj vaj-> Vaj (vaj_scale ?origin ~r vaj)
    | Vhv vhv-> Vhv (vhv_scale ?origin ~r vhv)
    | Vht vht-> Vht (vht_scale ?origin ~r vht)
    | Vhtj vhtj-> Vhtj (vhtj_scale ?origin ~r vhtj)
    | Vj vj-> Vj (vj_scale ?origin ~r vj)
    | Vc vc-> Vc (vc_scale ?origin ~r vc)
    | Vcj vcj-> Vcj (vcj_scale ?origin ~r vcj)
    | Tu tu-> Tu (tu_scale ?origin ~r tu)
    | Th th-> Th (th_scale ?origin ~r th)
    | Td td-> Td (td_scale ?origin ~r td)
    | Wtd wtd-> Wtd (wtd_scale ?origin ~r wtd)
    | Tht tht-> Tht (tht_scale ?origin ~r tht)
    | Thtj thtj-> Thtj (thtj_scale ?origin ~r thtj)
    | Tj tj-> Tj (tj_scale ?origin ~r tj)
    | Cj cj-> Cj (cj_scale ?origin ~r cj)
    | Fpj fpj-> Fpj (fpj_scale ?origin ~r fpj)
    | Pj pj-> Pj (pj_scale ?origin ~r pj)
    | Thtaj thtaj-> Thtaj (thtaj_scale ?origin ~r thtaj)
    | Tod tod-> Tod (tod_scale ?origin ~r tod)
end
