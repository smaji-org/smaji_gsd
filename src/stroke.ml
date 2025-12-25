(*
 * smaji_gsd.ml
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
  h2_length: float;
  a_radius: float adjust;
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
  c_end: point
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
  h2_length: float;
  a_radius: float adjust;
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

module To_path = struct

  module Path = GlyphPath.Path

  let circle_ctrl_distance ?(seg=4) r=
    r *. 4. /. 3. *. tan(Float.pi /. (2. *. (Float.of_int seg)))

  (*
    type h= {
      h_start: point;
      length: float;
    } (* Horizontal *)
      (* 1. the length is at least 4 times its weight *)
  *)

  (* downward curve, e.g. t or p
     [ratio] is based on the length between [start] and [end']
   *)
  let template_curve ~start ~end' ~ratio=
    let vec, length=
      let open Point in
      let open Float in
      let vec= end' - start in
      let length= (pow vec.x 2.) +. (pow vec.y 2.) |> sqrt in
      (vec, length)
    in
    let open Point in
    let angle= angle vec -. angle {x=0.;y=1.} in
    let rotate= rotate ~angle in
    let point= rotate (ratio <* length) in
    start + point

  let from_h h=
    let open Path in
    let start= h.h_start in
    let segments= [Line {start with x= start.x +. h.length}] in
    {
      start;
      segments;
    }

  (*
    type sh= {
      sh_start: point;
      end_: point;
    } (* Slanted Horizontal *)
      (* 1. the length is at least 4 times its weight *)
      (* 2. the angle is limited in 10 degrees *)
  *)

  let from_sh sh=
    let open Path in
    let start= sh.sh_start in
    let segments= [Line sh.end_] in
    {
      start;
      segments;
    }

  (*
    type u= {
      u_start: point;
      end_: point;
    } (* Upward horizontal *)
      (* 1. the length is at least 4 times its weight *)
      (* 2. the angle is limited between 10 to 80 degrees *)
  *)

  (* pointed *)
  let from_u u=
    let open Path in
    let start= u.u_start in
    let segments= [Line u.end_] in
    {
      start;
      segments;
    }

  (*
    type du= {
      du_start: point;
      dot: point adjust;
      end_: point;
    } (* Dot – Upward horizontal *)
      (* 1. the length is at least 4 times its weight *)
      (* 2. the angle is limited between 10 to 45 degrees *)
  *)

  let from_du ?(weight=8.) du=
    let open Path in
    let start= du.du_start in
    let dot= get_adjust du.dot
      ~f:(fun ()-> Point.{x= start.x +. weight; y= start.y +. 2. *. weight})
    in
    let segments= [Line dot; Line du.end_] in
    {
      start;
      segments;
    }

  (*
    type v= {
      v_start: point;
      length: float;
    } (* Vertical *)
      (* 1. the length is at least 4 times its weight *)
  *)

  let from_v v=
    let open Path in
    let start= v.v_start in
    let segments= [Line {start with y= start.y +. v.length}] in
    {
      start;
      segments;
    }

  (*
    type sv= {
      sv_start: point;
      width: length;
      height: length;
    } (* Slanted Vertical *)
      (* 1. the length is at least 4 times its weight *)
      (* 2. the end_ point is to the bottom left of sv_start *)
      (* 3. the angle is limited between 5 to 30 degrees *)
  *)

  let from_sv sv=
    let open Path in
    let start= sv.sv_start in
    let end_=
      let x= sv.sv_start.x -. (Float.abs sv.width)
      and y= sv.sv_start.y +. (Float.abs sv.length) in
      Point.{ x; y }
    in
    let segments= [Line end_] in
    {
      start;
      segments;
    }

  (*
    type rsv= {
      rsv_start: point;
      end_: point;
    } (* Right Slanted Vertical *)
      (* 1. the length is at least 4 times its weight *)
      (* 2. the end_ point is to the bottom right of rsv_start *)
      (* 3. the angle is limited between 5 to 30 degrees *)
  *)

  let from_rsv rsv=
    let open Path in
    let start= rsv.rsv_start in
    let end_=
      let x= rsv.rsv_start.x +. (Float.abs rsv.width)
      and y= rsv.rsv_start.y +. (Float.abs rsv.length) in
      Point.{ x; y }
    in
    let segments= [Line end_] in
    {
      start;
      segments;
    }

  (*
    type t_= {
      t_start: point;
      ctrl1: point adjust;
      ctrl2: point adjust;
      end_: point;
    } (* Throw *)
      (* 1. the length is at least 4 times its weight *)
      (* 2. the angle is limited between 40 to 50 degrees *)
      (* 3. ctrl1 is to the bottom left of t_start *)
      (* 4. ctrl2 is to the top right of end_ *)
  *)

  let from_t' (t:t')=
    let open Path in
    let start= t.t'_start
    and end'= t.end_ in
    let calc_ctrl= template_curve ~start ~end' in
    let ctrl1= get_adjust t.ctrl1
      ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.2; y= 0.25})
    and ctrl2= get_adjust t.ctrl2
      ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.2; y= 0.75})
    in
    let segments= [Ccurve {ctrl1; ctrl2; end'}] in
    {
      start;
      segments;
    }

  (*
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
  *)

  let from_ft ft=
    let open Path in
    let width= ft.ft_start.x -. ft.end_.x
    and height= ft.end_.y -. ft.ft_start.y in
    let start= ft.ft_start
    and ctrl1= get_adjust ft.ctrl1
      ~f:(fun ()->
        let x= ft.ft_start.x -. (width /. 4.)
        and y= ft.ft_start.y +. (height /. 2.) in
        Point.{x;y})
    and ctrl2= get_adjust ft.ctrl2
      ~f:(fun ()->
        let x= ft.end_.x +. (width /. 2.)
        and y= ft.end_.y -. (height /. 4.) in
        Point.{x;y})
    and end'= ft.end_ in
    let segments= [Ccurve {ctrl1; ctrl2; end'}] in
    {
      start;
      segments;
    }

  (*
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
  *)

  let from_wt wt=
    let open Path in
    let start= wt.wt_start in
    let width= start.x -. wt.end_.x
    and height= wt.end_.y -. start.y in
    let v_length= get_adjust wt.v_length
      ~f:(fun()-> start.y +. height /. 2.) in
    let t_height= height -. v_length in
    let t_start=
      let x= start.x
      and y= start.y +. v_length in
      Point.{x;y}
    in
    let ctrl1= get_adjust wt.ctrl1
      ~f:(fun ()->
        let x= t_start.x (* start straight *)
        and y= t_start.y +. (t_height /. 2.) in
        Point.{x;y})
    and ctrl2= get_adjust wt.ctrl2
      ~f:(fun ()->
        let x= wt.end_.x +. (width /. 2.)
        and y= wt.end_.y -. (t_height /. 4.) in
        Point.{x;y})
    and end'= wt.end_ in
    let segments= [Ccurve {ctrl1; ctrl2; end'}] in
    {
      start;
      segments;
    }

  (*
    type d= {
      d_start: point;
      end_: point adjust;
    } (* Dot *)
      (* 1. the length is at least 1 times its weight *)
      (* 2. the length is at most 2 times its weight *)
      (* 5. end is to the bottom right of d_start *)
  *)

  let from_d ?(weight=8.) d=
    let open Path in
    let start= d.d_start in
    let end_= get_adjust d.end_
      ~f:(fun()->
        let x= start.x +. 2. *. weight
        and y= start.y +. 2. *. weight in
        Point.{x;y}
      )
    in
    let end'= end_ in
    let segments= [Line  end'] in
    {
      start;
      segments;
    }

  (*
    type ed= {
      ed_start: point;
      end_: point adjust;
    } (* Extended Dot *)
      (* 1. the length is at least 3 times its weight *)
      (* 2. the length is at most 5 times its weight *)
      (* 5. end is to the bottom right of d_start *)
  *)

  let from_ed ?(weight=8.) ed=
    let open Path in
    let start= ed.ed_start in
    let end_= get_adjust ed.end_
      ~f:(fun()->
        let x= start.x +. 6. *. weight
        and y= start.y +. 6. *. weight in
        Point.{x;y}
      )
    in
    let width= end_.x -. start.x
    and height= end_.y -. start.y in
    let ctrl1=
      let x= start.x +. (width /. 2.)
      and y= start.y +. (height /. 4.) in
      Point.{x;y}
    and ctrl2=
      let x= end_.x -. (width /. 4.)
      and y= end_.y -. (height /. 2.) in
      Point.{x;y}
    in
    let end'= end_ in
    let segments= [Ccurve {ctrl1; ctrl2; end'}] in
    {
      start;
      segments;
    }

  (*
    type ld= {
      ld_start: point;
      end_: point adjust;
    } (* Left Dot *)
      (* 1. the length is at least 1 times its weight *)
      (* 2. the length is at most 2 times its weight *)
      (* 5. end is to the bottom left of d_start *)
  *)

  let from_ld ?(weight=8.) ld=
    let open Path in
    let start= ld.ld_start in
    let end_= get_adjust ld.end_
      ~f:(fun()->
        let x= start.x -. 2. *. weight
        and y= start.y +. 2. *. weight in
        Point.{x;y}
      )
    in
    let end'= end_ in
    let segments= [Line  end'] in
    {
      start;
      segments;
    }

  (*
    type wd= {
      wd_start: point;
      length: float adjust;
    } (* Wilted Dot *)
      (* 1. the length is at least 1 times its weight *)
      (* 2. the length is at most 2 times its weight *)
  *)

  let from_wd ?(weight=8.) wd=
    let open Path in
    let start= wd.wd_start in
    let length= get_adjust wd.length
      ~f:(fun()-> weight *. 2.)
    in
    let end_=
      let x= start.x
      and y= start.y +. length in
      Point.{x;y}
    in
    let segments= [Line  end_] in
    {
      start;
      segments;
    }

  (*
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
  *)

  let from_p (p:p)=
    let open Path in
    let start= p.p_start
    and end'= p.end_ in
    let calc_ctrl= template_curve ~start ~end' in
    let ctrl1= get_adjust p.ctrl1
      ~f:(fun ()-> calc_ctrl ~ratio:{x= -0.2; y= 0.25})
    and ctrl2= get_adjust p.ctrl2
      ~f:(fun ()-> calc_ctrl ~ratio:{x= -0.2; y= 0.75})
    in
    let segments= [Ccurve {ctrl1; ctrl2; end'}] in
    {
      start;
      segments;
    }

  (*
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
  *)

  let from_up ?(weight=8.) (up:up)=
    let open Path in
    let p_start= up.p_start in
    let start= get_adjust up.up_start
      ~f:(fun()->
        let x= p_start.x -. 2. *. weight
        and y= p_start.y +. 2. *. weight in
        Point.{x;y})
    in
    let u= Line p_start
    and p=
      let end'= up.end_ in
      let calc_ctrl= template_curve ~start:p_start ~end' in
      let ctrl1= calc_ctrl ~ratio:{x= -0.2; y= 0.33}
      and ctrl2= calc_ctrl ~ratio:{x= -0.2; y= 0.67} in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let segments= [u; p] in
    {
      start;
      segments;
    }

  (*
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
  *)

  let from_hp hp=
    let open Path in
    let start= hp.hp_start in
    let p_start= { start with x= start.x +. hp.length } in
    let calc_ctrl= template_curve ~start:p_start ~end':hp.end_ in
    let ctrl1= get_adjust hp.ctrl1
      ~f:(fun ()-> calc_ctrl ~ratio:{x= -0.25; y= 0.5})
    and ctrl2= get_adjust hp.ctrl2
      ~f:(fun ()-> calc_ctrl ~ratio:{x= -0.2; y= 0.85})
    and end'= hp.end_ in
    let segments= [Line p_start; Ccurve {ctrl1; ctrl2; end'}] in
    {
      start;
      segments;
    }

  (*
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
  *)

  let from_fp fp=
    let open Path in
    let start= fp.fp_start in
    let width= fp.end_.x -. start.x
    and height= fp.end_.y -. start.y in
    let ctrl1= get_adjust fp.ctrl1
      ~f:(fun ()->
        let x= start.x +. (width /. 6.)
        and y= start.y +. (height *. 3. /. 4.) in
        Point.{x;y})
    and ctrl2= get_adjust fp.ctrl2
      ~f:(fun ()->
        let x= fp.end_.x -. (width *. 3. /. 4.)
        and y= fp.end_.y in
        Point.{x;y})
    and end'= fp.end_ in
    let segments= [Ccurve {ctrl1; ctrl2; end'}] in
    {
      start;
      segments;
    }

  (*
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
  *)

  let from_ufp ufp=
    let open Path in
    let start= ufp.ufp_start
    and p_start= ufp.p_start in
    let width= ufp.end_.x -. p_start.x
    and height= ufp.end_.y -. p_start.y in
    let ctrl1= get_adjust ufp.ctrl1
      ~f:(fun ()->
        let x= p_start.x +. (width /. 4.)
        and y= p_start.y +. (height *. 2. /. 3.) in
        Point.{x;y})
    and ctrl2= get_adjust ufp.ctrl2
      ~f:(fun ()->
        let x= ufp.end_.x -. (width *. 2. /. 3.)
        and y= ufp.end_.y in
        Point.{x;y})
    and end'= ufp.end_ in
    let segments= [Line p_start; Ccurve {ctrl1; ctrl2; end'}] in
    {
      start;
      segments;
    }

  (*
    type c= {
      c_start: point;
      length: float;
      width: float;
      ctrl1: float adjust;
      ctrl2: float adjust;
    } (* Clockwise curve *)
  *)

  let from_c c=
    let open Path in
    let start= c.c_start in
    let right= Point.{ x= start.x +. c.width; y= start.y +. c.length /. 2. } in
    let end'=  { start with y= start.y +. c.length } in
    let distance_h= circle_ctrl_distance ~seg:4  c.width
    and distance_v= circle_ctrl_distance ~seg:4  (c.length /. 2.) in
    let curve1=
      let ctrl1= { start with x= start.x +. distance_h }
      and ctrl2= { right with y= right.y -. distance_v }
      and end'= right in
      Ccurve {ctrl1; ctrl2; end'}
    and curve2=
      let ctrl1= { right with y= right.y +. distance_v }
      and ctrl2= { end' with x= end'.x +. distance_h }
      and end'= end' in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let segments= [curve1; curve2] in
    {
      start;
      segments;
    }

  (*
    type a= {
      a_start: point;
      length: float;
      width: float;
      ctrl1: float adjust;
      ctrl2: float adjust;
      end_: point;
    } (* Anticlockwise curve *)
  *)

  let from_a a=
    let open Path in
    let start= a.a_start in
    let left= Point.{ x= start.x -. a.width; y= start.y +. a.length /. 2. } in
    let end'=  { start with y= start.y +. a.length } in
    let distance_h= circle_ctrl_distance ~seg:4  a.width
    and distance_v= circle_ctrl_distance ~seg:4  (a.length /. 2.) in
    let curve1=
      let ctrl1= { start with x= start.x -. distance_h }
      and ctrl2= { left with y= left.y -. distance_v }
      and end'= left in
      Ccurve {ctrl1; ctrl2; end'}
    and curve2=
      let ctrl1= { left with y= left.y +. distance_v }
      and ctrl2= { end' with x= end'.x -. distance_h }
      and end'= end' in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let segments= [curve1; curve2] in
    {
      start;
      segments;
    }

  (*
    type o= {
      o_start: point;
      length: float;
      width: float;
      ctrl_h: float adjust;
      ctrl_v: float adjust;
    } (* Oval *)
  *)

  let from_o o=
    let open Path in
    let start= o.o_start in
    let up= start in
    let down= { up with y= up.y +. o.length } in
    let right= Point.{
      x= up.x +. o.width /. 2.;
      y= up.y +. o.length /. 2.
    } in
    let left= { right with x= right.x -. o.width } in
    let distance_h= circle_ctrl_distance ~seg:4  (o.width /. 2.)
    and distance_v= circle_ctrl_distance ~seg:4  (o.length /. 2.) in
    let curve1=
      let ctrl1= { up with x= up.x +. distance_h }
      and ctrl2= { right with y= right.y -. distance_v }
      and end'= right in
      Ccurve {ctrl1; ctrl2; end'}
    and curve2=
      let ctrl1= { right with y= right.y +. distance_v }
      and ctrl2= { down with x= down.x +. distance_h }
      and end'= down in
      Ccurve {ctrl1; ctrl2; end'}
    and curve3=
      let ctrl1= { down with x= down.x -. distance_h }
      and ctrl2= { left with y= left.y +. distance_v }
      and end'= left in
      Ccurve {ctrl1; ctrl2; end'}
    and curve4=
      let ctrl1= { left with y= left.y -. distance_v }
      and ctrl2= { up with x= up.x -. distance_h }
      and end'= left in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let segments= [curve1; curve2; curve3; curve4] in
    {
      start;
      segments;
    }

  (*
    type hj= {
      hj_start: point;
      length: float;
      end_: point adjust;
    } (* Horizontal – J hook *)
  *)

  let from_hj hj=
    let open Path in
    let start= hj.hj_start in
    let segments=
      let h_end= { start with x= start.x +. hj.length } in
      let h= Line h_end
      and j=
        (* 45 degree *)
        let end_= get_adjust hj.end_ ~f:(fun()->
          let d= Float.pow (hj.length /. 4.) 2. /. 2. |> sqrt in
          Point.{ x= h_end.x -. d; y= h_end.y +. d })
        in
        Line end_
      in
      [h;j]
    in
    {
      start;
      segments;
    }

  (*
    type uj= {
      uj_start: point;
      u_end: point;
      end_: point adjust;
    } (* Upward horizontal – J hook *)
  *)

  let from_uj uj=
    let open Path in
    let start= uj.uj_start in
    let segments=
      let h= Line uj.u_end
      and j=
        (* 60 degree *)
        let end_= get_adjust uj.end_ ~f:(fun()->
          template_curve ~start:uj.u_end ~end':start ~ratio:{x= 0.3; y= 0.3}
        )
        in
        Line end_
      in
      [h;j]
    in
    {
      start;
      segments;
    }

  (*
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
  *)

  let from_ht ht=
    let open Path in
    let start= ht.ht_start in
    let t_start= { start with x= start.x +. ht.length } in
    let calc_ctrl= template_curve ~start:t_start ~end':ht.end_ in
    let ctrl1= get_adjust ht.ctrl1
      ~f:(fun()-> calc_ctrl ~ratio:{x= 0.2; y= 0.3})
    and ctrl2= get_adjust ht.ctrl2
      ~f:(fun()-> calc_ctrl ~ratio:{x= 0.2; y= 0.7})
    and end'= ht.end_ in
    let segments= [
      Line t_start;
      Ccurve {ctrl1; ctrl2; end'}
      ] in
    {
      start;
      segments;
    }

  (*
    type hsv= {
      hsv_start: point;
      h_length: float;
      end_: point adjust;
    } (* Horizontal – Slanted Vertical *)
      (* 1. the h_length is at least 4 times its weight *)
      (* 2. the angle is limited between 40 to 85 degrees *)
      (* 3. the length of the sv stroke is at least 4 times its weight *)
  *)

  let from_hsv hsv=
    let start= hsv.hsv_start in
    let sv_start= { start with x= start.x +. hsv.h_length } in
    let end_= get_adjust hsv.end_ ~f:(fun ()->
      (* 60 degree *)
      let d=
        ( -. )
          (Float.pow hsv.h_length 2.)
          (Float.pow (hsv.h_length /. 2.) 2.)
        |> sqrt
      in
      Point.{
        x= sv_start.x -. hsv.h_length /. 2.;
        y= sv_start.y +. d;
      }
    )
    in
    let open Path in
    let segments= [
      Line sv_start;
      Line end_;
      ] in
    {
      start;
      segments;
    }

  (*
    type hv= {
      hv_start: point;
      h_length: float;
      v_length: float adjust;
    } (* Horizontal – Vertical *)
      (* 1. the h_length is at least 4 times its weight *)
      (* 2. the v_length is at least 4 times its weight *)
  *)

  let from_hv hv=
    let start= hv.hv_start in
    let v_length= get_adjust hv.v_length ~f:(fun ()-> hv.h_length) in
    let h_end= { start with x= start.x +. hv.h_length } in
    let v_end= { h_end with y= h_end.y +. v_length } in
    let open Path in
    let segments= [
      Line h_end;
      Line v_end;
      ] in
    {
      start;
      segments;
    }

  (*
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
  *)

  let from_hvj hvj=
    let start= hvj.hvj_start in
    let v_length= get_adjust hvj.v_length ~f:(fun ()-> hvj.h_length) in
    let d= Float.pow (v_length /. 4.) 2. /. 2. |> sqrt in
    let h_end= { start with x= start.x +. hvj.h_length } in
    let v_end= { h_end with y= h_end.y +. v_length } in
    (* 45 degree *)
    let j_end= get_adjust hvj.end_
      ~f:(fun ()-> Point.{ x= v_end.x -. d; y= v_end.y -. d }) in
    let open Path in
    let segments= [
      Line h_end;
      Line v_end;
      Line j_end;
      ] in
    {
      start;
      segments;
    }

  (*
    type htj= {
      htj_start: point;
      length: float;
      ctrl1: point adjust;
      ctrl2: point adjust;
      t_end: point;
      end_: point adjust;
    } (* Horizontal – Throw – J hook *)
      (* 1. end_ is to the top left of t_end *)
  *)

  let from_htj htj=
    let open Path in
    let start= htj.htj_start
    and length= htj.length in
    let t_start= { start with x= start.x +. length }
    and t_end= htj.t_end in
    let h= Line t_start in
    let t=
      let start= t_start
      and end'= t_end in
      let calc_ctrl= template_curve ~start ~end' in
      let ctrl1= calc_ctrl ~ratio:{x= 0.1; y= 0.5}
      and ctrl2= calc_ctrl ~ratio:{x= 0.15; y= 0.8} in
      Ccurve { ctrl1; ctrl2; end' }
    in
    let j=
      let end_= get_adjust htj.end_ ~f:(fun ()->
        let open Float in
        let open Point in
        let line= t_end - t_start in
        let d= pow line.x 2. +. pow line.y 2. |> sqrt in
        Point.{ x= t_end.x -. d/.8.; y= t_end.y -. d/.8. })
      in
      Line end_;
    in
    let segments= [ h; t; j; ] in
    {
      start;
      segments;
    }

  (*
    type utj= {
      utj_start: point;
      t_start: point;
      ctrl1: point adjust;
      ctrl2: point adjust;
      t_end: point;
      end_: point adjust;
    } (* Upward horizontal – Throw – J hook *)
      (* 1. end_ is to the top left of t_end *)
  *)

  let from_utj utj=
    let open Path in
    let start= utj.utj_start
    and t_start= utj.t_start
    and t_end= utj.t_end in
    let length=
      ( +. )
        (Float.pow (t_start.x -. start.x) 2.)
        (Float.pow (t_start.y -. start.y) 2.)
      |> sqrt
    in
    let u= Line t_start in
    let t=
      let end'= t_end in
      let calc_ctrl= template_curve ~start:t_start ~end' in
      let ctrl1= calc_ctrl ~ratio:{x= 0.15; y=0.6}
      and ctrl2= calc_ctrl ~ratio:{x= 0.2; y=0.8} in
      Ccurve {ctrl1; ctrl2; end'}
    and j=
      let t_d= Float.pow (length /. 8.) 2. /. 2. |> sqrt in
      let end_= get_adjust utj.end_
        ~f:(fun ()-> Point.{ x= t_end.x -. t_d; y= t_end.y -. t_d }) in
      Line end_;
    in
    let segments= [u; t; j] in
    {
      start;
      segments;
    }

  (*
    type hvh= {
      hvh_start: point;
      h1_length: float;
      v_length: float;
      h2_length: float;
    } (* Horizontal – Vertical – Horizontal *)
  *)

  let from_hvh hvh=
    let start= hvh.hvh_start in
    let h1_end= { start with x= start.x +. hvh.h1_length } in
    let v_end= { h1_end with y= h1_end.y +. hvh.v_length } in
    let h2_end= { v_end with x= v_end.x +. hvh.h2_length } in
    let open Path in
    let segments= [
      Line h1_end;
      Line v_end;
      Line h2_end;
      ] in
    {
      start;
      segments;
    }

  (*
    type hvu= {
      hvu_start: point;
      h_length: float;
      v_length: float;
      end_: point adjust;
    } (* Horizontal – Vertical – Upward horizontal *)
  *)

  let from_hvu hvu=
    let start= hvu.hvu_start in
    let h_end= { start with x= start.x +. hvu.h_length } in
    let v_end= { h_end with y= h_end.y +. hvu.v_length } in
    let u_end= get_adjust hvu.end_ ~f:(fun ()->
      let d= Float.pow (hvu.v_length /. 3.) 2. /. 2. |> sqrt in
      Point.{x= v_end.x +. d; y= v_end.y -. d}
      ) in
    let open Path in
    let segments= [
      Line h_end;
      Line v_end;
      Line u_end;
      ] in
    {
      start;
      segments;
    }

  (*
    type ha= {
      ha_start: point;
      h1_length: float;
      v_length: float;
      h2_length: float;
      a_radius: float adjust;
    } (* Horizontal – Anticlockwise curve *)
  *)

  let from_ha (ha:ha)=
    let a_radius= get_adjust ha.a_radius ~f:(fun ()->
      let avg= (ha.h1_length +. ha.v_length +. ha.h2_length) /. 3. in
      avg /. 8.)
    in
    let a_d= circle_ctrl_distance ~seg:4 a_radius in
    let start= ha.ha_start in
    let h1_end= { start with x= start.x +. ha.h1_length } in
    let v_end= { h1_end with y= h1_end.y +. ha.v_length } in
    let h2_start= Point.{
      x= v_end.x +. a_radius;
      y= v_end.y +. a_radius;
    } in
    let h2_end= { h2_start with x= h2_start.x +. ha.h2_length } in
    let open Path in
    let curve=
      let ctrl1= { v_end with y= v_end.y +. a_d }
      and ctrl2= { h2_start with x= h2_start.x -. a_d }
      and end'= h2_start in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let segments= [
      Line h1_end;
      Line v_end;
      curve;
      Line h2_end;
    ] in
    {
      start;
      segments;
    }

  (*
    type haj= {
      haj_start: point;
      h1_length: float;
      v_length: float;
      h2_length: float;
      a_radius: float adjust;
      end_: point adjust;
    } (* Horizontal – Anticlockwise curve – J hook *)
  *)

  let from_haj (haj:haj)=
    let ha= {
      ha_start= haj.haj_start;
      h1_length= haj.h1_length;
      v_length= haj.v_length;
      h2_length= haj.h2_length;
      a_radius= haj.a_radius;
    }
    in
    let open Path in
    let path= from_ha ha in
    let end_= get_adjust haj.end_ ~f:(fun ()->
      let d= haj.h2_length /. 4. in
      let last= get_end path in
      { last with y= last.y -. d })
    in
    let segments= path.segments |> List.rev
      |> List.cons (Line end_) |> List.rev in
    { path with segments }

  (*
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
  *)

  (*
  let from_hpj hpj=
    let open Path in
    let start= hpj.hpj_start
    and length= hpj.length in
    let p_start= { start with x= start.x +. length }
    and p_end= hpj.p_end in
    let height= p_end.y -. p_start.y in
    let start= p_start in
    let p=
      let ctrl1= get_adjust hpj.ctrl1
        ~f:(fun ()->
          let x= p_start.x
          and y= p_start.y +. (height /. 2.) in
          Point.{x;y})
      and ctrl2= get_adjust hpj.ctrl2
        ~f:(fun ()->
          let x= p_end.x -. (length /. 4.)
          and y= p_end.y -. (height /. 4.) in
          Point.{x;y})
      and end'= hpj.p_end in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let end_= get_adjust hpj.end_
      ~f:(fun ()-> { p_end with y= p_end.y -. height /. 4. }) in
    let segments= [
      Line p_start;
      p;
      Line end_;
      ] in
    {
      start;
      segments;
    }
  *)

  let from_hpj hpj=
    let open Path in
    let hp= {
      hp_start= hpj.hpj_start;
      length= hpj.length;
      ctrl1= hpj.ctrl1;
      ctrl2= hpj.ctrl2;
      end_= hpj.p_end;
    } in
    let hp= from_hp hp in
    let j=
      let height= hpj.p_end.y -. hpj.hpj_start.y in
      let end_= get_adjust hpj.end_ ~f:(fun ()->
        { hpj.p_end with y= hpj.p_end.y -. height /. 4. })
      in
      Path.Line end_
    in
    { hp with segments= hp.segments @ [j] }

  (*
    type htaj= {
      htaj_start: point;
      h1_length: float;
      t_end: point;
      h2_length: float;
      a_radius: float adjust;
      end_: point adjust;
    } (* Horizontal – Throw – Anticlockwise curve – J hook *)
  *)

  let from_htaj (htaj:htaj)=
    let a_radius= get_adjust htaj.a_radius ~f:(fun ()->
      let avg= (htaj.h1_length +. htaj.h2_length) /. 2. in
      avg /. 8.)
    in
    let start= htaj.htaj_start in
    let h1_end= { start with x= start.x +. htaj.h1_length } in
    let t_end= htaj.t_end in
    let h2_start= { t_end with
      y= t_end.y +. a_radius *. 2.;
    } in
    let h2_end= { h2_start with x= h2_start.x +. htaj.h2_length } in
    let open Path in
    let curve=
      let vec= Point.(t_end - h1_end) in
      let a_d= a_radius in
      let unit=
        Float.pow a_d 2.
          /. (1. +. Float.pow (vec.y /. vec.x) 2.)
          |> sqrt
      in
      let ctrl1=
        let dx= unit and dy= (vec.y /. vec.x) *. unit |> Float.abs in
        Point.{ x= t_end.x -. dx; y= t_end.y +. dy }
      and ctrl2= { h2_start with x= h2_start.x -. a_d }
      and end'= h2_start in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let end_= get_adjust htaj.end_ ~f:(fun ()->
      let d= htaj.h2_length /. 4. in
      { h2_end with y= h2_end.y -. d })
    in
    let segments= [
      Line h1_end;
      Line t_end;
      curve;
      Line h2_end;
      Line end_;
    ] in
    {
      start;
      segments;
    }

  (*
  let from_htaj (htaj:htaj)=
    let a_radius= get_adjust htaj.a_radius ~f:(fun ()->
      let avg= (htaj.h1_length +. htaj.h2_length) /. 2. in
      avg /. 8.)
    in
    let a_d= circle_ctrl_distance ~seg:4 a_radius in
    let start= htaj.htaj_start in
    let h1_end= { start with x= start.x +. htaj.h1_length } in
    let t_end= htaj.t_end in
    let h2_start= Point.{
      x= t_end.x +. a_radius;
      y= t_end.y +. a_radius;
    } in
    let h2_end= { h2_start with x= h2_start.x +. htaj.h2_length } in
    let open Path in
    let curve=
      let vec= Point.(t_end - h1_end) in
      let unit=
        Float.pow a_d 2.
          /. (1. +. Float.pow (vec.y /. vec.x) 2.)
          |> sqrt
      in
      let ctrl1=
        let dx= unit and dy= (vec.y /. vec.x) *. unit |> Float.abs in
        Point.{ x= t_end.x -. dx; y= t_end.y +. dy }
      and ctrl2= { h2_start with x= h2_start.x -. a_d }
      and end'= h2_start in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let end_= get_adjust htaj.end_ ~f:(fun ()->
      let d= htaj.h2_length /. 4. in
      { h2_end with y= h2_end.y -. d })
    in
    let segments= [
      Line h1_end;
      Line t_end;
      curve;
      Line h2_end;
      Line end_;
    ] in
    {
      start;
      segments;
    }
  *)

  (*
    type htc= {
      htc_start: point;
      h_length: float;
      t_ctrl1: point adjust;
      t_ctrl2: point adjust;
      t_end: point;
      c_length: float;
      c_width: float;
      c_ctrl1: point adjust;
      c_ctrl2: point adjust;
    } (* Horizontal – Throw – Clockwise curve *)
  *)

  let from_htc htc=
    let open Path in
    let start= htc.htc_start in
    let t_start= { start with x= start.x +. htc.h_length } in
    let h= Line t_start in
    let t=
      let start= t_start and end'= htc.t_end in
      let calc_ctrl= template_curve ~start ~end' in
      let ctrl1= get_adjust htc.t_ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.1; y= 0.3})
      and ctrl2= get_adjust htc.t_ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.1; y= 0.7})
      in
      Ccurve {ctrl1; ctrl2; end'} in
    let c=
      let start= htc.t_end and end'= htc.c_end in
      let calc_ctrl= template_curve ~start ~end' in
      let ctrl1= get_adjust htc.c_ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.5; y= 0.2})
      and ctrl2= get_adjust htc.c_ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.5; y= 0.8})
      in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let segments= h :: t :: c :: [] in
    {
      start;
      segments;
    }

  (*
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
  *)

  let from_htht htht=
    let open Path in
    let h1, t1=
      let h1_start= htht.htht_start in
      let t_start= { h1_start with x= h1_start.x +. htht.h1_length } in
      let width= t_start.x -. htht.t1_end.x
      and height= htht.t1_end.y -. t_start.y in
      let ctrl1= get_adjust htht.t1_ctrl1
        ~f:(fun ()->
          let x= t_start.x -. (width /. 4.)
          and y= t_start.y +. (height /. 2.) in
          Point.{x;y})
      and ctrl2= get_adjust htht.t1_ctrl2
        ~f:(fun ()->
          let x= htht.t1_end.x +. (width /. 2.)
          and y= htht.t1_end.y -. (height /. 4.) in
          Point.{x;y})
      and end'= htht.t1_end in
      (Line t_start, Ccurve {ctrl1; ctrl2; end'})
    and h2, t2=
      let h2_start= htht.t1_end in
      let t_start= { h2_start with x= h2_start.x +. htht.h2_length } in
      let width= t_start.x -. htht.end_.x
      and height= htht.end_.y -. t_start.y in
      let ctrl1= get_adjust htht.t2_ctrl1
        ~f:(fun ()->
          let x= t_start.x -. (width /. 4.)
          and y= t_start.y +. (height /. 2.) in
          Point.{x;y})
      and ctrl2= get_adjust htht.t2_ctrl2
        ~f:(fun ()->
          let x= htht.end_.x +. (width /. 2.)
          and y= htht.end_.y -. (height /. 4.) in
          Point.{x;y})
      and end'= htht.end_ in
      (Line t_start, Ccurve {ctrl1; ctrl2; end'})
    in
    let start= htht.htht_start
    and segments= [h1;t1;h2;t2] in
    {
      start;
      segments;
    }

  (*
    type htcj= {
      htcj_start: point;
      h_length: float;
      t_ctrl1: point adjust;
      t_ctrl2: point adjust;
      t_end: point;
      c_length: float;
      c_width: float;
      c_ctrl1: point adjust;
      c_ctrl2: point adjust;
      end_: point adjust;
    } (* Horizontal – Throw – Clockwise curve – J hook *)
  *)

  let from_htcj htcj=
    let open Path in
    let start= htcj.htcj_start in
    let t_start= { start with x= start.x +. htcj.h_length } in
    let h= Line t_start in
    let t=
      let start= t_start and end'= htcj.t_end in
      let calc_ctrl= template_curve ~start ~end' in
      let ctrl1= get_adjust htcj.t_ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.1; y= 0.3})
      and ctrl2= get_adjust htcj.t_ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.1; y= 0.7})
      in
      Ccurve {ctrl1; ctrl2; end'} in
    let c=
      let start= htcj.t_end and end'= htcj.c_end in
      let calc_ctrl= template_curve ~start ~end' in
      let ctrl1= get_adjust htcj.c_ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.35; y= 0.5})
      and ctrl2= get_adjust htcj.c_ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.25; y= 0.9})
      in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let j=
      let end_= get_adjust htcj.end_ ~f:(fun ()->
        let open Float in
        let open Point in
        let c_start= htcj.t_end in
        let c_end= htcj.c_end in
        let line= c_end - c_start in
        let length= (pow line.x 2.) +. (pow line.y 2.) |> sqrt in
        let d= Float.pow (length /. 4.) 2. /. 2. |> sqrt in
        Point.{ x= c_end.x -. d; y= c_end.y -. d; })
      in
      Line end_
    in
    let segments= h :: t :: c :: j :: [] in
    Path.
    {
      start;
      segments;
    }

  (*
    type hvhv= {
      hvhv_start: point;
      h1_length: float;
      v1_length: float;
      h2_length: float;
      v2_length: float;
    } (* Horizontal – Vertical – Horizontal – Vertical *)
  *)

  let from_hvhv hvhv=
    let start= hvhv.hvhv_start in
    let h1_end= { start with x= start.x +. hvhv.h1_length } in
    let v1_end= { h1_end with y= h1_end.y +. hvhv.v1_length } in
    let h2_end= { v1_end with x= v1_end.x +. hvhv.h2_length } in
    let v2_end= { h2_end with y= h2_end.y +. hvhv.v2_length } in
    let open Path in
    let segments= [
      Line h1_end;
      Line v1_end;
      Line h2_end;
      Line v2_end;
    ]
    in
    { start; segments }

  (*
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
  *)

  let from_hthtj (hthtj:hthtj)=
    let open Path in
    let htj= from_htj {
      htj_start= hthtj.t1_end;
      length= hthtj.h2_length;
      ctrl1= hthtj.t2_ctrl1;
      ctrl2= hthtj.t2_ctrl2;
      t_end= hthtj.t2_end;
      end_= hthtj.end_;
    } in
    let h, t=
      let h1_start= hthtj.hthtj_start in
      let t_start= { h1_start with x= h1_start.x +. hthtj.h1_length } in
      let width= t_start.x -. hthtj.t1_end.x
      and height= hthtj.t1_end.y -. t_start.y in
      let ctrl1= get_adjust hthtj.t1_ctrl1
        ~f:(fun ()->
          let x= t_start.x -. (width /. 4.)
          and y= t_start.y +. (height /. 2.) in
          Point.{x;y})
      and ctrl2= get_adjust hthtj.t1_ctrl2
        ~f:(fun ()->
          let x= hthtj.t1_end.x +. (width /. 2.)
          and y= hthtj.t1_end.y -. (height /. 4.) in
          Point.{x;y})
      and end'= hthtj.t1_end in
      (Line t_start, Ccurve {ctrl1; ctrl2; end'})
    in
    let start= hthtj.hthtj_start
    and segments= h::t::htj.segments in
    {
      start;
      segments;
    }

  (*
    type vu= {
      vu_start: point;
      length: float;
      end_: point adjust;
    } (* Vertical – Upward horizontal *)
  *)

  let from_vu vu=
    let start= vu.vu_start in
    let v_end= { start with y= start.y +. vu.length } in
    let u_end= get_adjust vu.end_ ~f:(fun ()->
      let d= Float.pow (vu.length /. 3.) 2. /. 2. |> sqrt in
      Point.{x= v_end.x +. d; y= v_end.y -. d}
      ) in
    let open Path in
    let segments= [
      Line v_end;
      Line u_end;
      ] in
    {
      start;
      segments;
    }

  (*
    type vh= {
      vh_start: point;
      v_length: float;
      h_length: float;
    } (* Vertical – Horizontal *)
  *)

  let from_vh vh=
    let start= vh.vh_start in
    let v_end= { start with y= start.y +. vh.v_length } in
    let h_end= { v_end with x= v_end.x +. vh.h_length } in
    let open Path in
    let segments= [
      Line v_end;
      Line h_end;
      ] in
    {
      start;
      segments;
    }

  (*
    type va= {
      va_start: point;
      v_length: float;
      h_length: float;
      a_radius: float adjust;
    } (* Vertical – Anticlockwise curve *)
  *)

  let from_va va=
    let start= va.va_start in
    let v_end= { start with y= start.y +. va.v_length } in
    let a_radius=
      let avg= (va.v_length +. va.h_length) /. 2.
      in avg /. 4.
    in
    let a_d= circle_ctrl_distance ~seg:4 a_radius in
    let open Point in
    let open Path in
    let h_start= { x= v_end.x +. a_radius; y= v_end.y +. a_radius } in
    let h_end= { h_start with x= h_start.x +. va.h_length } in
    let curve=
      let ctrl1= { v_end with y= v_end.y +. a_d }
      and ctrl2= { h_start with x= h_start.x -. a_d }
      and end'= h_start in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let open Path in
    let segments= [
      Line v_end;
      curve;
      Line h_end;
      ] in
    {
      start;
      segments;
    }

  (*
    type vaj= {
      vaj_start: point;
      v_length: float;
      h_length: float;
      a_radius: float adjust;
      end_: point adjust;
    } (* Vertical – Anticlockwise curve – J hook *)
  *)

  let from_vaj vaj=
    let va= from_va {
      va_start= vaj.vaj_start;
      v_length= vaj.v_length;
      h_length= vaj.h_length;
      a_radius= vaj.a_radius;
    }
    and j=
      let open Point in
      let a_radius=
        let avg= (vaj.v_length +. vaj.h_length) /. 2.
        in avg /. 4.
      in
      let start= vaj.vaj_start in
      let v_end= { start with y= start.y +. vaj.v_length } in
      let h_start= { x= v_end.x +. a_radius; y= v_end.y +. a_radius } in
      let h_end= { h_start with x= h_start.x +. vaj.h_length } in
      let j_end= get_adjust vaj.end_ ~f:(fun ()->
        let avg= (vaj.v_length +. vaj.h_length) /. 2. in
        let d= avg /. 4. in
        { h_end with y= h_end.y -. d })
      in
      Path.Line j_end;
    in
    { va with segments= va.segments @ [j] }

  (*
    type vhv= {
      vhv_start: point;
      v1_length: float;
      h_length: float;
      v2_length: float;
    } (* Vertical – Horizontal – Vertical *)
  *)

  let from_vhv vhv=
    let start= vhv.vhv_start in
    let v1_end= { start with y= start.y +. vhv.v1_length } in
    let h_end= { v1_end with x= v1_end.x +. vhv.h_length } in
    let v2_end= { h_end with y= h_end.y +. vhv.v2_length } in
    let open Path in
    let segments= [
      Line v1_end;
      Line h_end;
      Line v2_end;
    ]
    in
    { start; segments }

  (*
    type vht= {
      vht_start: point;
      v_length: float;
      h_length: float;
      ctrl1: point adjust;
      ctrl2: point adjust;
      end_: point;
    } (* Vertical – Horizontal – Throw *)
  *)

  let from_vht vht=
    let start= vht.vht_start in
    let v_end= { start with y= start.y +. vht.v_length } in
    let h_end= { v_end with x= v_end.x +. vht.h_length } in
    let open Path in
    let t=
      let start= h_end in
      let width= start.x -. vht.end_.x
      and height= vht.end_.y -. start.y in
      let ctrl1= get_adjust vht.ctrl1
        ~f:(fun ()->
          let x= start.x -. (width /. 4.)
          and y= start.y +. (height /. 2.) in
          Point.{x;y})
      and ctrl2= get_adjust vht.ctrl2
        ~f:(fun ()->
          let x= vht.end_.x +. (width /. 2.)
          and y= vht.end_.y -. (height /. 4.) in
          Point.{x;y})
      and end'= vht.end_ in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let segments= [
      Line v_end;
      Line h_end;
      t;
      ]
    in
    { start; segments }

  (*
    type vhtj= {
      vhtj_start: point;
      v_length: float;
      h_length: float;
      ctrl1: point adjust;
      ctrl2: point adjust;
      t_end: point;
      end_: point adjust;
    } (* Vertical – Horizontal – Throw – J hook *)
  *)

  let from_vhtj vhtj=
    let h_start= {vhtj.vhtj_start with
        y= vhtj.vhtj_start.y +. vhtj.v_length}
    in
    let htj= from_htj {
      htj_start= h_start;
      length= vhtj.h_length;
      ctrl1= vhtj.ctrl1;
      ctrl2= vhtj.ctrl2;
      t_end= vhtj.t_end;
      end_= vhtj.end_;
    } in
    let open Path in
    let v= Line h_start in
    let start= vhtj.vhtj_start
    and segments= v :: htj.segments in
    { start; segments }

  (*
    type vj= {
      vj_start: point;
      length: float;
      end_: point adjust;
    } (* Vertical – J hook *)
  *)

  let from_vj vj=
    let start= vj.vj_start in
    let v_end= {start with y= start.y +. vj.length} in
    let open Path in
    let v= Line v_end  in
    let j=
      let j= get_adjust vj.end_ ~f:(fun ()->
        let t_d= Float.pow (vj.length /. 4.) 2. /. 2. |> sqrt in
        Point.{ x= v_end.x -. t_d; y= v_end.y -. t_d })
      in
      Line j
    in
    let segments= [ v; j ] in
    {
      start;
      segments;
    }

  (*
    type vc= {
      vc_start: point;
      v_length: float;
      h_length: float;
      a_radius: float adjust;
    } (* Vertical – Clockwise curve *)
  *)

  let from_vc vc=
    let start= vc.vc_start in
    let v_end= { start with y= start.y +. vc.v_length } in
    let a_radius=
      let avg= (vc.v_length +. vc.h_length) /. 2.
      in avg /. 4.
    in
    let a_d= circle_ctrl_distance ~seg:4 a_radius in
    let open Point in
    let open Path in
    let h_start= { x= v_end.x -. a_radius; y= v_end.y +. a_radius } in
    let h_end= { h_start with x= h_start.x -. vc.h_length } in
    let curve=
      let ctrl1= { v_end with y= v_end.y +. a_d }
      and ctrl2= { h_start with x= h_start.x +. a_d }
      and end'= h_start in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let open Path in
    let segments= [
      Line v_end;
      curve;
      Line h_end;
      ] in
    {
      start;
      segments;
    }

  (*
    type vcj= {
      vcj_start: point;
      v_length: float;
      h_length: float;
      a_radius: float adjust;
      end_: point adjust;
    } (* Vertical – Clockwise curve – J hook *)
  *)

  let from_vcj vcj=
    let vc=
      let vc= {
        vc_start= vcj.vcj_start;
        v_length= vcj.v_length;
        h_length= vcj.h_length;
        a_radius= vcj.a_radius;
      }
      in
      from_vc vc
    in
    let open Path in
    let vc_end= Path.get_end vc in
    let j=
      let j= get_adjust vcj.end_ ~f:(fun ()->
        let avg_len= (vcj.v_length +. vcj.h_length) /. 2. in
        let t_d= avg_len /. 4. in
        { vc_end with y= vc_end.y -. t_d })
      in
      Line j
    in
    let segments= vc.segments @ [j] in
    {
      vc with
      segments;
    }

  (*
    type tu= {
      tu_start: point;
      ctrl1: point adjust;
      ctrl2: point adjust;
      t_end: point;
      end_: point adjust;
    } (* Throw – Upward horizontal *)
  *)

  let from_tu tu=
    let open Path in
    let start= tu.tu_start in
    let width= start.x -. tu.t_end.x
    and height= tu.t_end.y -. start.y in
    let t=
      let ctrl1= get_adjust tu.ctrl1
        ~f:(fun ()->
          let x= tu.tu_start.x -. (width /. 4.)
          and y= tu.tu_start.y +. (height /. 2.) in
          Point.{x;y})
      and ctrl2= get_adjust tu.ctrl2
        ~f:(fun ()->
          let x= tu.t_end.x +. (width /. 2.)
          and y= tu.t_end.y -. (height /. 4.) in
          Point.{x;y})
      and end'= tu.t_end in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let u=
      let u= get_adjust tu.end_ ~f:(fun ()->
        let len= (Float.pow width 2.) +. (Float.pow height 2.) |> sqrt in
        let d_y= len /. 2. in
        let d_x= (Float.pow len 2.) -. (Float.pow d_y 2.) |> sqrt in
        Point.{ x= tu.t_end.x +. d_x; y= tu.t_end.y -. d_y })
      in
      Line u
    in
    let segments= [t; u] in
    {
      start;
      segments;
    }

  (*
    type th= {
      th_start: point;
      ctrl1: point adjust;
      ctrl2: point adjust;
      t_end: point;
      length: float adjust;
    } (* Throw – Horizontal *)
  *)

  let from_th th=
    let open Path in
    let start= th.th_start
    and t_end= th.t_end in
    let width= start.x -. th.t_end.x
    and height= th.t_end.y -. start.y in
    let t=
      let ctrl1= get_adjust th.ctrl1
        ~f:(fun ()->
          let x= start.x -. (width /. 4.)
          and y= start.y +. (height /. 2.) in
          Point.{x;y})
      and ctrl2= get_adjust th.ctrl2
        ~f:(fun ()->
          let x= t_end.x +. (width /. 2.)
          and y= t_end.y -. (height /. 4.) in
          Point.{x;y})
      and end'= th.t_end in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let h=
      let h_len= get_adjust th.length ~f:(fun ()->
        (Float.pow width 2.) +. (Float.pow height 2.) |> sqrt)
      in
      Line { t_end with x= t_end.x +. h_len }
    in
    let segments= [t; h] in
    {
      start;
      segments;
    }

  (*
    type td= {
      td_start: point;
      ctrl1: point adjust;
      ctrl2: point adjust;
      t_end: point;
      end_: point adjust;
    } (* Throw – Dot *)
  *)

  let from_td td=
    let open Path in
    let start= td.td_start
    and t_end= td.t_end in
    let height= td.t_end.y -. start.y in
    let d_end= get_adjust td.end_ ~f:(fun ()->
      Point.{ x= start.x -. height *. 0.1; y= start.y +. height *. 1.9 }
    )
    in
    let t=
      let end'= t_end in
      let calc_ctrl= template_curve ~start ~end' in
      let ctrl1= get_adjust td.ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.1; y= 0.4})
      and ctrl2= get_adjust td.ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.1; y= 0.5})
      in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let d=
      let start= t_end
      and end'= d_end in
      let calc_ctrl= template_curve ~start ~end' in
      let ctrl1= get_adjust td.ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.10; y= 0.45})
      and ctrl2= get_adjust td.ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.12; y= 0.8})
      in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let segments= [t; d] in
    {
      start;
      segments;
    }

  (*
    type wtd= {
      wtd_start: point;
      v_length: float adjust;
      ctrl1: point adjust;
      ctrl2: point adjust;
      t_end: point;
      end_: point adjust;
    } (* Wilted Throw – Dot *)
  *)

  (*
  let from_wtd wtd=
    let open Path in
    let start= wtd.wtd_start
    and t_end= wtd.t_end in
    let width= start.x -. wtd.end_.x
    and height= wtd.end_.y -. start.y in
    let v_length= get_adjust wtd.v_length
      ~f:(fun()-> start.y +. height /. 2.) in
    let t_height= height -. v_length in
    let wt=
      let t_start=
        let x= start.x
        and y= start.y +. v_length in
        Point.{x;y}
      in
      let ctrl1= get_adjust wtd.ctrl1
        ~f:(fun ()->
          let x= t_start.x (* start straight *)
          and y= t_start.y +. (t_height /. 2.) in
          Point.{x;y})
      and ctrl2= get_adjust wtd.ctrl2
        ~f:(fun ()->
          let x= wtd.end_.x +. (width /. 2.)
          and y= wtd.end_.y -. (t_height /. 4.) in
          Point.{x;y})
      and end'= wtd.end_ in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let d=
      let start= t_end in
      let ctrl1=
        let x= start.x +. (width /. 2.)
        and y= start.y +. (height /. 4.) in
        Point.{x;y}
      and ctrl2=
        let x= t_end.x -. (width /. 4.)
        and y= t_end.y -. (height /. 2.) in
        Point.{x;y}
      and end'= wtd.end_ in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let segments= [wt; d] in
    {
      start;
      segments;
    }
  *)

  let from_wtd wtd=
    let open Path in
    let start= wtd.wtd_start
    and t_end= wtd.t_end in
    let height= wtd.t_end.y -. start.y in
    let d_end= get_adjust wtd.end_ ~f:(fun ()->
      Point.{ x= start.x +. height *. 0.2; y= start.y +. height *. 1.9 }
    )
    in
    let t=
      let end'= t_end in
      let calc_ctrl= template_curve ~start ~end' in
      let ctrl1= get_adjust wtd.ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.05; y= 0.7})
      and ctrl2= get_adjust wtd.ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.05; y= 0.9})
      in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let d=
      let start= t_end
      and end'= d_end in
      let calc_ctrl= template_curve ~start ~end' in
      let ctrl1= get_adjust wtd.ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.10; y= 0.45})
      and ctrl2= get_adjust wtd.ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.12; y= 0.8})
      in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let segments= [t; d] in
    {
      start;
      segments;
    }

  (*
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
  *)

  let from_tht tht=
    let th= {
      th_start= tht.tht_start;
      ctrl1= tht.t1_ctrl1;
      ctrl2= tht.t2_ctrl1;
      t_end= tht.t1_end;
      length= Specify tht.length;
    } in
    let th_path= from_th th in
    let t2=
      let t2_start= { tht.t1_end with x= tht.t1_end.x +. tht.length } in
      let open Path in
      let width= t2_start.x -. tht.end_.x
      and height= tht.end_.y -. t2_start.y in
      let ctrl1= get_adjust tht.t2_ctrl1
        ~f:(fun ()->
          let x= t2_start.x -. (width /. 4.)
          and y= t2_start.y +. (height /. 2.) in
          Point.{x;y})
      and ctrl2= get_adjust tht.t2_ctrl2
        ~f:(fun ()->
          let x= tht.end_.x +. (width /. 2.)
          and y= tht.end_.y -. (height /. 4.) in
          Point.{x;y})
      and end'= tht.end_ in
      Ccurve {ctrl1; ctrl2; end'}
    in
    {
      th_path with segments= th_path.segments @ [t2]
    }

  (*
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
  *)

  let from_thtj (thtj:thtj)=
    let open Path in
    let t=
      let end'= thtj.t1_end in
      let calc_ctrl= template_curve ~start:thtj.thtj_start ~end' in
      let ctrl1= get_adjust thtj.t1_ctrl1 ~f:(fun ()->
        calc_ctrl ~ratio:{x= 0.05; y=0.4})
      and ctrl2= get_adjust thtj.t1_ctrl2 ~f:(fun ()->
        calc_ctrl ~ratio:{x= 0.05; y=0.6}) in
      Path.Ccurve {ctrl1; ctrl2; end'}
    in
    let htj= from_htj {
      htj_start= thtj.t1_end;
      length= thtj.length;
      ctrl1= thtj.t2_ctrl1;
      ctrl2= thtj.t2_ctrl2;
      t_end= thtj.t2_end;
      end_= thtj.end_;
    } in
    let start= thtj.thtj_start
    and segments= t :: htj.segments in
    {
      start;
      segments;
    }

  (*
    type tj= {
      tj_start: point;
      ctrl1: point adjust;
      ctrl2: point adjust;
      t_end: point;
      end_: point adjust;
    } (* Throw – J hook *)
      (* 1. end_ is to the bottom left of tj_start *)
  *)

  let from_tj tj=
    let open Path in
    let pow= Float.pow in
    let (t, length)=
      let t_start= tj.tj_start in
      let width= t_start.x -. tj.t_end.x
      and height= tj.t_end.y -. t_start.y in
      let ctrl1= get_adjust tj.ctrl1
        ~f:(fun ()->
          let x= t_start.x -. (width /. 4.)
          and y= t_start.y +. (height /. 2.) in
          Point.{x;y})
      and ctrl2= get_adjust tj.ctrl2
        ~f:(fun ()->
          let x= tj.t_end.x +. (width /. 2.)
          and y= tj.t_end.y -. (height /. 4.) in
          Point.{x;y})
      and end'= tj.t_end
      and length= (pow width 2.) +. (pow height 2.) |> sqrt in
      (Ccurve {ctrl1; ctrl2; end'}, length)
    in
    let j=
      let end'= get_adjust tj.end_ ~f:(fun ()->
        let t_end= tj.t_end in
        let d=
          Float.pow (length /. 4.) 2.
          /. 2. |> sqrt in
        { t_end with y= t_end.y -. d; })
      in
      Line end'
    in
    let start= tj.tj_start
    and segments= [t;j] in
    {
      start;
      segments;
    }

  (*
    type cj= {
      cj_start: point;
      ctrl1: point adjust;
      ctrl2: point adjust;
      t_end: point;
      end_: point adjust;
    } (* Clockwise curve – J hook *)
      (* 1. end_ is to the bottom right of cj_start *)
  *)

  (*
  let from_cj (cj:cj)=
    let vec, length=
      let open Point in
      let open Float in
      let vec= cj.t_end - cj.cj_start in
      let length= (pow vec.x 2.) +. (pow vec.y 2.) |> sqrt in
      (vec, length)
    in
    let width= length /. 2. in
    let angle=
      Point.angle vec -. Point.angle {x=0.;y=1.}
    in
    let rotate= Point.rotate ~angle in
    let distance_h= circle_ctrl_distance ~seg:4  width
    and distance_v= circle_ctrl_distance ~seg:4  (length /. 2.) in
    let open Path in
    let start= cj.cj_start in
    let right= Point.{ x= start.x +. width; y= start.y +. length /. 2. } in
    let end'=  { start with y= start.y +. length } in
    let right_rotateed= rotate right in
    let end_rotateed= cj.t_end in
    let curve1=
      let ctrl1= { start with x= start.x +. distance_h } |> rotate
      and ctrl2= { right with y= right.y -. distance_v } |> rotate
      and end'= right_rotateed in
      Ccurve {ctrl1; ctrl2; end'}
    and curve2=
      let ctrl1= { right with y= right.y +. distance_v } |> rotate
      and ctrl2= { end' with x= end'.x +. distance_h } |> rotate
      and end'= end_rotateed in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let j=
      let end'= get_adjust cj.end_ ~f:(fun ()->
        let t_end= cj.t_end in
        let d=
          Float.pow (length /. 4.) 2.
          /. 2. |> sqrt in
        Point.{ x= t_end.x -. d; y= t_end.y -. d; })
      in
      Line end'
    in
    let segments= [curve1; curve2;j] in
    {
      start;
      segments;
    }
  *)

  let from_cj (cj:cj)=
    let length=
      let open Point in
      let open Float in
      let vec= cj.t_end - cj.cj_start in
      (pow vec.x 2.) +. (pow vec.y 2.) |> sqrt
    in
    let open Path in
    let start= cj.cj_start in
    let curve=
      let t= from_t' {
        t'_start= cj.cj_start;
        ctrl1= cj.ctrl2;
        ctrl2= cj.ctrl2;
        end_= cj.t_end
        }
      in
      List.hd t.segments
    in
    let j=
      let end'= get_adjust cj.end_ ~f:(fun ()->
        let t_end= cj.t_end in
        let d=
          Float.pow (length /. 4.) 2.
          /. 2. |> sqrt in
        Point.{ x= t_end.x -. d; y= t_end.y -. d; })
      in
      Line end'
    in
    let segments= [curve;j] in
    {
      start;
      segments;
    }

  (*
    type fpj= {
      fpj_start: point;
      ctrl1: point adjust;
      ctrl2: point adjust;
      t_end: point;
      end_: point adjust;
    } (* Flat Press – J hook *)
      (* 1. the angle is limited between 5 to 40 degrees *)
  *)

  let from_fpj fpj=
    let fp= {
      fp_start= fpj.fpj_start;
      ctrl1= fpj.ctrl1;
      ctrl2= fpj.ctrl2;
      end_= fpj.t_end;
    } in
    let fp= from_fp fp in
    let open Float in
    let length=
      let line= Point.(fpj.t_end - fpj.fpj_start) in
      (pow line.x 2.) +. (pow line.y 2.) |> sqrt
    in
    let j=
      let end'= get_adjust fpj.end_ ~f:(fun ()->
        let d= length /. 4. in
        { fpj.t_end with y= fpj.t_end.y -. d }
        )
      in
      Path.Line end'
    in
    { fp with segments= fp.segments @ [j] }

  (*
    type pj= {
      pj_start: point;
      ctrl1: point adjust;
      ctrl2: point adjust;
      t_end: point;
      end_: point adjust;
    } (* Press – J hook *)
      (* 1. the angle is limited between 45 to 85 degrees *)
  *)

  let from_pj pj=
    let p= from_p {
      p_start= pj.pj_start;
      ctrl1= pj.ctrl1;
      ctrl2= pj.ctrl2;
      end_= pj.t_end;
    } in
    let length=
      let open Float in
      let line= Point.(pj.t_end - pj.pj_start) in
      (pow line.x 2.) +. (pow line.y 2.) |> sqrt
    in
    let j=
      let end'= get_adjust pj.end_ ~f:(fun ()->
        let d= length /. 4. in
        { pj.t_end with y= pj.t_end.y -. d }
        )
      in
      Path.Line end'
    in
    { p with segments= p.segments @ [j] }

  (*
    type thtaj= {
      thtaj_start: point;
      ctrl1: point adjust;
      ctrl2: point adjust;
      t1_end: point;
      h1_length: float;
      t2_end: point;
      h2_length: float;
      a_radius: float adjust;
      end_: point adjust;
    } (* Throw – Horizontal – Throw – Anticlockwise curve – J hook *)
  *)

  let from_thtaj thtaj=
    let t= from_t' {
      t'_start= thtaj.thtaj_start;
      ctrl1= thtaj.ctrl1;
      ctrl2= thtaj.ctrl2;
      end_= thtaj.t1_end;
    }
    and htaj= from_htaj {
      htaj_start= thtaj.t1_end;
      h1_length= thtaj.h1_length;
      t_end= thtaj.t2_end;
      h2_length= thtaj.h2_length;
      a_radius= thtaj.a_radius;
      end_= thtaj.end_;
    } in
    let start= thtaj.thtaj_start
    and segments= t.segments @ htaj.segments in
    Path.{ start; segments }

  (*
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
  *)

  let from_tod tod=
    let start= tod.tod_start
    and bottom= tod.bottom
    and left= tod.left in
    let right_height= left.y -. start.y
    and left_height= bottom.y -. left.y
    and left_width= bottom.x -. left.x
    and right_width= start.x -. bottom.x in
    let up= { bottom with y= bottom.y -. left_height *. 2. }
    and end_= get_adjust tod.end_ ~f:(fun ()->
      { start with y= start.y +. right_height *. 2. })
    in
    let d2= get_adjust tod.ctrl2 ~f:(fun ()->
      right_width *. 4. /. 5.)
    and d3= get_adjust tod.ctrl3 ~f:(fun ()->
      left_width /. 2.)
    and d4= get_adjust tod.ctrl4 ~f:(fun ()->
      left_height *. 4. /. 7.)
    in
    let open Path in
    let l1=
      let ctrl1= get_adjust tod.ctrl1 ~f:(fun ()->
        Point.{
          x= start.x -. right_width /. 6.;
          y= start.y +. right_height;
        })
      and ctrl2= { bottom with x= bottom.x +. d2 }
      and end'= bottom in
      Ccurve {ctrl1; ctrl2; end'}
    and l2=
      let ctrl1= { bottom with x= bottom.x -. d3 }
      and ctrl2= { left with y= left.y +. d4 }
      and end'= left in
      Ccurve {ctrl1; ctrl2; end'}
    and l3=
      let ctrl1= { left with y= left.y -. d4 }
      and ctrl2= { up with x= up.x -. d3 }
      and end'= up in
      Ccurve {ctrl1; ctrl2; end'}
    and l4=
      let ctrl1= { up with x= up.x +. d2 }
      and ctrl2= Point.{
        x= end_.x -. right_width /. 6.;
        y= end_.y -. right_height;
        }
      and end'= end_ in
      Ccurve {ctrl1; ctrl2; end'}
    in
    let segments= [l1;l2;l3;l4] in
    { start; segments }

  let to_path= function
    | H h-> from_h h
    | Sh sh-> from_sh sh
    | U u-> from_u u
    | Du du-> from_du du
    | V v-> from_v v
    | Sv sv-> from_sv sv
    | Rsv rsv-> from_rsv rsv
    | T t_-> from_t' t_
    | Ft ft-> from_ft ft
    | Wt wt-> from_wt wt
    | D d-> from_d d
    | Ed ed-> from_ed ed
    | Ld ld-> from_ld ld
    | Wd wd-> from_wd wd
    | P p-> from_p p
    | Up up-> from_up up
    | Hp hp-> from_hp hp
    | Fp fp-> from_fp fp
    | Ufp ufp-> from_ufp ufp
    | C c-> from_c c
    | A a-> from_a a
    | O o-> from_o o
    | Hj hj-> from_hj hj
    | Uj uj-> from_uj uj
    | Ht ht-> from_ht ht
    | Hsv hsv-> from_hsv hsv
    | Hv hv-> from_hv hv
    | Hvj hvj-> from_hvj hvj
    | Htj htj-> from_htj htj
    | Utj utj-> from_utj utj
    | Hvh hvh-> from_hvh hvh
    | Hvu hvu-> from_hvu hvu
    | Ha ha-> from_ha ha
    | Haj haj-> from_haj haj
    | Hpj hpj-> from_hpj hpj
    | Htaj htaj-> from_htaj htaj
    | Htc htc-> from_htc htc
    | Htht htht-> from_htht htht
    | Htcj htcj-> from_htcj htcj
    | Hvhv hvhv-> from_hvhv hvhv
    | Hthtj hthtj-> from_hthtj hthtj
    | Vu vu-> from_vu vu
    | Vh vh-> from_vh vh
    | Va va-> from_va va
    | Vaj vaj-> from_vaj vaj
    | Vhv vhv-> from_vhv vhv
    | Vht vht-> from_vht vht
    | Vhtj vhtj-> from_vhtj vhtj
    | Vj vj-> from_vj vj
    | Vc vc-> from_vc vc
    | Vcj vcj-> from_vcj vcj
    | Tu tu-> from_tu tu
    | Th th-> from_th th
    | Td td-> from_td td
    | Wtd wtd-> from_wtd wtd
    | Tht tht-> from_tht tht
    | Thtj thtj-> from_thtj thtj
    | Tj tj-> from_tj tj
    | Cj cj-> from_cj cj
    | Fpj fpj-> from_fpj fpj
    | Pj pj-> from_pj pj
    | Thtaj thtaj-> from_thtaj thtaj
    | Tod tod-> from_tod tod
end

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
      c_end= ps htc.c_end;
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

let to_path= To_path.to_path
let to_frame t=
  let frame, _last=t |> to_path |> Smaji_glyph_path.Path.frame in
  frame
