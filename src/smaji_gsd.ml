(*
 * smaji_gsd.ml
 * -----------
 * Copyright : (c) 2023 - 2025, smaji.org
 * Copyright : (c) 2023 - 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_gsd.
 *)


module Svg= Smaji_glyph_path.Svg
module Glif= Smaji_glyph_path.Glif
module Stroke= Stroke
module Path= Smaji_glyph_path.Path
module Point= Path.Point

open Printf

type outline_type=
  | Outline_svg
  | Outline_glif

type frame= Path.frame= {
  x: float;
  y: float;
  width: float;
  height: float;
}

let frame_to_string= Path.frame_to_string

let frame_dummy= Path.frame_dummy
let frame_zero= {
  x= 0.;
  y= 0.;
  width= 0.;
  height= 0.;
}

type stroke= Stroke.t

type pos= {
  pos_x: float;
  pos_y: float;
}
type ratio= {
  ratio_x: float;
  ratio_y: float;
}
type pos_ratio= {
  pos: pos;
  ratio: ratio;
}
let pos_ratio_default= {
  pos= {pos_x=0.;pos_y=0.};
  ratio= {ratio_x=1.;ratio_y=1.};
}

let pos_ratio_adjust ~pos_ratio frame=
  let pos= pos_ratio.pos
  and ratio= pos_ratio.ratio in
  let x= frame.x *. ratio.ratio_x +. pos.pos_x
  and y= frame.y *. ratio.ratio_y +. pos.pos_y
  and width= frame.width *. ratio.ratio_x
  and height= frame.height *. ratio.ratio_y in
  let (x, width )=
    if width < 1. then
      let d= 1. -. width in
      (x -. d, width +. d) 
    else
      (x, width)
  and (y, height )=
    if height < 1. then
      let d= 1. -. height in
      (y -. d, height +. d) 
    else
      (y, height)
  in
  { x; y; width; height }

type size= { width: float; height: float }

type code_point= int * int

let code_point_of_string str=
  let of_hex str= "0x"^str |> int_of_string in
  let codes= String.split_on_char ',' str in
  match codes with
  | []-> failwith "code_point_of_string"
  | c::e::_-> (of_hex c, of_hex e)
  | c::_->
    try (of_hex c,0) with
    | _->
      let codes= String.split_on_char ':' str in
      match codes with
      | []-> failwith "code_point_of_string"
      | c::e::_-> (of_hex c, of_hex e)
      | c::_-> (of_hex c,0)

let byte str i = Char.code (String.unsafe_get str i)
let fail str pos msg = failwith (Printf.sprintf "at position %d of %s: %s" pos str msg)

let unsafe_extract_next str ofs =
  let ch = String.unsafe_get str ofs in
  match ch with
    | '\x00' .. '\x7f' ->
      (Uchar.of_char ch, ofs + 1)
    | '\xc0' .. '\xdf' ->
      if ofs + 2 > String.length str then
        fail str ofs "unterminated UTF-8 sequence"
      else
        (Uchar.of_int (((Char.code ch land 0x1f) lsl 6) lor (byte str (ofs + 1) land 0x3f)), ofs + 2)
    | '\xe0' .. '\xef' ->
      if ofs + 3 > String.length str then
        fail str ofs "unterminated UTF-8 sequence"
      else
        (Uchar.of_int (
          ((Char.code ch land 0x0f) lsl 12) lor
          ((byte str (ofs + 1) land 0x3f) lsl 6)lor
          (byte str (ofs + 2) land 0x3f))
        , ofs + 3)
    | '\xf0' .. '\xf7' ->
      if ofs + 4 > String.length str then
        fail str ofs "unterminated UTF-8 sequence"
      else
        (Uchar.of_int (
          ((Char.code ch land 0x07) lsl 18) lor
          ((byte str (ofs + 1) land 0x3f) lsl 12) lor
          ((byte str (ofs + 2) land 0x3f) lsl 6) lor
          (byte str (ofs + 3) land 0x3f))
        , ofs + 4)
    | _ ->
      fail str ofs "invalid start of UTF-8 sequence"

let code_point_of_utf8 str=
  let len= String.length str in
  let core, next= unsafe_extract_next str 0 in
  if next >= len then
    (Uchar.to_int core, 0)
  else
    let variation, _= unsafe_extract_next str next in
    (Uchar.to_int core, Uchar.to_int variation)

let string_of_code_point cp=
  let c,e= cp in
  sprintf "(%x,%x)" c e

let path_of_code_point code_point=
  let code_core, code_variation= code_point in
  let core_dir= sprintf "%x" code_core in
  let variation_dir= sprintf "%x" code_variation in
  let ( / ) = Filename.concat in
  core_dir / variation_dir

let version_of_string str=
  let versions= String.split_on_char '.' str in
  match versions with
  | c::e::_-> (int_of_string c, int_of_string e)
  | c::_-> (int_of_string c,0)
  | []-> failwith "version_of_string"

type transform=
  | NoTransform
  | MirrorHorizontal
  | MirrorVertical
  | Rotate180

let transform_of_string= function
  | "" | "none"-> NoTransform
  | "mirror_horizontal"-> MirrorHorizontal
  | "mirror_vertical"-> MirrorVertical
  | "rotate180"-> Rotate180
  | _-> failwith "transform_of_string"
let string_of_transform= function
  | NoTransform-> "none"
  | MirrorHorizontal-> "mirror_horizontal"
  | MirrorVertical-> "mirror_vertical"
  | Rotate180-> "rotate180"

let reduce_transforms l=
  let[@tail_mod_cons] rec reduce l=
    match l with
    | []-> []
    | [_]-> l
    | MirrorHorizontal::MirrorHorizontal::tl-> reduce tl
    | MirrorVertical::MirrorVertical::tl-> reduce tl
    | Rotate180::Rotate180::tl-> reduce tl
    | MirrorHorizontal::MirrorVertical::tl->
      Rotate180::tl |> List.sort compare |> reduce
    | MirrorVertical::MirrorHorizontal::tl->
      Rotate180::tl |> List.sort compare |> reduce
    | hd::tl-> hd :: reduce tl
  in
  l |> List.sort compare |> reduce

module Raw = struct
  type ref= {
    code_point: code_point;
    frame: frame;
  }

  type stroke_unknown= {
    stroke: Stroke.t option;
    x: float option;
    y: float option;
    width: float option;
    height: float option;
  }

  type ref_unknown= {
    code_point: code_point option;
    x: float option;
    y: float option;
    width: float option;
    height: float option;
  }

  type element=
    | Stroke of Stroke.t
    | Ref of ref

  type gsd= {
    version_major: int;
    version_minor: int;
    code_point: code_point;
    transform: transform;
    elements: element list;
  }

  module Xml = struct
    let get_point name nodes=
      let (attrs, _nodes)= Ezxmlm.member_with_attr name nodes in
      let x= Ezxmlm.get_attr "x" attrs |> Float.of_string
      and y= Ezxmlm.get_attr "y" attrs |> Float.of_string in
      Point.{ x; y }

    let get_length name nodes=
      let (attrs, _nodes)= Ezxmlm.member_with_attr name nodes in
      Ezxmlm.get_attr "is" attrs |> Float.of_string

    let get_point_adjust name nodes=
      let open Stroke in
      let nodes= Ezxmlm.member name nodes in
      if Ezxmlm.has_member "auto" nodes then
        Auto
      else if Ezxmlm.has_member "point" nodes then
        let (attrs, _nodes)= Ezxmlm.member_with_attr "point" nodes in
        let x= Ezxmlm.get_attr "x" attrs |> Float.of_string
        and y= Ezxmlm.get_attr "y" attrs |> Float.of_string in
        Specify Point.{ x; y }
      else
        Auto

    let get_length_adjust name nodes=
      let open Stroke in
      let nodes= Ezxmlm.member name nodes in
      if Ezxmlm.has_member "auto" nodes then
        Auto
      else if Ezxmlm.has_member "length" nodes then
        let (attrs, _nodes)= Ezxmlm.member_with_attr "length" nodes in
        Specify (Ezxmlm.get_attr "is" attrs |> Float.of_string)
      else
        Auto
  end

  module GetStroke = struct
    let h node=
      match node with
      | `El (((_ns,"h"), _attrs), nodes)->
        let h_start= Xml.get_point "start" nodes
        and length= Xml.get_length "length" nodes in
        Stroke.H {
          h_start;
          length;
        } |> Option.some
      | _-> None

    let sh node=
      match node with
      | `El (((_ns,"sh"), _attrs), nodes)->
        let open Stroke in
        let sh_start= Xml.get_point "start" nodes
        and end_= Xml.get_point "end" nodes in
        Sh {
          sh_start;
          end_;
        } |> Option.some
      | _-> None

    let u node=
      match node with
      | `El (((_ns,"u"), _attrs), nodes)->
        let open Stroke in
        let u_start= Xml.get_point "start" nodes
        and end_= Xml.get_point "end" nodes in
        U {
          u_start;
          end_;
        } |> Option.some
      | _-> None

    let du node=
      match node with
      | `El (((_ns,"du"), _attrs), nodes)->
        let open Stroke in
        let du_start= Xml.get_point "start" nodes
        and dot= Xml.get_point_adjust "dot" nodes
        and end_= Xml.get_point "end" nodes in
        Du {
          du_start;
          dot;
          end_;
        } |> Option.some
      | _-> None

    let v node=
      match node with
      | `El (((_ns,"v"), _attrs), nodes)->
        let open Stroke in
        let v_start= Xml.get_point "start" nodes
        and length= Xml.get_length "length" nodes in
        V {
          v_start;
          length;
        } |> Option.some
      | _-> None

    let sv node=
      match node with
      | `El (((_ns,"sv"), _attrs), nodes)->
        let open Stroke in
        let sv_start= Xml.get_point "start" nodes
        and length= Xml.get_length "length" nodes
        and width= Xml.get_length "width" nodes in
        Sv {
          sv_start;
          length;
          width;
        } |> Option.some
      | _-> None

    let rsv node=
      match node with
      | `El (((_ns,"rsv"), _attrs), nodes)->
        let open Stroke in
        let rsv_start= Xml.get_point "start" nodes
        and length= Xml.get_length "length" nodes
        and width= Xml.get_length "width" nodes in
        Rsv {
          rsv_start;
          length;
          width;
        } |> Option.some
      | _-> None

    let t' node=
      match node with
      | `El (((_ns,"t"), _attrs), nodes)->
        let open Stroke in
        let t'_start= Xml.get_point "start" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and end_= Xml.get_point "end" nodes in
        T {
          t'_start;
          ctrl1;
          ctrl2;
          end_;
        } |> Option.some
      | _-> None

    let ft node=
      match node with
      | `El (((_ns,"ft"), _attrs), nodes)->
        let open Stroke in
        let ft_start= Xml.get_point "start" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and end_= Xml.get_point "end" nodes in
        Ft {
          ft_start;
          ctrl1;
          ctrl2;
          end_;
        } |> Option.some
      | _-> None

    let wt node=
      match node with
      | `El (((_ns,"wt"), _attrs), nodes)->
        let open Stroke in
        let wt_start= Xml.get_point "start" nodes
        and v_length= Xml.get_length_adjust "length" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and end_= Xml.get_point "end" nodes in
        Wt {
          wt_start;
          v_length;
          ctrl1;
          ctrl2;
          end_;
        } |> Option.some
      | _-> None

    let d node=
      match node with
      | `El (((_ns,"d"), _attrs), nodes)->
        let open Stroke in
        let d_start= Xml.get_point "start" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        D {
          d_start;
          end_;
        } |> Option.some
      | _-> None

    let ed node=
      match node with
      | `El (((_ns,"ed"), _attrs), nodes)->
        let open Stroke in
        let ed_start= Xml.get_point "start" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Ed {
          ed_start;
          end_;
        } |> Option.some
      | _-> None

    let ld node=
      match node with
      | `El (((_ns,"ld"), _attrs), nodes)->
        let open Stroke in
        let ld_start= Xml.get_point "start" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Ld {
          ld_start;
          end_;
        } |> Option.some
      | _-> None

    let wd node=
      match node with
      | `El (((_ns,"wd"), _attrs), nodes)->
        let open Stroke in
        let wd_start= Xml.get_point "start" nodes
        and length= Xml.get_length_adjust "length" nodes in
        Wd {
          wd_start;
          length;
        } |> Option.some
      | _-> None

    let p node=
      match node with
      | `El (((_ns,"p"), _attrs), nodes)->
        let open Stroke in
        let p_start= Xml.get_point "start" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and end_= Xml.get_point "end" nodes in
        P {
          p_start;
          ctrl1;
          ctrl2;
          end_;
        } |> Option.some
      | _-> None

    let up node=
      match node with
      | `El (((_ns,"up"), _attrs), nodes)->
        let open Stroke in
        let up_start= Xml.get_point_adjust "start" nodes
        and p_start= Xml.get_point "p_start" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and end_= Xml.get_point "end" nodes in
        Up {
          up_start;
          p_start;
          ctrl1;
          ctrl2;
          end_;
        } |> Option.some
      | _-> None

    let hp node=
      match node with
      | `El (((_ns,"hp"), _attrs), nodes)->
        let open Stroke in
        let hp_start= Xml.get_point "start" nodes
        and length= Xml.get_length "length" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and end_= Xml.get_point "end" nodes in
        Hp {
          hp_start;
          length;
          ctrl1;
          ctrl2;
          end_;
        } |> Option.some
      | _-> None

    let fp node=
      match node with
      | `El (((_ns,"fp"), _attrs), nodes)->
        let open Stroke in
        let fp_start= Xml.get_point "start" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and end_= Xml.get_point "end" nodes in
        Fp {
          fp_start;
          ctrl1;
          ctrl2;
          end_;
        } |> Option.some
      | _-> None

    let ufp node=
      match node with
      | `El (((_ns,"ufp"), _attrs), nodes)->
        let open Stroke in
        let ufp_start= Xml.get_point "start" nodes
        and p_start= Xml.get_point "p_start" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and end_= Xml.get_point "end" nodes in
        Ufp {
          ufp_start;
          p_start;
          ctrl1;
          ctrl2;
          end_;
        } |> Option.some
      | _-> None

    let c node=
      match node with
      | `El (((_ns,"c"), _attrs), nodes)->
        let open Stroke in
        let c_start= Xml.get_point "start" nodes
        and length= Xml.get_length "length" nodes
        and width= Xml.get_length "width" nodes
        and ctrl1= Xml.get_length_adjust "ctrl1" nodes
        and ctrl2= Xml.get_length_adjust "ctrl2" nodes in
        C {
          c_start;
          length;
          width;
          ctrl1;
          ctrl2;
        } |> Option.some
      | _-> None

    let a node=
      match node with
      | `El (((_ns,"a"), _attrs), nodes)->
        let open Stroke in
        let a_start= Xml.get_point "start" nodes
        and length= Xml.get_length "length" nodes
        and width= Xml.get_length "width" nodes
        and ctrl1= Xml.get_length_adjust "ctrl1" nodes
        and ctrl2= Xml.get_length_adjust "ctrl2" nodes in
        A {
          a_start;
          length;
          width;
          ctrl1;
          ctrl2;
        } |> Option.some
      | _-> None

    let o node=
      match node with
      | `El (((_ns,"o"), _attrs), nodes)->
        let open Stroke in
        let o_start= Xml.get_point "start" nodes
        and length= Xml.get_length "length" nodes
        and width= Xml.get_length "width" nodes
        and ctrl_h= Xml.get_length_adjust "ctrl_h" nodes
        and ctrl_v= Xml.get_length_adjust "ctrl_v" nodes in
        O {
          o_start;
          length;
          width;
          ctrl_h;
          ctrl_v;
        } |> Option.some
      | _-> None

    let hj node=
      match node with
      | `El (((_ns,"hj"), _attrs), nodes)->
        let open Stroke in
        let hj_start= Xml.get_point "start" nodes
        and length= Xml.get_length "length" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Hj {
          hj_start;
          length;
          end_;
        } |> Option.some
      | _-> None

    let uj node=
      match node with
      | `El (((_ns,"uj"), _attrs), nodes)->
        let open Stroke in
        let uj_start= Xml.get_point "start" nodes
        and u_end= Xml.get_point "u_end" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Uj {
          uj_start;
          u_end;
          end_;
        } |> Option.some
      | _-> None

    let ht node=
      match node with
      | `El (((_ns,"ht"), _attrs), nodes)->
        let open Stroke in
        let ht_start= Xml.get_point "start" nodes
        and length= Xml.get_length "length" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and end_= Xml.get_point "end" nodes in
        Ht {
          ht_start;
          length;
          ctrl1;
          ctrl2;
          end_;
        } |> Option.some
      | _-> None

    let hsv node=
      match node with
      | `El (((_ns,"hsv"), _attrs), nodes)->
        let open Stroke in
        let hsv_start= Xml.get_point "start" nodes
        and h_length= Xml.get_length "length" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Hsv {
          hsv_start;
          h_length;
          end_;
        } |> Option.some
      | _-> None

    let hv node=
      match node with
      | `El (((_ns,"hv"), _attrs), nodes)->
        let open Stroke in
        let hv_start= Xml.get_point "start" nodes
        and h_length= Xml.get_length "h_length" nodes
        and v_length= Xml.get_length_adjust "v_length" nodes in
        Hv {
          hv_start;
          h_length;
          v_length;
        } |> Option.some
      | _-> None

    let hvj node=
      match node with
      | `El (((_ns,"hvj"), _attrs), nodes)->
        let open Stroke in
        let hvj_start= Xml.get_point "start" nodes
        and h_length= Xml.get_length "h_length" nodes
        and v_length= Xml.get_length_adjust "v_length" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Hvj {
          hvj_start;
          h_length;
          v_length;
          end_;
        } |> Option.some
      | _-> None

    let htj node=
      match node with
      | `El (((_ns,"htj"), _attrs), nodes)->
        let open Stroke in
        let htj_start= Xml.get_point "start" nodes
        and length= Xml.get_length "length" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and t_end= Xml.get_point "t_end" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Htj {
          htj_start;
          length;
          ctrl1;
          ctrl2;
          t_end;
          end_;
        } |> Option.some
      | _-> None

    let utj node=
      match node with
      | `El (((_ns,"utj"), _attrs), nodes)->
        let open Stroke in
        let utj_start= Xml.get_point "start" nodes
        and t_start= Xml.get_point "t_start" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and t_end= Xml.get_point "t_end" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Utj {
          utj_start;
          t_start;
          ctrl1;
          ctrl2;
          t_end;
          end_;
        } |> Option.some
      | _-> None

    let hvh node=
      match node with
      | `El (((_ns,"hvh"), _attrs), nodes)->
        let open Stroke in
        let hvh_start= Xml.get_point "start" nodes
        and h1_length= Xml.get_length "h1_length" nodes
        and v_length= Xml.get_length "v_length" nodes
        and h2_length= Xml.get_length "h2_length" nodes in
        Hvh {
          hvh_start;
          h1_length;
          v_length;
          h2_length;
        } |> Option.some
      | _-> None

    let hvu node=
      match node with
      | `El (((_ns,"hvu"), _attrs), nodes)->
        let open Stroke in
        let hvu_start= Xml.get_point "start" nodes
        and h_length= Xml.get_length "h_length" nodes
        and v_length= Xml.get_length "v_length" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Hvu {
          hvu_start;
          h_length;
          v_length;
          end_;
        } |> Option.some
      | _-> None

    let ha node=
      match node with
      | `El (((_ns,"ha"), _attrs), nodes)->
        let open Stroke in
        let ha_start= Xml.get_point "start" nodes
        and h1_length= Xml.get_length "h1_length" nodes
        and v_length= Xml.get_length "v_length" nodes
        and h2_length= Xml.get_length "h2_length" nodes
        and a_radius= Xml.get_length_adjust "a_radius" nodes in
        Ha {
          ha_start;
          h1_length;
          v_length;
          h2_length;
          a_radius;
        } |> Option.some
      | _-> None

    let haj node=
      match node with
      | `El (((_ns,"haj"), _attrs), nodes)->
        let open Stroke in
        let haj_start= Xml.get_point "start" nodes
        and h1_length= Xml.get_length "h1_length" nodes
        and v_length= Xml.get_length "v_length" nodes
        and h2_length= Xml.get_length "h2_length" nodes
        and a_radius= Xml.get_length_adjust "a_radius" nodes
        and end_= Xml.get_point_adjust "ned" nodes in
        Haj {
          haj_start;
          h1_length;
          v_length;
          h2_length;
          a_radius;
          end_;
        } |> Option.some
      | _-> None

    let hpj node=
      match node with
      | `El (((_ns,"hpj"), _attrs), nodes)->
        let open Stroke in
        let hpj_start= Xml.get_point "start" nodes
        and length= Xml.get_length "h1_length" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and p_end= Xml.get_point "p_end" nodes
        and end_= Xml.get_point_adjust "ned" nodes in
        Hpj {
          hpj_start;
          length;
          ctrl1;
          ctrl2;
          p_end;
          end_;
        } |> Option.some
      | _-> None

    let htaj node=
      match node with
      | `El (((_ns,"htaj"), _attrs), nodes)->
        let open Stroke in
        let htaj_start= Xml.get_point "start" nodes
        and h1_length= Xml.get_length "h1_length" nodes
        and t_end= Xml.get_point "t_end" nodes
        and h2_length= Xml.get_length "h2_length" nodes
        and a_radius= Xml.get_length_adjust "a_radius" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Htaj {
          htaj_start;
          h1_length;
          t_end;
          h2_length;
          a_radius;
          end_;
        } |> Option.some
      | _-> None

    let htc node=
      match node with
      | `El (((_ns,"htc"), _attrs), nodes)->
        let open Stroke in
        let htc_start= Xml.get_point "start" nodes
        and h_length= Xml.get_length "h_length" nodes
        and t_ctrl1= Xml.get_point_adjust "t_ctrl1" nodes
        and t_ctrl2= Xml.get_point_adjust "t_ctrl2" nodes
        and t_end= Xml.get_point "t_end" nodes
        and c_ctrl1= Xml.get_point_adjust "c_ctrl1" nodes
        and c_ctrl2= Xml.get_point_adjust "c_ctrl2" nodes
        and c_end= Xml.get_point "c_end" nodes in
        Htc {
          htc_start;
          h_length;
          t_ctrl1;
          t_ctrl2;
          t_end;
          c_ctrl1;
          c_ctrl2;
          c_end;
        } |> Option.some
      | _-> None

    let htht node=
      match node with
      | `El (((_ns,"htht"), _attrs), nodes)->
        let open Stroke in
        let htht_start= Xml.get_point "start" nodes
        and h1_length= Xml.get_length "h1_length" nodes
        and t1_ctrl1= Xml.get_point_adjust "t1_ctrl1" nodes
        and t1_ctrl2= Xml.get_point_adjust "t1_ctrl2" nodes
        and t1_end= Xml.get_point "t1_end" nodes
        and h2_length= Xml.get_length "h2_length" nodes
        and t2_ctrl1= Xml.get_point_adjust "t2_ctrl1" nodes
        and t2_ctrl2= Xml.get_point_adjust "t2_ctrl2" nodes
        and end_= Xml.get_point "end" nodes
        in
        Htht {
          htht_start;
          h1_length;
          t1_ctrl1;
          t1_ctrl2;
          t1_end;
          h2_length;
          t2_ctrl1;
          t2_ctrl2;
          end_;
        } |> Option.some
      | _-> None

    let htcj node=
      match node with
      | `El (((_ns,"htcj"), _attrs), nodes)->
        let open Stroke in
        let htcj_start= Xml.get_point "start" nodes
        and h_length= Xml.get_length "h_length" nodes
        and t_ctrl1= Xml.get_point_adjust "t_ctrl1" nodes
        and t_ctrl2= Xml.get_point_adjust "t_ctrl2" nodes
        and t_end= Xml.get_point "t_end" nodes
        and c_ctrl1= Xml.get_point_adjust "c_ctrl1" nodes
        and c_ctrl2= Xml.get_point_adjust "c_ctrl2" nodes
        and c_end= Xml.get_point "c_end" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Htcj {
          htcj_start;
          h_length;
          t_ctrl1;
          t_ctrl2;
          t_end;
          c_ctrl1;
          c_ctrl2;
          c_end;
          end_;
        } |> Option.some
      | _-> None

    let hvhv node=
      match node with
      | `El (((_ns,"hvhv"), _attrs), nodes)->
        let open Stroke in
        let hvhv_start= Xml.get_point "start" nodes
        and h1_length= Xml.get_length "h1_length" nodes
        and v1_length= Xml.get_length "v1_length" nodes
        and h2_length= Xml.get_length "h2_length" nodes
        and v2_length= Xml.get_length "v2_length" nodes in
        Hvhv {
          hvhv_start;
          h1_length;
          v1_length;
          h2_length;
          v2_length;
        } |> Option.some
      | _-> None

    let hthtj node=
      match node with
      | `El (((_ns,"hthtj"), _attrs), nodes)->
        let open Stroke in
        let hthtj_start= Xml.get_point "start" nodes
        and h1_length= Xml.get_length "h1_length" nodes
        and t1_ctrl1= Xml.get_point_adjust "t1_ctrl1" nodes
        and t1_ctrl2= Xml.get_point_adjust "t1_ctrl2" nodes
        and t1_end= Xml.get_point "t1_end" nodes
        and h2_length= Xml.get_length "h2_length" nodes
        and t2_ctrl1= Xml.get_point_adjust "t2_ctrl1" nodes
        and t2_ctrl2= Xml.get_point_adjust "t2_ctrl2" nodes
        and t2_end= Xml.get_point "end" nodes
        and end_= Xml.get_point_adjust "end" nodes
        in
        Hthtj {
          hthtj_start;
          h1_length;
          t1_ctrl1;
          t1_ctrl2;
          t1_end;
          h2_length;
          t2_ctrl1;
          t2_ctrl2;
          t2_end;
          end_;
        } |> Option.some
      | _-> None

    let vu node=
      match node with
      | `El (((_ns,"vu"), _attrs), nodes)->
        let open Stroke in
        let vu_start= Xml.get_point "start" nodes
        and length= Xml.get_length "length" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Vu {
          vu_start;
          length;
          end_
        } |> Option.some
      | _-> None

    let vh node=
      match node with
      | `El (((_ns,"vh"), _attrs), nodes)->
        let open Stroke in
        let vh_start= Xml.get_point "start" nodes
        and v_length= Xml.get_length "v_length" nodes
        and h_length= Xml.get_length "h_length" nodes in
        Vh {
          vh_start;
          v_length;
          h_length;
        } |> Option.some
      | _-> None

    let va node=
      match node with
      | `El (((_ns,"va"), _attrs), nodes)->
        let open Stroke in
        let va_start= Xml.get_point "start" nodes
        and v_length= Xml.get_length "v_length" nodes
        and h_length= Xml.get_length "h_length" nodes
        and a_radius= Xml.get_length_adjust "a_radius" nodes in
        Va {
          va_start;
          v_length;
          h_length;
          a_radius;
        } |> Option.some
      | _-> None

    let vaj node=
      match node with
      | `El (((_ns,"vaj"), _attrs), nodes)->
        let open Stroke in
        let vaj_start= Xml.get_point "start" nodes
        and v_length= Xml.get_length "v_length" nodes
        and h_length= Xml.get_length "h_length" nodes
        and a_radius= Xml.get_length_adjust "a_radius" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Vaj {
          vaj_start;
          v_length;
          h_length;
          a_radius;
          end_;
        } |> Option.some
      | _-> None

    let vhv node=
      match node with
      | `El (((_ns,"vhv"), _attrs), nodes)->
        let open Stroke in
        let vhv_start= Xml.get_point "start" nodes
        and v1_length= Xml.get_length "v1_length" nodes
        and h_length= Xml.get_length "h_length" nodes
        and v2_length= Xml.get_length "v2_length" nodes in
        Vhv {
          vhv_start;
          v1_length;
          h_length;
          v2_length;
        } |> Option.some
      | _-> None

    let vht node=
      match node with
      | `El (((_ns,"vht"), _attrs), nodes)->
        let open Stroke in
        let vht_start= Xml.get_point "start" nodes
        and v_length= Xml.get_length "v_length" nodes
        and h_length= Xml.get_length "h_length" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and end_= Xml.get_point "end" nodes in
        Vht {
          vht_start;
          v_length;
          h_length;
          ctrl1;
          ctrl2;
          end_;
        } |> Option.some
      | _-> None

    let vhtj node=
      match node with
      | `El (((_ns,"vhtj"), _attrs), nodes)->
        let open Stroke in
        let vhtj_start= Xml.get_point "start" nodes
        and v_length= Xml.get_length "v_length" nodes
        and h_length= Xml.get_length "h_length" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and t_end= Xml.get_point "t_end" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Vhtj {
          vhtj_start;
          v_length;
          h_length;
          ctrl1;
          ctrl2;
          t_end;
          end_;
        } |> Option.some
      | _-> None

    let vj node=
      match node with
      | `El (((_ns,"vj"), _attrs), nodes)->
        let open Stroke in
        let vj_start= Xml.get_point "start" nodes
        and length= Xml.get_length "length" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Vj {
          vj_start;
          length;
          end_;
        } |> Option.some
      | _-> None

    let vc node=
      match node with
      | `El (((_ns,"vc"), _attrs), nodes)->
        let open Stroke in
        let vc_start= Xml.get_point "start" nodes
        and v_length= Xml.get_length "length" nodes
        and h_length= Xml.get_length "length" nodes
        and a_radius= Xml.get_length_adjust "a_radius" nodes in
        Vc {
          vc_start;
          v_length;
          h_length;
          a_radius;
        } |> Option.some
      | _-> None

    let vcj node=
      match node with
      | `El (((_ns,"vcj"), _attrs), nodes)->
        let open Stroke in
        let vcj_start= Xml.get_point "start" nodes
        and v_length= Xml.get_length "length" nodes
        and h_length= Xml.get_length "length" nodes
        and a_radius= Xml.get_length_adjust "a_radius" nodes
        and end_= Xml.get_point_adjust "end_" nodes in
        Vcj {
          vcj_start;
          v_length;
          h_length;
          a_radius;
          end_;
        } |> Option.some
      | _-> None

    let tu node=
      match node with
      | `El (((_ns,"tu"), _attrs), nodes)->
        let open Stroke in
        let tu_start= Xml.get_point "start" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and t_end= Xml.get_point "t_end" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Tu {
          tu_start;
          ctrl1;
          ctrl2;
          t_end;
          end_;
        } |> Option.some
      | _-> None

    let th node=
      match node with
      | `El (((_ns,"th"), _attrs), nodes)->
        let open Stroke in
        let th_start= Xml.get_point "start" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and t_end= Xml.get_point "t_end" nodes
        and length= Xml.get_length_adjust "length" nodes in
        Th {
          th_start;
          ctrl1;
          ctrl2;
          t_end;
          length;
        } |> Option.some
      | _-> None

    let td node=
      match node with
      | `El (((_ns,"td"), _attrs), nodes)->
        let open Stroke in
        let td_start= Xml.get_point "start" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and t_end= Xml.get_point "t_end" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Td {
          td_start;
          ctrl1;
          ctrl2;
          t_end;
          end_;
        } |> Option.some
      | _-> None

    let wtd node=
      match node with
      | `El (((_ns,"wtd"), _attrs), nodes)->
        let open Stroke in
        let wtd_start= Xml.get_point "start" nodes
        and v_length= Xml.get_length_adjust "v_length" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and t_end= Xml.get_point "t_end" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Wtd {
          wtd_start;
          v_length;
          ctrl1;
          ctrl2;
          t_end;
          end_;
        } |> Option.some
      | _-> None

    let tht node=
      match node with
      | `El (((_ns,"tht"), _attrs), nodes)->
        let open Stroke in
        let tht_start= Xml.get_point "start" nodes
        and t1_ctrl1= Xml.get_point_adjust "t1_ctrl1" nodes
        and t1_ctrl2= Xml.get_point_adjust "t1_ctrl2" nodes
        and t1_end= Xml.get_point "t1_end" nodes
        and length= Xml.get_length "length" nodes
        and t2_ctrl1= Xml.get_point_adjust "t2_ctrl1" nodes
        and t2_ctrl2= Xml.get_point_adjust "t2_ctrl2" nodes
        and end_= Xml.get_point "end" nodes
        in
        Tht {
          tht_start;
          t1_ctrl1;
          t1_ctrl2;
          t1_end;
          length;
          t2_ctrl1;
          t2_ctrl2;
          end_;
        } |> Option.some
      | _-> None

    let thtj node=
      match node with
      | `El (((_ns,"thtj"), _attrs), nodes)->
        let open Stroke in
        let thtj_start= Xml.get_point "start" nodes
        and t1_ctrl1= Xml.get_point_adjust "t1_ctrl1" nodes
        and t1_ctrl2= Xml.get_point_adjust "t1_ctrl2" nodes
        and t1_end= Xml.get_point "t1_end" nodes
        and length= Xml.get_length "length" nodes
        and t2_ctrl1= Xml.get_point_adjust "t2_ctrl1" nodes
        and t2_ctrl2= Xml.get_point_adjust "t2_ctrl2" nodes
        and t2_end= Xml.get_point "t2_end" nodes
        and end_= Xml.get_point_adjust "end" nodes
        in
        Thtj {
          thtj_start;
          t1_ctrl1;
          t1_ctrl2;
          t1_end;
          length;
          t2_ctrl1;
          t2_ctrl2;
          t2_end;
          end_;
        } |> Option.some
      | _-> None

    let tj node=
      match node with
      | `El (((_ns,"tj"), _attrs), nodes)->
        let open Stroke in
        let tj_start= Xml.get_point "start" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and t_end= Xml.get_point "t_end" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Tj {
          tj_start;
          ctrl1;
          ctrl2;
          t_end;
          end_;
        } |> Option.some
      | _-> None

    let cj node=
      match node with
      | `El (((_ns,"cj"), _attrs), nodes)->
        let open Stroke in
        let cj_start= Xml.get_point "start" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and t_end= Xml.get_point "t_end" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Cj {
          cj_start;
          ctrl1;
          ctrl2;
          t_end;
          end_;
        } |> Option.some
      | _-> None

    let fpj node=
      match node with
      | `El (((_ns,"fpj"), _attrs), nodes)->
        let open Stroke in
        let fpj_start= Xml.get_point "start" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and t_end= Xml.get_point "t_end" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Fpj {
          fpj_start;
          ctrl1;
          ctrl2;
          t_end;
          end_;
        } |> Option.some
      | _-> None

    let pj node=
      match node with
      | `El (((_ns,"pj"), _attrs), nodes)->
        let open Stroke in
        let pj_start= Xml.get_point "start" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and t_end= Xml.get_point "t_end" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Pj {
          pj_start;
          ctrl1;
          ctrl2;
          t_end;
          end_;
        } |> Option.some
      | _-> None

    let thtaj node=
      match node with
      | `El (((_ns,"thtaj"), _attrs), nodes)->
        let open Stroke in
        let thtaj_start= Xml.get_point "start" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_point_adjust "ctrl2" nodes
        and t1_end= Xml.get_point "t1_end" nodes
        and h1_length= Xml.get_length "h1_length" nodes
        and t2_end= Xml.get_point "t2_end" nodes
        and h2_length= Xml.get_length "h2_length" nodes
        and a_radius= Xml.get_length_adjust "a_radius" nodes
        and end_= Xml.get_point_adjust "end" nodes
        in
        Thtaj {
          thtaj_start;
          ctrl1;
          ctrl2;
          t1_end;
          h1_length;
          t2_end;
          h2_length;
          a_radius;
          end_;
        } |> Option.some
      | _-> None

    let tod node=
      match node with
      | `El (((_ns,"tod"), _attrs), nodes)->
        let open Stroke in
        let tod_start= Xml.get_point "start" nodes
        and ctrl1= Xml.get_point_adjust "ctrl1" nodes
        and ctrl2= Xml.get_length_adjust "ctrl2" nodes
        and bottom= Xml.get_point "bottom" nodes
        and ctrl3= Xml.get_length_adjust "ctrl3" nodes
        and ctrl4= Xml.get_length_adjust "ctrl4" nodes
        and left= Xml.get_point "left" nodes
        and end_= Xml.get_point_adjust "end" nodes in
        Tod {
          tod_start;
          ctrl1;
          ctrl2;
          bottom;
          ctrl3;
          ctrl4;
          left;
          end_;
        } |> Option.some
      | _-> None
  end

  let get_stroke nodes _attrs=
    match
      List.find
        (function
        | `El _-> true
        | `Data _-> false)
        nodes
    with
    | `El (((_ns,name), _attrs), _nodes) as node->
      let open GetStroke in
      (match (match Stroke.tag_of_string name with
      | S_h-> h node
      | S_sh-> sh node
      | S_u-> u node
      | S_du-> du node
      | S_v-> v node
      | S_sv-> sv node
      | S_rsv-> rsv node
      | S_t-> t' node
      | S_ft-> ft node
      | S_wt-> wt node
      | S_d-> d node
      | S_ed-> ed node
      | S_ld-> ld node
      | S_wd-> wd node
      | S_p-> p node
      | S_up-> up node
      | S_hp-> hp node
      | S_fp-> fp node
      | S_ufp-> ufp node
      | S_c-> c node
      | S_a-> a node
      | S_o-> o node
      | S_hj-> hj node
      | S_uj-> uj node
      | S_ht-> ht node
      | S_hsv-> hsv node
      | S_hv-> hv node
      | S_hvj-> hvj node
      | S_htj-> htj node
      | S_utj-> utj node
      | S_hvh-> hvh node
      | S_hvu-> hvu node
      | S_ha-> ha node
      | S_haj-> haj node
      | S_hpj-> hpj node
      | S_htaj-> htaj node
      | S_htc-> htc node
      | S_htht-> htht node
      | S_htcj-> htcj node
      | S_hvhv-> hvhv node
      | S_hthtj-> hthtj node
      | S_vu-> vu node
      | S_vh-> vh node
      | S_va-> va node
      | S_vaj-> vaj node
      | S_vhv-> vhv node
      | S_vht-> vht node
      | S_vhtj-> vhtj node
      | S_vj-> vj node
      | S_vc-> vc node
      | S_vcj-> vcj node
      | S_tu-> tu node
      | S_th-> th node
      | S_td-> td node
      | S_wtd-> wtd node
      | S_tht-> tht node
      | S_thtj-> thtj node
      | S_tj-> tj node
      | S_cj-> cj node
      | S_fpj-> fpj node
      | S_pj-> pj node
      | S_thtaj-> thtaj node
      | S_tod-> tod node
      | exception _-> None)
      with exception _-> None | stroke-> stroke)
    | `Data _-> None

  let get_ref attrs=
    match ListLabels.fold_left
      attrs
      ~init:{code_point=None; x=None; y=None; width=None; height=None}
      ~f:(fun (acc:ref_unknown) attr->
        let ((_ns, name), value)= attr in
        match name with
        | "unicode"-> { acc with code_point= Some (code_point_of_string value) }
        | "x"-> { acc with x= Some (float_of_string value) }
        | "y"-> { acc with y= Some (float_of_string value) }
        | "width"-> { acc with width= Some (float_of_string value) }
        | "height"-> { acc with height= Some (float_of_string value) }
        | _-> acc)
    with
    | {
        code_point= Some code_point;
        x= Some x;
        y= Some y;
        width= Some width;
        height= Some height;
      }->
      let frame:frame= { x; y; width; height; } in
      {
        code_point;
        frame;
      }
    | _-> failwith "get_ref"

  let get_character attrs=
    match ListLabels.fold_left
      attrs
      ~init:{code_point=None; x=None; y=None; width=None; height=None}
      ~f:(fun (acc:ref_unknown) attr->
        let ((_ns, name), value)= attr in
        match name with
        | "utf8"-> { acc with code_point= Some (code_point_of_utf8 value) }
        | "x"-> { acc with x= Some (float_of_string value) }
        | "y"-> { acc with y= Some (float_of_string value) }
        | "width"-> { acc with width= Some (float_of_string value) }
        | "height"-> { acc with height= Some (float_of_string value) }
        | _-> acc)
    with
    | {
        code_point= Some code_point;
        x= Some x;
        y= Some y;
        width= Some width;
        height= Some height;
      }->
      let frame:frame= { x; y; width; height; } in
      {
        code_point;
        frame;
      }
    | _-> failwith "get_character"

  let of_xml_nodes nodes=
    let attrs, gsd= Ezxmlm.member_with_attr "gsd" nodes in
    let (version_major, version_minor)= attrs |> Ezxmlm.get_attr "version" |> version_of_string in
    let attrs, glyph= Ezxmlm.member_with_attr "glyph" gsd in
    let code_point= attrs |> Ezxmlm.get_attr "unicode" |> code_point_of_string in
    let transform= (try attrs |> Ezxmlm.get_attr "transform" with Not_found-> "none")
      |> transform_of_string in
    let elements= List.filter_map (fun node->
      match node with
      | `El (((_ns,name), attrs), nodes)->
        (match name with
        | "stroke"-> get_stroke nodes attrs |> Option.map (fun s-> Stroke s)
        | "ref"-> Some (Ref (get_ref attrs))
        | "character"-> Some (Ref (get_character attrs))
        | _-> None)
      | `Data _-> None)
      glyph
    in
    {
      version_major;
      version_minor;
      code_point;
      transform;
      elements;
    }


  let of_string string=
    let _dtd, nodes= Ezxmlm.from_string string in
    of_xml_nodes nodes

  let load_file path=
    In_channel.with_open_text path @@ fun chan->
    let _dtd, nodes= Ezxmlm.from_channel chan in
    of_xml_nodes nodes

end

type gsd= {
  version_major: int;
  version_minor: int;
  code_point: code_point;
  transform: transform;
  elements: element list;
}
and subgsd= { gsd: gsd; gframe: frame }
and fstroke= { stroke: stroke; sframe: frame }
and element=
  | Stroke of fstroke
  | SubGsd of subgsd

let gsd_frame gsd=
  ListLabels.fold_left gsd.elements
    ~init:frame_dummy
    ~f:(fun acc element->
      let frame=
        match element with
        | Stroke fstroke-> fstroke.sframe
        | SubGsd subgsd-> subgsd.gframe
      in
      let x= min acc.x frame.x
      and y= min acc.y frame.y in
      let max_x= max (acc.x+.acc.width) (frame.x+.frame.width)
      and max_y= max (acc.y+.acc.height) (frame.y+.frame.height) in
      let width= max_x-.x |> max 1.
      and height= max_y-.y |> max 1. in
      {
        x; y; width; height;
      })

let calc_size gsd: size=
  let frame= gsd_frame gsd in
  let width= frame.width
  and height= frame.height in
  { width; height }

let rec string_of_gsd ?(indent=0) gsd=
  let indent_str= String.make indent ' ' in
  let elements= List.map (string_of_element ~indent:(indent+2)) gsd.elements |> String.concat "\n" in
  sprintf "%s{ version: %d.%d; unicode: %s; elements:\n%s\n%s}"
    indent_str
    gsd.version_minor gsd.version_minor
    (string_of_code_point gsd.code_point)
    elements
    indent_str
and string_of_element ?(indent=0) elem=
  let indent_str= String.make indent ' ' in
  match elem with
  | Stroke stroke-> indent_str ^ (Stroke.to_string stroke.stroke)
  | SubGsd subgsd->
    let gsd= string_of_gsd ~indent:(indent+2) subgsd.gsd
    and frame= frame_to_string subgsd.gframe in
    sprintf "%s{ frame: %s; gsd:\n%s\n%s}" indent_str frame gsd indent_str

let rec load_file ~dir ?(filename="default.xml") code_point=
  let ( / ) = Filename.concat in
  let gsd_raw= Raw.load_file (dir / path_of_code_point code_point / filename) in
  let elements= gsd_raw.elements |> List.map (function
    | Raw.Ref ref-> SubGsd { gsd= load_file ~dir ~filename ref.code_point; gframe= ref.frame }
    | Raw.Stroke stroke-> Stroke { stroke; sframe= Stroke.to_frame stroke }
    )
  in
  {
    version_major= gsd_raw.version_major;
    version_minor= gsd_raw.version_minor;
    code_point= gsd_raw.code_point;
    transform= gsd_raw.transform;
    elements;
  }

let of_string ~dir ?(filename="default.xml") string=
  let gsd_raw= Raw.of_string string in
  let elements= gsd_raw.elements |> List.map (function
    | Raw.Ref ref-> SubGsd { gsd= load_file ~dir ~filename ref.code_point; gframe= ref.frame }
    | Raw.Stroke stroke-> Stroke { stroke; sframe= Stroke.to_frame stroke }
    )
  in
  {
    version_major= gsd_raw.version_major;
    version_minor= gsd_raw.version_minor;
    code_point= gsd_raw.code_point;
    transform= gsd_raw.transform;
    elements;
  }

(*
let pos_ratio_adjust ~(pos_ratio:pos_ratio) stroke=
  let pos= pos_ratio.pos
  and ratio= pos_ratio. ratio in
  let stroke= Stroke.Transform.translate ~d:{x= pos.pos_x; y= pos.pos_y} stroke in
  let frame= Stroke.to_frame stroke in
  Stroke.Transform.scale stroke
    ~origin:{x= frame.x; y= frame.y}
    ~r:{x= ratio.ratio_x; y= ratio.ratio_y}
*)

(** Return the svg outline of the gsd. Note: this function only works with god without any transformed Components inside, or an Invalid_argument exception is raised *)
let rec gsd_flatten ?(pos_ratio=pos_ratio_default) gsd=
  match gsd.transform with
  | MirrorHorizontal | MirrorVertical | Rotate180-> invalid_arg "transform"
  | NoTransform->
  let elements= ListLabels.map
    gsd.elements
    ~f:(fun element->
      match element with
      | Stroke fstroke-> [{ fstroke with sframe= pos_ratio_adjust ~pos_ratio fstroke.sframe}]
      | SubGsd subgsd->
        let size= calc_size subgsd.gsd in
        let ratio= {
          ratio_x= subgsd.gframe.width /. size.width;
          ratio_y= subgsd.gframe.height /. size.height;
        } in
        let ratio_final= {
          ratio_x= ratio.ratio_x *. pos_ratio.ratio.ratio_x;
          ratio_y= ratio.ratio_y *. pos_ratio.ratio.ratio_y;
        } in
        let pos_x=
          subgsd.gframe.x
            *. pos_ratio.ratio.ratio_x
            +. pos_ratio.pos.pos_x
        and pos_y=
          subgsd.gframe.y
            *. pos_ratio.ratio.ratio_y
            +. pos_ratio.pos.pos_y
        in
        let pos_ratio= {
          pos= {pos_x; pos_y};
          ratio= ratio_final;
        } in
        gsd_flatten ~pos_ratio subgsd.gsd)
  in
  List.concat elements

let svg_of_stroke stroke=
  let svg: Svg.t=
    let sub= stroke |> Stroke.to_path |> Smaji_glyph_path.Svg.Svg_path.sub_of_path in
    let svg_path= [sub] in
    let paths= [svg_path] in
    let frame, _latest= Smaji_glyph_path.Svg.Svg_path.get_frame_sub sub in
    let viewBox= Smaji_glyph_path.Svg.ViewBox.{
      min_x= frame.x;
      min_y= frame.y;
      width= max 1. (frame.width);
      height= max 1. (frame.height);
    }
    in
    Smaji_glyph_path.Svg.{ viewBox; paths }
  in
  svg

let paths_of_stroke stroke=
  Stroke.to_path stroke

let fstroke_to_stroke fstroke=
  let target= fstroke.sframe in
  let frame= Stroke.to_frame fstroke.stroke in
  let stroke= fstroke.stroke in
  let origin= Point.{ x= target.x; y= target.y } in
  let d= Point.{ x= target.x -. frame.x; y= target.y -. frame.y } in
  let r= Point.{ x= target.width /. frame.width; y= target.height /. frame.height } in
  let stroke = stroke
    |> Stroke.Transform.translate ~d
    |> Stroke.Transform.scale ~origin ~r
  in
  (* DEBUG
  let result= Stroke.to_frame stroke in
  printf "target %f %f %f %f\n" target.x target.y target.width target.height;
  printf "frame %f %f %f %f\n" frame.x frame.y frame.width frame.height;
  printf "pos ratio %f %f %f %f\n" d.x d.y r.x r.y;
  printf "result %f %f %f %f\n" result.x result.y result.width result.height;
  *)
  stroke

(** Return the svg outline of the gsd. Note: this function only works with god without any transformed Components inside, or an Invalid_argument exception is raised *)
let svg_of_gsd gsd=
  let viewBox= Smaji_glyph_path.Svg.ViewBox.{ min_x= 0.; min_y= 0.; width= 0.; height= 0.; }
  and paths= [gsd
    |> gsd_flatten
    |> List.map (fun fstroke-> fstroke |> fstroke_to_stroke |> Stroke.to_path |> Svg.Svg_path.sub_of_path)
    ]
  in
  let svg= Smaji_glyph_path.Svg.{ viewBox; paths } in
  Smaji_glyph_path.Svg.Adjust.viewBox_fitFrame_reset svg

let outline_svg_of_gsd ?padding ?weight gsd=
  let frame= gsd_frame gsd in
  let size= { width=frame.width; height= frame.height } in
  let padding= max 1. @@
    match padding with
    | Some padding-> padding
    | None-> (size.height +. size.width) /. 10. /. 2.
  in
  let weight=
    match weight with
    | Some weight-> weight
    | None-> Utils.string_of_float (padding /. 2.)
  in
  let rec svg_of_gsd ?(indent=0) ?(pos_ratio=pos_ratio_default) gsd=
    let elem_indent= indent +
      match gsd.transform with
      | NoTransform-> 0
      | _-> 2
    in
    let elem_indent_str0= String.make elem_indent ' '
    and elem_indent_str1= String.make (elem_indent+2) ' ' in
    let indent_str0= String.make indent ' ' in
    let elements= ListLabels.map gsd.elements ~f:(fun element->
      match element with
      | Stroke fstroke->
        let fstroke= { fstroke with
          sframe= pos_ratio_adjust ~pos_ratio fstroke.sframe
        }
        in
        fstroke
          |> fstroke_to_stroke
          |> Stroke.to_path
          |> Svg.Svg_path.sub_of_path
          |> Svg.Svg_path.sub_to_string_svg ~close:false ~indent:(elem_indent+2)
          |> fun cmd-> sprintf "%s<path fill=\"none\" d=\"\n%s\n%s\"\n%s/>"
            elem_indent_str0
            cmd
            elem_indent_str1
            elem_indent_str0
      | SubGsd subgsd->
        let size= calc_size subgsd.gsd in
        let ratio= {
          ratio_x= subgsd.gframe.width /. size.width;
          ratio_y= subgsd.gframe.height /. size.height;
        } in
        let ratio_final= {
          ratio_x= ratio.ratio_x *. pos_ratio.ratio.ratio_x;
          ratio_y= ratio.ratio_y *. pos_ratio.ratio.ratio_y;
        } in
        let pos_x=
          subgsd.gframe.x
            *. pos_ratio.ratio.ratio_x
            +. pos_ratio.pos.pos_x
        and pos_y=
          subgsd.gframe.y
            *. pos_ratio.ratio.ratio_y
            +. pos_ratio.pos.pos_y
        in
        let pos_ratio= {
          pos= {pos_x; pos_y};
          ratio= ratio_final;
        } in
        let indent=
          match gsd.transform with
          | NoTransform-> indent
          | _-> indent + 2
        in
        svg_of_gsd ~indent ~pos_ratio subgsd.gsd)
    in
    let elements_str= String.concat "\n" elements in

    let transform=
      let x= sprintf "translate(%s 0)"
        (Utils.string_of_float
          (-. size.width -. pos_ratio.pos.pos_x))
      and y= sprintf "translate(0 %s)"
        (Utils.string_of_float
          (-. size.height -. pos_ratio.pos.pos_y))
      in
      match gsd.transform with
      | NoTransform-> fun elements-> sprintf "%s" elements
      | MirrorHorizontal-> fun elements->
        let transform= ["scale(-1 1)"; x]
          |> String.concat " "
          |> sprintf {|transform="%s"|}
        in
        sprintf "%s<g %s>\n%s\n%s</g>"
          indent_str0
          transform
          elements
          indent_str0
      | MirrorVertical-> fun elements->
        let transform= ["scale(1 -1)"; y]
          |> String.concat " "
          |> sprintf {|transform="%s"|}
        in
        sprintf "%s<g %s>\n%s\n%s</g>"
          indent_str0
          transform
          elements
          indent_str0
      | Rotate180-> fun elements->
        let transform= ["scale(-1 -1)"; x; y]
          |> String.concat " "
          |> sprintf {|transform="%s"|}
        in
        sprintf "%s<g %s>\n%s\n%s</g>"
          indent_str0
          transform
          elements
          indent_str0
    in
    transform elements_str
  in
  match gsd.version_major, gsd.version_minor with
  | (1, 0) ->
    let svg_str= svg_of_gsd ~indent:4 gsd in
    sprintf
      "<svg viewBox=\"0,0 %s,%s\" xmlns=\"http://www.w3.org/2000/svg\">\n  <g stroke=\"black\" stroke-width=\"%s\">\n%s\n  </g>\n</svg>"
      (Utils.string_of_float (frame.x +. frame.width +. padding*.2.))
      (Utils.string_of_float (frame.y +. frame.height +. padding*.2.))
      weight
      svg_str
  | _-> failwith (sprintf "outline_svg_of_gsd %d %d" gsd.version_major gsd.version_minor)

module StrokeMap= Map.Make(Stroke.Tag)

(* a stroke sample collection, assuming that the frame box is 128x128 *)
let stroke_samples=
  let open Stroke in
  let open Point in
  let open Tag in [
  (S_h, H {
    h_start= {x= 8.; y=8.};
    length= 128.;
  });
  (S_sh, Sh {
    sh_start= {x= 8.; y= 18.};
    end_= {x= 127.; y=8.};
  });
  (S_u, U {
    u_start= {x= 8.; y= 28.};
    end_= {x= 127.; y=8.};
  });
  (S_du, Du {
    du_start= {x= 8.; y= 64.};
    dot= Auto;
    end_= {x= 64.; y=8.};
  });
  (S_v, V {
    v_start= {x= 8.; y= 8.};
    length= 128.;
  });
  (S_sv, Sv {
    sv_start= {x= 26.; y= 8.};
    length= 110.;
    width= 18.;
  });
  (S_rsv, Rsv {
    rsv_start= {x= 8.; y= 8.};
    length= 128.;
    width= 18.;
  });
  (S_t, T {
    t'_start= {x= 127.; y= 8.};
    ctrl1= Auto;
    ctrl2= Auto;
    end_= {x= 8.; y= 127.};
  });
  (S_ft, Ft {
    ft_start= {x= 120.; y= 8.};
    ctrl1= Auto;
    ctrl2= Auto;
    end_= {x= 8.; y= 48.};
  });
  (S_wt, Wt {
    wt_start= {x= 32.; y= 8.};
    v_length= Auto;
    ctrl1= Auto;
    ctrl2= Auto;
    end_= {x= 8.; y= 127.};
  });
  (S_d, D {
    d_start= {x= 8.; y= 8.};
    end_= Auto;
  });
  (S_ed, Ed {
    ed_start= {x= 8.; y= 8.};
    end_= Auto;
  });
  (S_ld, Ld {
    ld_start= {x= 18.; y= 18.};
    end_= Auto;
  });
  (S_wd, Wd {
    wd_start= {x= 8.; y= 8.};
    length= Auto;
  });
  (S_p, P {
    p_start= {x= 8.; y= 8.};
    ctrl1= Auto;
    ctrl2= Auto;
    end_= {x= 127.; y= 127.};
  });
  (S_up, Up {
    up_start= Auto;
    p_start= {x= 18.; y= 8.};
    ctrl1= Auto;
    ctrl2= Auto;
    end_= {x= 120.; y= 120.};
  });
  (S_hp, Hp {
    hp_start= {x= 8.; y= 8.};
    length= 36.;
    ctrl1= Auto;
    ctrl2= Auto;
    end_= {x= 96.; y= 96.};
  });
  (S_fp, Fp {
    fp_start= {x= 8.; y= 8.};
    ctrl1= Auto;
    ctrl2= Auto;
    end_= {x= 120.; y= 48.};
  });
  (S_ufp, Ufp {
    ufp_start= {x= 8.; y= 31.};
    p_start= {x= 31.; y= 31.};
    ctrl1= Auto;
    ctrl2= Auto;
    end_= {x= 127.; y= 31.};
  });
  (S_c, C {
    c_start= {x= 8.; y= 8.};
    length= 128.;
    width= 64.;
    ctrl1= Auto;
    ctrl2= Auto;
  });
  (S_a, A {
    a_start= {x= 72.; y= 8.};
    length= 128.;
    width= 64.;
    ctrl1= Auto;
    ctrl2= Auto;
  });
  (S_o, O {
    o_start= {x= 8.; y= 8.};
    length= 128.;
    width= 128.;
    ctrl_h= Auto;
    ctrl_v= Auto;
  });
  (S_hj, Hj {
    hj_start= {x= 8.; y= 8.};
    length= 128.;
    end_= Auto;
  });
  (S_uj, Uj {
    uj_start= {x= 8.; y= 31.};
    u_end= {x= 127.; y= 8.};
    end_= Auto;
  });
  (S_ht, Ht {
    ht_start= {x= 12.; y= 8.};
    length= 76.;
    ctrl1= Auto;
    ctrl2= Auto;
    end_= {x= 8.; y= 108.};
  });
  (S_hsv, Hsv {
    hsv_start= {x= 8.; y= 8.};
    h_length= 128.;
    end_= Auto;
  });
  (S_hv, Hv {
    hv_start= {x= 8.; y= 8.};
    h_length= 96.;
    v_length= Auto;
  });
  (S_hvj, Hvj {
    hvj_start= {x= 8.; y= 8.};
    h_length= 128.;
    v_length= Auto;
    end_= Auto;
  });
  (S_htj, Htj {
    htj_start= {x= 8.; y= 8.};
    length= 110.;
    ctrl1= Auto;
    ctrl2= Auto;
    t_end= {x= 81.; y= 110.};
    end_= Auto;
  });
  (S_utj, Utj {
    utj_start= {x= 8.; y= 32.};
    t_start= {x= 120.; y= 8.};
    ctrl1= Auto;
    ctrl2= Auto;
    t_end= {x= 96.; y= 64.};
    end_= Auto;
  });
  (S_hvh, Hvh {
    hvh_start= {x= 8.; y= 8.};
    h1_length= 60.;
    v_length= 120.;
    h2_length= 60.;
  });
  (S_hvu, Hvu {
    hvu_start= {x= 8.; y= 8.};
    h_length= 64.;
    v_length= 128.;
    end_= Auto;
  });
  (S_ha, Ha {
    ha_start= {x= 8.; y= 8.};
    h1_length= 56.;
    v_length= 96.;
    h2_length= 56.;
    a_radius= Auto;
  });
  (S_haj, Haj {
    haj_start= {x= 8.; y= 8.};
    h1_length= 56.;
    v_length= 96.;
    h2_length= 56.;
    a_radius= Auto;
    end_= Auto;
  });
  (S_hpj, Hpj {
    hpj_start= {x= 8.; y= 8.};
    length= 36.;
    ctrl1= Auto;
    ctrl2= Auto;
    p_end= {x= 96.; y= 96.};
    end_= Auto;
  });
  (S_htaj, Htaj {
    htaj_start= {x= 16.; y= 8.};
    h1_length= 96.;
    t_end= {x= 16.; y= 112.};
    h2_length= 112.;
    a_radius= Auto;
    end_= Auto;
  });
  (S_htc, Htc {
    htc_start= {x= 8.; y= 8.};
    h_length= 64.;
    t_ctrl1= Auto;
    t_ctrl2= Auto;
    t_end= {x= 40.; y= 32.};
    c_ctrl1= Auto;
    c_ctrl2= Auto;
    c_end= {x= 36.; y= 96.};
  });
  (S_htht, Htht {
    htht_start= {x= 8.; y= 8.};
    h1_length= 64.;
    t1_ctrl1= Auto;
    t1_ctrl2= Auto;
    t1_end= {x= 32.; y= 48.};
    h2_length= 64.;
    t2_ctrl1= Auto;
    t2_ctrl2= Auto;
    end_= {x= 16.; y= 110.};
  });
  (S_htcj, Htcj {
    htcj_start= {x= 8.; y= 8.};
    h_length= 64.;
    t_ctrl1= Auto;
    t_ctrl2= Auto;
    t_end= {x= 40.; y= 32.};
    c_ctrl1= Auto;
    c_end= {x= 44.; y= 96.};
    c_ctrl2= Auto;
    end_= Auto;
  });
  (S_hvhv, Hvhv {
    hvhv_start= {x= 8.; y= 8.};
    h1_length= 64.;
    v1_length= 64.;
    h2_length= 64.;
    v2_length= 64.;
  });
  (S_hthtj, Hthtj {
    hthtj_start= {x= 8.; y= 8.};
    h1_length= 64.;
    t1_ctrl1= Auto;
    t1_ctrl2= Auto;
    t1_end= {x= 32.; y= 48.};
    h2_length= 64.;
    t2_ctrl1= Auto;
    t2_ctrl2= Auto;
    t2_end= {x= 64.; y= 110.};
    end_= Auto;
  });
  (S_vu, Vu {
    vu_start= {x= 8.; y= 8.};
    length= 128.;
    end_= Auto;
  });
  (S_vh, Vh {
    vh_start= {x= 8.; y= 8.};
    v_length= 128.;
    h_length= 128.;
  });
  (S_va, Va {
    va_start= {x= 8.; y= 8.};
    v_length= 112.;
    h_length= 112.;
    a_radius= Auto;
  });
  (S_vaj, Vaj {
    vaj_start= {x= 8.; y= 8.};
    v_length= 112.;
    h_length= 112.;
    a_radius= Auto;
    end_= Auto;
  });
  (S_vhv, Vhv {
    vhv_start= {x= 8.; y= 8.};
    v1_length= 64.;
    h_length= 96.;
    v2_length= 64.;
  });
  (S_vht, Vht {
    vht_start= {x= 8.; y= 8.};
    v_length= 64.;
    h_length= 96.;
    ctrl1= Auto;
    ctrl2= Auto;
    end_= {x= 8.; y= 128.};
  });
  (S_vhtj, Vhtj {
    vhtj_start= {x= 8.; y= 8.};
    v_length= 56.;
    h_length= 81.;
    ctrl1= Auto;
    ctrl2= Auto;
    t_end= {x= 56.; y= 120.};
    end_= Auto;
  });
  (S_vj, Vj {
    vj_start= {x= 16.; y= 8.};
    length= 128.;
    end_= Auto;
  });
  (S_vc, Vc {
    vc_start= {x= 120.; y= 8.};
    v_length= 112.;
    h_length= 90.;
    a_radius= Auto;
  });
  (S_vcj, Vcj {
    vcj_start= {x= 120.; y= 8.};
    v_length= 112.;
    h_length= 90.;
    a_radius= Auto;
    end_= Auto;
  });
  (S_tu, Tu {
    tu_start= {x= 64.; y= 8.};
    ctrl1= Auto;
    ctrl2= Auto;
    t_end= {x= 8.; y= 127.};
    end_= Auto;
  });
  (S_th, Th {
    th_start= {x= 64.; y= 8.};
    ctrl1= Auto;
    ctrl2= Auto;
    t_end= {x= 8.; y= 127.};
    length= Auto;
  });
  (S_td, Td {
    td_start= {x= 56.; y= 8.};
    ctrl1= Auto;
    ctrl2= Auto;
    t_end= {x= 8.; y= 63.};
    end_= Auto;
  });
  (S_wtd, Wtd {
    wtd_start= {x= 48.; y= 8.};
    v_length= Auto;
    ctrl1= Auto;
    ctrl2= Auto;
    t_end= {x= 8.; y= 64.};
    end_= Auto;
  });
  (S_tht, Tht {
    tht_start= {x= 60.; y= 8.};
    t1_ctrl1= Auto;
    t1_ctrl2= Auto;
    t1_end= {x= 8.; y= 60.};
    length= 96.;
    t2_ctrl1= Auto;
    t2_ctrl2= Auto;
    end_= {x= 8.; y= 127.};
  });
  (S_thtj, Thtj {
    thtj_start= {x= 32.; y= 8.};
    t1_ctrl1= Auto;
    t1_ctrl2= Auto;
    t1_end= {x= 8.; y= 60.};
    length= 81.;
    t2_ctrl1= Auto;
    t2_ctrl2= Auto;
    t2_end= {x= 64.; y= 120.};
    end_= Auto;
  });
  (S_tj, Tj {
    tj_start= {x= 120.; y= 8.};
    ctrl1= Auto;
    ctrl2= Auto;
    t_end= {x= 32.; y= 120.};
    end_= Auto;
  });
  (S_cj, Cj {
    cj_start= {x= 16.; y= 8.};
    ctrl1= Auto;
    ctrl2= Auto;
    t_end= {x= 32.; y= 127.};
    end_= Auto;
  });
  (S_fpj, Fpj {
    fpj_start= {x= 8.; y= 8.};
    ctrl1= Auto;
    ctrl2= Auto;
    t_end= {x= 120.; y= 48.};
    end_= Auto;
  });
  (S_pj, Pj {
    pj_start= {x= 8.; y= 8.};
    ctrl1= Auto;
    ctrl2= Auto;
    t_end= {x= 64.; y= 127.};
    end_= Auto;
  });
  (S_thtaj, Thtaj {
    thtaj_start= {x= 32.; y= 8.};
    ctrl1= Auto;
    ctrl2= Auto;
    t1_end= {x= 12.; y= 32.};
    h1_length= 81.;
    t2_end= {x= 16.; y= 96.;};
    h2_length= 90.;
    a_radius= Auto;
    end_= Auto;
  });
  (S_tod, Tod {
    tod_start= {x= 112.; y= 16.};
    ctrl1= Auto;
    ctrl2= Auto;
    bottom= {x= 60.; y= 120.};
    ctrl3= Auto;
    ctrl4= Auto;
    left= {x= 8.; y= 64.;};
    end_= Auto;
  });
  ] |> List.to_seq |> StrokeMap.of_seq

(* DEBUG
  let test_stroke_samples= StrokeMap.iter
    (fun tag stroke->
      let gsd= {
        version_major= 1;
        version_minor= 0;
        code_point= (0,0);
        transform= NoTransform;
        elements= [Stroke { stroke; sframe= Stroke.to_frame stroke }];
      } in
      let svg= outline_svg_of_gsd ~padding:8. ~weight:"8" gsd in
      Out_channel.(with_open_text
        (Stroke.Tag.t_to_string tag ^ ".svg")
        (Fun.flip output_string svg))
      )
    stroke_samples
*)

type glif_of_gsd=
  | Glif of Glif.t
  | Wrapped of { wrap: Glif.t; content: Glif.t }

let outline_glif_of_gsd gsd=
  let rec glif_elts_of_gsd ?(pos_ratio=pos_ratio_default) gsd=
    let elements= ListLabels.map gsd.elements ~f:(fun element->
      match element with
      | Stroke fstroke->
        let fstroke= { fstroke with
          sframe= pos_ratio_adjust ~pos_ratio fstroke.sframe
        }
        in
        let points=
          fstroke
            |> fstroke_to_stroke
            |> Stroke.to_path
            |> Glif.points_of_path
        in
        [Glif.Contour {
          identifier= None;
          points;
        }]
      | SubGsd subgsd->
        let size= calc_size subgsd.gsd in
        let ratio= {
          ratio_x= subgsd.gframe.width /. size.width;
          ratio_y= subgsd.gframe.height /. size.height;
        } in
        let ratio_final= {
          ratio_x= ratio.ratio_x *. pos_ratio.ratio.ratio_x;
          ratio_y= ratio.ratio_y *. pos_ratio.ratio.ratio_y;
        } in
        let pos_x=
          subgsd.gframe.x
            *. pos_ratio.ratio.ratio_x
            +. pos_ratio.pos.pos_x
        and pos_y=
          subgsd.gframe.y
            *. pos_ratio.ratio.ratio_y
            +. pos_ratio.pos.pos_y
        in
        let pos_ratio= {
          pos= {pos_x; pos_y};
          ratio= ratio_final;
        } in
        glif_elts_of_gsd ~pos_ratio subgsd.gsd)
    in
    List.concat elements;
  in
  let glif_of_gsd ?(wrapped=true) gsd=
    let size= calc_size gsd in
    let name=
      let base= string_of_code_point gsd.code_point in
      if wrapped then
        base ^ "_content"
      else
        base
    in
    let format= 2
    and formatMinor= 0
    and advance= Glif.{ width= size.width; height= size.height }
    and unicodes= let (core,_)= gsd.code_point in [core]
    and elements= glif_elts_of_gsd gsd
    in
    Glif.{
      name;
      format;
      formatMinor;
      advance;
      unicodes;
      elements;
    }

  in
  let transform_wrap gsd=
    let size= calc_size gsd in
    let code_point= gsd.code_point in
    let wrap=
      let xScale, yScale, xOffset, yOffset=
        match gsd.transform with
        | NoTransform-> (1., 1., 0., 0.)
        | MirrorHorizontal-> (-1., 1., -. size.width, 0.)
        | MirrorVertical-> (1., -1., 0., -. size.height)
        | Rotate180-> (-1., -1., -. size.width, -. size.height)
      in
      Glif.Component {
        base= Some (string_of_code_point code_point ^ "_content");
        xScale;
        xyScale= 0.;
        yxScale= 0.;
        yScale;
        xOffset;
        yOffset;
        identifier= None;
      }
    in
    let (core,_)= code_point in
    Glif.{
      name= string_of_code_point code_point;
      format= 2;
      formatMinor= 0;
      advance= {width= 0.; height= 0.};
      unicodes= [core];
      elements= [wrap];
    }
  in
  let gen_glif ()=
    match gsd.transform with
    | NoTransform-> Glif (glif_of_gsd ~wrapped:false gsd)
    | MirrorHorizontal->
      let content= glif_of_gsd gsd in
      let wrap= transform_wrap gsd in
      Wrapped { wrap; content }
      (* ["scale(-1 1)"; x] *)
    | MirrorVertical->
      let content= glif_of_gsd gsd in
      let wrap= transform_wrap gsd in
      Wrapped { wrap; content }
      (* ["scale(1 -1)"; y] *)
    | Rotate180->
      let content= glif_of_gsd gsd in
      let wrap= transform_wrap gsd in
      Wrapped { wrap; content }
      (* ["scale(-1 -1)"; x; y] *)
  in
  match gsd.version_major, gsd.version_minor with
  | (1, 0) ->
    gen_glif ()
  | _-> failwith (sprintf "outline_glif_of_gsd %d %d" gsd.version_major gsd.version_minor)

