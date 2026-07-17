(*
 * stroke_black.ml
 * -----------
 * Copyright : (c) 2023 - 2025, smaji.org
 * Copyright : (c) 2023 - 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_gsd.
 *)

open Stroke_def

module To_path = struct
  open GlyphPath.Point
  module Path = GlyphPath.Path

  let circle_ctrl_distance ?(seg=4) r=
    r *. 4. /. 3. *. tan(Float.pi /. (2. *. (Float.of_int seg)))

  (* downward curve, e.g. t or p
     [ratio] is based on the length between [start] and [end']
   *)
  let template_curve ~start ~end' ~ratio=
    let open Point in
    let open Ops in
    let open Float in
    let vec, length=
      let vec= end' - start in
      let length= (pow vec.x 2.) +. (pow vec.y 2.) |> sqrt in
      (vec, length)
    in
    let open Point in
    let angle= angle vec -. angle {x=0.;y=1.} in
    let rotate= rotate ~angle in
    let point= rotate (ratio *< length) in
    start + point

  (*
    type h= {
      h_start: point;
      length: float;
    } (* Horizontal *)
      (* 1. the length is at least 4 times its weight *)
  *)

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

  let from_ufp ?(width=8.) ufp=
    let open Path in
    let p_start= ufp.p_start in
    let start= get_adjust ufp.ufp_start
      ~f:(fun()->
        let x= p_start.x -. 2. *. width
        and y= p_start.y +. 2. *. width in
        Point.{x;y})
    in
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
      ctrl1: float adjust;
      ctrl2: float adjust;
      end_: point;
    } (* Clockwise curve *)
  *)

  let from_c c=
    let open Path in
    let open Point in
    let start= c.c_start
    and end'= c.end_ in
    let vec_c= Ops.(end' - start) in
    let vec_anti90= Matrix.(apply anticlock_90 vec_c) in
    let length= distance vec_c in
    let r= length /. 2. in
    let ctrl1, ctrl2=
      let ctrl_vec=
        Line.extended_vec ~vec:vec_anti90
          (circle_ctrl_distance ~seg:2 r) in
      let ctrl1= get_adjust c.ctrl1 ~f:(fun ()->
        Ops.(start + ctrl_vec)) in
      let ctrl2= get_adjust c.ctrl2 ~f:(fun ()->
        Ops.(end' + ctrl_vec)) in
      (ctrl1, ctrl2)
    in
    let segments= [Ccurve {ctrl1; ctrl2; end'}] in
    {
      start;
      segments;
    }

  (*
    type a= {
      a_start: point;
      ctrl1: point adjust;
      ctrl2: point adjust;
      end_: point;
    } (* Anticlockwise curve *)
  *)

  let from_a a=
    let open Path in
    let open Point in
    let start= a.a_start
    and end'= a.end_ in
    let vec_c= Ops.(end' - start) in
    let vec_clock90= Matrix.(apply clockwise_90 vec_c) in
    let length= distance vec_c in
    let r= length /. 2. in
    let ctrl1, ctrl2=
      let ctrl_vec=
        Line.extended_vec ~vec:vec_clock90
          (circle_ctrl_distance ~seg:2 r) in
      let ctrl1= get_adjust a.ctrl1 ~f:(fun ()->
        Ops.(start + ctrl_vec)) in
      let ctrl2= get_adjust a.ctrl2 ~f:(fun ()->
        Ops.(end' + ctrl_vec)) in
      (ctrl1, ctrl2)
    in
    let segments= [Ccurve {ctrl1; ctrl2; end'}] in
    {
      start;
      segments;
    }

  (*
    type o= {
      o_start: point;
      width: float;
      end_: point;
    } (* Oval *)
  *)

  let from_o o=
    let open Path in
    let open Point in
    let start= o.o_start
    and end'= o.end_ in
    let vec_o= Ops.(end' - start) in
    let vec_o_rev= neg vec_o in
    let width= o.width
    and length= distance vec_o in
    let vec_clock90= Matrix.(apply clockwise_90 vec_o)
    and vec_anti90= Matrix.(apply anticlock_90 vec_o) in
    let vec_right= Line.extended_vec ~vec:vec_anti90 (width/.2.)
    and vec_left= Line.extended_vec ~vec:vec_clock90 (width/.2.)
    and vec_down= Line.extended_vec ~vec:vec_o (length/.2.)
    and vec_up= Line.extended_vec ~vec:vec_o_rev (length/.2.) in
    let left, right=
      let middle= Ops.((start + end') /< 2.) in
      Ops.(middle + vec_left, middle + vec_right)
    in
    let arc_u_r=
      let ctrl1= Ops.(start + vec_right /< 2.)
      and ctrl2= Ops.(right + vec_up /< 2.)
      and end'= right in
      Ccurve {ctrl1; ctrl2;end'}
    and arc_d_r=
      let ctrl1= Ops.(right + vec_down /< 2.)
      and ctrl2= Ops.(end' + vec_right /< 2.)
      and end'= end' in
      Ccurve {ctrl1; ctrl2;end'}
    and arc_d_l=
      let ctrl1= Ops.(end' + vec_left /< 2.)
      and ctrl2= Ops.(left + vec_down /< 2.)
      and end'= left in
      Ccurve {ctrl1; ctrl2;end'}
    and arc_u_l=
      let ctrl1= Ops.(left + vec_up /< 2.)
      and ctrl2= Ops.(start + vec_left /< 2.)
      and end'= start in
      Ccurve {ctrl1; ctrl2;end'}
    in
    let segments= [arc_u_r; arc_d_r; arc_d_l; arc_u_l] in
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
        let open Ops in
        let line= t_end - t_start in
        let d= pow line.x 2. +. pow line.y 2. |> sqrt in
        { x= t_end.x -. d/.8.; y= t_end.y -. d/.8. })
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
      h2_length: float;
      a_end: point;
      a_radius: float adjust;
      end_: point adjust;
    } (* Horizontal – Throw – Anticlockwise curve – J hook *)
  *)

  let from_htaj (htaj:htaj)=
    let open Point in
    let a_radius= get_adjust htaj.a_radius ~f:(fun ()->
      let avg= (htaj.h1_length +. htaj.h2_length) /. 2. in
      avg /. 6.)
    in
    let start= htaj.htaj_start in
    let h1_end= { start with x= start.x +. htaj.h1_length } in
    let a_end= htaj.a_end in
    let h2_start= { a_end with
      x= a_end.x -. htaj.h2_length;
    } in
    let vec_h2= Ops.(a_end - h2_start) in
    let h2_length= distance vec_h2 in
    let end'= get_adjust htaj.end_ ~f:(fun()->
      { a_end with y= a_end.y -. h2_length *. 0.2}) in
    let vec_h2= Ops.(htaj.a_end - h2_start) in
    let vec_h2_anti90= Matrix.(apply anticlock_90 vec_h2) in
    let a_center= Ops.(h2_start +
      Line.extended_vec ~vec:vec_h2_anti90 a_radius) in
    let vec_center_start= Ops.(h1_end - a_center) in
    let t_end=
      let vec= Matrix.(apply anticlock_90 vec_center_start) in
      Ops.(a_center + Line.extended_vec ~vec a_radius)
    in
    let vec_t= Ops.(t_end - h1_end) in
    let t_length= distance vec_t in
    let t_c= Ops.(
      (t_end+h1_end) /< 2. +
      Line.extended_vec
        ~vec:Matrix.(apply clockwise_90 vec_t)
        (t_length*.0.05)) in
    let h2_c= Ops.(
      (h2_start+a_end) /< 2. +
      Line.extended_vec
        ~vec:Matrix.(apply clockwise_90 vec_h2)
        (h2_length*.0.05)) in
    let a_c=
      let line1= Line.of_points t_c t_end
      and line2= Line.of_points h2_start h2_c in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point)in
    let open Path in
    let segments= [
      Line h1_end;
      Qcurve {ctrl= t_c; end'= t_end};
      Qcurve {ctrl= a_c; end'= h2_start};
      Qcurve {ctrl= h2_c; end'= a_end};
      Line end';
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
      c_ctrl1: point adjust;
      c_ctrl2: point adjust;
      end_: point
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
      let start= htc.t_end and end'= htc.end_ in
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
        let line= Ops.(c_end - c_start) in
        let length= (pow line.x 2.) +. (pow line.y 2.) |> sqrt in
        let d= Float.pow (length /. 4.) 2. /. 2. |> sqrt in
        { x= c_end.x -. d; y= c_end.y -. d; })
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
      let vec= Ops.(cj.c_end - cj.cj_start) in
      (pow vec.x 2.) +. (pow vec.y 2.) |> sqrt
    in
    let open Path in
    let start= cj.cj_start in
    let curve=
      let t= from_t' {
        t'_start= cj.cj_start;
        ctrl1= cj.ctrl2;
        ctrl2= cj.ctrl2;
        end_= cj.c_end
        }
      in
      List.hd t.segments
    in
    let j=
      let end'= get_adjust cj.end_ ~f:(fun ()->
        let t_end= cj.c_end in
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
      end_= fpj.p_end;
    } in
    let fp= from_fp fp in
    let open Float in
    let length=
      let line= Point.Ops.(fpj.p_end - fpj.fpj_start) in
      (pow line.x 2.) +. (pow line.y 2.) |> sqrt
    in
    let j=
      let end'= get_adjust fpj.end_ ~f:(fun ()->
        let d= length /. 4. in
        { fpj.p_end with y= fpj.p_end.y -. d }
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
      end_= pj.p_end;
    } in
    let length=
      let open Float in
      let line= Point.Ops.(pj.p_end - pj.pj_start) in
      (pow line.x 2.) +. (pow line.y 2.) |> sqrt
    in
    let j=
      let end'= get_adjust pj.end_ ~f:(fun ()->
        let d= length /. 4. in
        { pj.p_end with y= pj.p_end.y -. d }
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
      t_end: point;
      h1_length: float;
      h2_length: float;
      a_end: point;
      a_radius: float adjust;
      end_: point adjust;
    } (* Throw – Horizontal – Throw – Anticlockwise curve – J hook *)
  *)

  let from_thtaj thtaj=
    let t= from_t' {
      t'_start= thtaj.thtaj_start;
      ctrl1= thtaj.ctrl1;
      ctrl2= thtaj.ctrl2;
      end_= thtaj.t_end;
    }
    and htaj= from_htaj {
      htaj_start= thtaj.t_end;
      h1_length= thtaj.h1_length;
      a_end= thtaj.a_end;
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
    let c_left_h= get_adjust tod.ctrl2 ~f:(fun ()->
      right_width *. 4. /. 5.)
    and c_right_h= get_adjust tod.ctrl3 ~f:(fun ()->
      left_width /. 2.)
    and c_right_v= get_adjust tod.ctrl4 ~f:(fun ()->
      left_height *. 4. /. 7.)
    in
    let open Path in
    let l1=
      let ctrl1= get_adjust tod.ctrl1 ~f:(fun ()->
        Point.{
          x= start.x -. right_width /. 6.;
          y= start.y +. right_height;
        })
      and ctrl2= { bottom with x= bottom.x +. c_left_h }
      and end'= bottom in
      Ccurve {ctrl1; ctrl2; end'}
    and l2=
      let ctrl1= { bottom with x= bottom.x -. c_right_h }
      and ctrl2= { left with y= left.y +. c_right_v }
      and end'= left in
      Ccurve {ctrl1; ctrl2; end'}
    and l3=
      let ctrl1= { left with y= left.y -. c_right_v }
      and ctrl2= { up with x= up.x -. c_right_h }
      and end'= up in
      Ccurve {ctrl1; ctrl2; end'}
    and l4=
      let ctrl1= { up with x= up.x +. c_left_h }
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

let to_path= Stroke_path To_path.to_path
let to_frame_raw t=
  let frame, _last=t |> To_path.to_path |> Smaji_glyph_path.Path.frame in
  frame
let to_frame= Stroke_frame to_frame_raw

