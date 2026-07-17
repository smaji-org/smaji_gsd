(*
 * stroke_song.ml
 * -----------
 * Copyright : (c) 2023 - 2025, smaji.org
 * Copyright : (c) 2023 - 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_gsd.
 *)

open Stroke_def

open GlyphPath
open Point

module To_path(Width : sig val width : float end) = struct
  module Path = GlyphPath.Path

  let pi= Float.pi

  let circle_ctrl_distance_raw ?(seg=4.) r=
    r *. 4. /. 3. *. tan(pi /. (2. *. seg))

  let circle_ctrl_distance ?(seg=4) r=
    r *. 4. /. 3. *. tan(pi /. (2. *. (Float.of_int seg)))

  (* downward curve, e.g. t or p
     [ratio] is based on the length between [start] and [end']
   *)
  let template_curve ~start ~end' ~ratio=
    let open Float in
    let open PointF in
    let open Ops in
    let vec, length=
      let vec= end' - start in
      let length= (pow vec.x 2.) +. (pow vec.y 2.) |> sqrt in
      (vec, length)
    in
    let open PointF in
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

  let from_h ?(width=Width.width) h=
    let _h= 0.5
    and _a= 1. in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa *. _h in
    let open Path in
    let open Point in
    let start= h.h_start in
    let p0= start in
    let p1= { p0 with x= p0.x +. h.length -. width*._a } in
    let p2= {
      x= p1.x +. width*._a;
      y= p1.y -. width*._a; } in
    let p3= {
      x= p2.x +. width*.(_a+._h);
      y= start.y +. width*._h } in
    let p4= { p3 with
      x= min (p0.x +. width*.2.) p1.x;
      } in
    let c5= { p4 with
      x= p4.x -. width*._sah;
      } in
    let p5= {
      x= p0.x +. width*._sah;
      y= p0.y +. width*._sah;
      } in
    let segments= [
      Line p1;
      Line p2;
      Line p3;
      Line p4;
      Qcurve { ctrl= c5; end'= p5 };
      Line p0
      ] in
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

  let from_sh ?(width=Width.width) sh=
    let _h= 0.5
    and _a= 1. in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa *. _h in
    let open Path in
    let open Point in
    let length= distance ~from:sh.sh_start sh.end_ in
    let adjust p=
      let sub= Ops.(-) sh.end_ sh.sh_start in
      let matrix= Matrix.{
        c1= { r1= sub.x /. length; r2= sub.y /. length };
        c2= { r1= -. sub.y /. length; r2= sub.x /. length};
        }
      in
      Ops.(Matrix.apply matrix (p - sh.sh_start) + sh.sh_start)
    in
    let start= sh.sh_start in
    let p0= start in
    let p1= { p0 with x= p0.x +. length -. width*._a } in
    let p2= {
      x= p1.x +. width*._a;
      y= p1.y -. width*._a; } in
    let p3= {
      x= p2.x +. width*.(_a+._h);
      y= start.y +. width*._h } in
    let p4= { p3 with
      x= p0.x +. width*.2.;
      } in
    let c5= { p4 with
      x= p4.x -. width*._sah;
      } in
    let p5= {
      x= p0.x +. width*._sah;
      y= p0.y +. width*._sah;
      } in
    let p1= adjust p1 and p2= adjust p2 and p3= adjust p3
    and p4= adjust p4 and c5= adjust c5 and p5= adjust p5 in
    let segments= [
      Line p1;
      Line p2;
      Line p3;
      Line p4;
      Qcurve { ctrl= c5; end'= p5 };
      Line p0
      ] in
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
  let from_u ?(width=Width.width) u=
    let open Path in
    let open PointF in
    let start= u.u_start in
    let p0= start in
    let p1= u.end_ in
    let slope=
      let l= Ops.(p1 - p0) in
      l.y /. l.x
    in
    let orth= Line.orth slope in
    let c1=
      let c= Ops.((p0 + p1) / {x=2.;y=2.}) in
      Ops.(Line.extended_slope ~slope:orth (width*.0.5) + c)
    in
    let p2= Ops.(Line.extended_slope ~slope:orth (width/.4.) + p1) in
    let p3=
      let c= Ops.(Line.extended_slope ~slope (width*.0.8) + p0) in
      Ops.(Line.extended_slope ~slope:orth (width*.1.0) + c)
    in
    let c3=
      let c= Ops.((p2 + p3) / {x=2.1;y=2.}) in
      Ops.(Line.extended_slope ~slope:orth (width*.0.8) + c)
    in
    let p4=
      let c= Ops.(Line.extended_slope ~slope (width*.0.4) + p0) in
      Ops.(Line.extended_slope ~slope:orth (width*.1.3) + c)
    in
    let c4= Ops.(Line.extended_slope ~slope (width/.4.) + p4) in
    let c5= Ops.(Line.extended_slope ~slope (-.width/.4.) + p4) in
    let segments= [
      Qcurve { ctrl= c1; end'= p1 };
      Line p2;
      Qcurve { ctrl= c3; end'= p3 };
      Qcurve { ctrl= c4; end'= p4 };
      Qcurve { ctrl= c5; end'= p0 };
      ] in
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

  let from_du ?(width=Width.width) du=
    let open Path in
    let open PointF in
    let start= du.du_start in
    let joint= { x= start.x +. width*.1.8; y= start.y +. width*.0.3 } in
    let dot= get_adjust du.dot
      ~f:(fun ()-> {x= joint.x +. width*.0.2; y= joint.y +. width*.4.})
    in
    let vec_j_s= Ops.(start - joint)
    and vec_j_u= Ops.(du.end_ - joint)
    and vec_j_d= Ops.(dot - joint) in
    let slope_u= vec_j_u.y /. vec_j_u.x in
    let vec_unit_j_s= Line.unit_vector vec_j_s
    and vec_unit_j_u= Line.unit_vector vec_j_u
    and vec_unit_j_d= Line.unit_vector vec_j_d in
    let vec_mid_s_u= Line.vector_sum [vec_unit_j_s;vec_unit_j_u]
    and vec_mid_u_d= Line.vector_sum [vec_unit_j_u;vec_unit_j_d]
    and vec_mid_s_d= Line.vector_sum [vec_unit_j_s;vec_unit_j_d] in
    let left_p0= Ops.(start - Line.extended_vec ~vec:{x=0.; y=1.} (width*.0.25)) in
    let left_p1= Ops.(start + Line.extended_vec ~vec:{x=0.; y=1.} (width*.0.25)) in
    let p_s_u= Ops.(joint + Line.extended_vec ~vec:vec_mid_s_u (width*.0.5)) in
    let p_s_u_c_left= Ops.(-) p_s_u @@
      Line.extended_vec ~vec:Matrix.(apply clockwise_90 vec_mid_s_u) (width*. 0.5)
    and p_s_u_c_right= Ops.(+) p_s_u @@
      Line.extended_vec ~vec:Matrix.(apply clockwise_90 vec_mid_s_u) (width*. 0.5) in
    let p_u_d= Ops.(joint + Line.extended_vec ~vec:vec_mid_u_d (width*.0.75)) in
    let p_u_d_c= Ops.(-) p_u_d @@
      Line.extended_vec ~vec:Matrix.(apply clockwise_90 vec_mid_u_d) (width*. 0.3)
    in
    let dot_right=
      let dot_up= Ops.(dot - Line.extended_vec ~vec:vec_j_d (width*.0.8)) in
      Ops.(dot_up + Line.extended_vec ~vec:Matrix.(apply anticlock_90 vec_j_d) (width*.0.8))
    in
    let mid_p_u_d_dot_right= Ops.((p_u_d + dot_right) / {x=2.1;y=2.05}) in
    let mid_p_u_d_dot_right_c= Ops.(p_u_d +
      (Line.extended_vec ~vec:(p_u_d - p_u_d_c) (width*. 0.5)))
    in
    let dot_right_c= Ops.(mid_p_u_d_dot_right +
      (Line.extended_vec ~vec:(mid_p_u_d_dot_right - mid_p_u_d_dot_right_c) (width*. 0.4)))
    in
    let dot_c= Ops.(dot_right +
      (Line.extended_vec ~vec:(dot_right - dot_right_c) (width*. 0.7)))
    in
    let dot_left=
      let dot_up= Ops.(dot - Line.extended_vec ~vec:vec_j_d (width*.0.3)) in
      Ops.(dot_up + Line.extended_vec ~vec:Matrix.(apply clockwise_90 vec_j_d) (width*.0.4))
    in
    let dot_left_c= Ops.(dot +
      (Line.extended_vec ~vec:(dot - dot_c) (width*. 0.3)))
    in
    let p_s_d= Ops.(joint + {x=width*.0.2;y=width*.0.3} + Line.extended_vec ~vec:vec_mid_s_d (width*.0.6)) in
    let mid_dot_left_p_s_d= Ops.((p_s_d + dot_left) / {x=1.9;y=1.95}) in
    let mid_dot_left_p_s_d_c= Ops.(dot_left +
      (Line.extended_vec ~vec:(dot_left - dot_left_c) (width*. 0.2)))
    in
    let p_s_d_c= Ops.(mid_dot_left_p_s_d +
      (Line.extended_vec ~vec:(mid_dot_left_p_s_d - mid_dot_left_p_s_d_c) (width*. 0.8)))
    in
    let p_s_d_start_c= Ops.(p_s_d +
      (Line.extended_vec ~vec:(p_s_d - p_s_d_c) (width*. 0.6)))
    in
    let end_p0= Ops.(du.end_ + Line.extended_slope ~slope:(Line.orth slope_u) (width*. -0.15))
    and end_p1= Ops.(du.end_ + Line.extended_slope ~slope:(Line.orth slope_u) (width*. 0.15)) in
    let segments= [
      Qcurve { ctrl= p_s_u_c_left; end'= p_s_u };
      Qcurve { ctrl= p_s_u_c_right; end'= end_p0 };
      Line end_p1;
      Qcurve { ctrl= p_u_d_c; end'= p_u_d };
      Qcurve { ctrl= mid_p_u_d_dot_right_c; end'= mid_p_u_d_dot_right };
      Qcurve { ctrl= dot_right_c; end'= dot_right };
      Qcurve { ctrl= dot_c; end'= dot };
      Qcurve { ctrl= dot_left_c; end'= dot_left };
      Qcurve { ctrl= mid_dot_left_p_s_d_c; end'= mid_dot_left_p_s_d };
      Qcurve { ctrl= p_s_d_c; end'= p_s_d };
      Qcurve { ctrl= p_s_d_start_c; end'= left_p1 };
      Line left_p0;
      ] in
    {
      start= left_p0;
      segments;
    }

  (*
    type v= {
      v_start: point;
      length: float;
    } (* Vertical *)
      (* 1. the length is at least 4 times its weight *)
  *)

  let from_v ?(width=Width.width) v=
    let open Path in
    let open Point in
    let start= v.v_start in
    let p0= start in
    let p1= { x= p0.x +. width*.1.5; y= p0.y+.width*.0.5 } in
    let p2= { x= p1.x -. width*.0.5; y= p1.y+.width*.0.3 } in
    let p3= { p2 with y= p0.y +. v.length -. width *. 0.3 } in
    let p4= { p0 with y= p0.y +. v.length} in
    let c4= Ops.((p4 + p3) / {x=2.;y=2.} - {x=width*.0.05; y=width*.0.05}) in
    let segments= [
      Line p1;
      Line p2;
      Line p3;
      Qcurve { ctrl= c4; end'= p4 };
      Line p0
      ] in
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

  let from_sv ?(width=Width.width) (sv:sv)=
    let open Path in
    let open Point in
    let adjust p=
      let length= sv.length in
      let height= sqrt(length *. length -. sv.width *. sv.width) in
      let matrix= Matrix.{
        c1= { r1= height /. length; r2= sv.width /. length };
        c2= { r1= -. sv.width /. length; r2= height /. length};
        }
      in
      Ops.(Matrix.apply matrix (p - sv.sv_start) + sv.sv_start)
    in
    let p0= sv.sv_start in
    let p1= { x= p0.x +. width*.1.5; y= p0.y+.width*.0.5 } in
    let p2= { x= p1.x -. width*.0.5; y= p1.y+.width*.0.3 } in
    let p3= { p2 with y= p0.y +. sv.length -. width *. 0.3 } in
    let p4= { p0 with y= p0.y +. sv.length} in
    let c4= Ops.((p4 + p3) / {x=2.;y=2.} - {x=width*.0.05; y=width*.0.05}) in
    let p0= adjust p0 and p1= adjust p1 and p2= adjust p2
    and p3= adjust p3 and p4= adjust p4 and c4= adjust c4 in
    let segments= [
      Line p1;
      Line p2;
      Line p3;
      Qcurve { ctrl= c4; end'= p4 };
      Line p0
      ] in
    {
      start= p0;
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

  let from_rsv ?(width=Width.width) (rsv:rsv)=
    let open Path in
    let open Point in
    let adjust p=
      let length= rsv.length in
      let height= sqrt(length *. length -. rsv.width *. rsv.width) in
      let matrix= Matrix.{
        c1= { r1= height /. length; r2= -. rsv.width /. length };
        c2= { r1= rsv.width /. length; r2= height /. length};
        }
      in
      Ops.(Matrix.apply matrix (p - rsv.rsv_start) + rsv.rsv_start)
    in
    let p0= rsv.rsv_start in
    let p1= { x= p0.x +. width*.1.5; y= p0.y+.width*.0.5 } in
    let p2= { x= p1.x -. width*.0.5; y= p1.y+.width*.0.3 } in
    let p3= { p2 with y= p0.y +. rsv.length -. width *. 0.3 } in
    let p4= { p0 with y= p0.y +. rsv.length} in
    let c4= Ops.((p4 + p3) / {x=2.;y=2.} - {x=width*.0.05; y=width*.0.05}) in
    let p0= adjust p0 and p1= adjust p1 and p2= adjust p2
    and p3= adjust p3 and p4= adjust p4 and c4= adjust c4 in
    let segments= [
      Line p1;
      Line p2;
      Line p3;
      Qcurve { ctrl= c4; end'= p4 };
      Line p0
      ] in
    {
      start= p0;
      segments;
    }

  (*
    type t'= {
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

  let from_t' ?(width=Width.width) (t:t')=
    let open Path in
    let open PointF in
    let start= t.t'_start
    and end'= t.end_ in
    let vec_t= Ops.(end' - start) in
    let angle_t= angle vec_t in
    let p1_angle= angle_t -. pi/.1.8 in
    let p2_angle= p1_angle +. pi*.0.65 in
    let p1= Ops.(+) start @@
      Line.extended_angle ~angle:p1_angle (width*. 1.3)
    in
    let p2= Ops.(+) p1 @@
      Line.extended_angle ~angle:p2_angle (width*. 0.4)
    in
    let p3= Ops.(+) end' @@
      Line.extended_angle ~angle:p1_angle (width*. 0.2)
    in
    let t_down=
      let calc_ctrl= template_curve ~start:p2 ~end':p3 in
      let ctrl1= match t.ctrl1 with
        | Auto-> calc_ctrl ~ratio:{x= 0.10; y= 0.25}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(angle_t-. (pi*.0.5)) (width*. 0.8)
      and ctrl2= match t.ctrl2 with
        | Auto-> calc_ctrl ~ratio:{x= 0.09; y= 0.75}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(angle_t-. (pi*.0.5)) (width*. 0.6)
      in
      Ccurve {ctrl1; ctrl2; end'= p3 }
    in
    let t_up=
      let calc_ctrl= template_curve ~start ~end' in
      let ctrl1= get_adjust t.ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.09; y= 0.25})
      and ctrl2= get_adjust t.ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.09; y= 0.75})
      in
      Ccurve {ctrl1= ctrl2; ctrl2= ctrl1; end'= start}
    in
    let segments= [
      Line p1;
      Line p2;
      t_down;
      Line end';
      t_up;
      ]
    in
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

  let from_ft ?(width=Width.width) (ft:ft)=
    let open Path in
    let open PointF in
    let start= ft.ft_start
    and end'= ft.end_ in
    let vec_t= Ops.(end' - start) in
    let angle_t= angle vec_t in
    let p1_angle= angle_t -. pi/.1.8 in
    let p2_angle= p1_angle +. pi*.0.65 in
    let p1= Ops.(+) start @@
      Line.extended_angle ~angle:p1_angle (width*. 1.3)
    in
    let p2= Ops.(+) p1 @@
      Line.extended_angle ~angle:p2_angle (width*. 0.4)
    in
    let p3= Ops.(+) end' @@
      Line.extended_angle ~angle:p1_angle (width*. 0.2)
    in
    let t_down=
      let calc_ctrl= template_curve ~start:p2 ~end':p3 in
      let ctrl1= match ft.ctrl1 with
        | Auto-> calc_ctrl ~ratio:{x= 0.06; y= 0.25}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(angle_t-. (pi*.0.5)) (width*. 0.8)
      and ctrl2= match ft.ctrl2 with
        | Auto-> calc_ctrl ~ratio:{x= 0.04; y= 0.75}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(angle_t-. (pi*.0.5)) (width*. 0.6)
      in
      Ccurve {ctrl1; ctrl2; end'= p3 }
    in
    let t_up=
      let calc_ctrl= template_curve ~start ~end' in
      let ctrl1= get_adjust ft.ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.05; y= 0.25})
      and ctrl2= get_adjust ft.ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.04; y= 0.75})
      in
      Ccurve {ctrl1= ctrl2; ctrl2= ctrl1; end'= start}
    in
    let segments= [
      Line p1;
      Line p2;
      t_down;
      Line end';
      t_up;
      ]
    in
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

  let from_wt ?(width=Width.width) wt=
    let open Path in
    let open PointF in
    let start= wt.wt_start
    and end'= wt.end_ in
    let height= end'.y -. start.y in
    let v_length= max width @@ get_adjust wt.v_length
      ~f:(fun()-> height *. 0.4) in
    let vec_t= Ops.(end' - { start with y= start.y+.v_length}) in
    let angle_t= angle vec_t in
    let p1= Ops.(+) start {x=width*.1.4; y= width*.0.4} in
    let p2= Ops.(+) p1 {x= -. width*.0.4; y= width*.0.4} in
    let p3= Ops.(+) p2 {x= 0.; y= v_length -. width*.0.8} in
    let r_end= Ops.(+) end' @@
      Line.extended_angle ~angle:(angle_t -. pi/.1.8) (width*. 0.2)
    in
    let r_ctrl1, r_ctrl2=
      let calc_ctrl= template_curve ~start:p3 ~end':r_end in
      let ctrl1= match wt.ctrl1 with
        | Auto->
          let ctrl= calc_ctrl ~ratio:{x= 0.10; y= 0.5} in
          let x= min ctrl.x p3.x in
          { ctrl with x }
        | Specify p-> Ops.(+) p @@
          Line.extended_angle
            ~angle:(angle_t-. (pi*.0.5)) (width*. 0.8)
      and ctrl2= match wt.ctrl2 with
        | Auto-> calc_ctrl ~ratio:{x= 0.09; y= 0.75}
        | Specify p-> Ops.(+) p @@
          Line.extended_angle
            ~angle:(angle_t-. (pi*.0.5)) (width*. 0.6)
      in
      (ctrl1, ctrl2)
    in
    let c4=
      let r_w_ext= Line.of_points p2 p3 in
      let r_ctrl_ext= Line.of_points r_ctrl1 r_ctrl2 in
      Line.(get_intersection_point
        (intersection_of_lines r_w_ext r_ctrl_ext))
    in
    let p5= Ops.(+) start {x=0.;y=v_length} in
    let l_ctrl1, l_ctrl2=
      let calc_ctrl= template_curve ~start:p5 ~end' in
      let ctrl1=get_adjust wt.ctrl1
        ~f:(fun ()->
          let ctrl= calc_ctrl ~ratio:{x= 0.09; y= 0.5} in
          let x= min ctrl.x p5.x in
          { ctrl with x })
      and ctrl2= get_adjust wt.ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.09; y= 0.75})
      in
      ctrl1, ctrl2
    in
    let c5=
      let r_w_ext= Line.of_points start p5 in
      let r_ctrl_ext= Line.of_points l_ctrl1 l_ctrl2 in
      Line.(get_intersection_point
        (intersection_of_lines r_w_ext r_ctrl_ext))
    in
    let segments= [
      Line p1;
      Line p2;
      Line p3;
      Qcurve {ctrl= c4; end'= r_ctrl1};
      Qcurve {ctrl= r_ctrl2; end'= r_end};
      Line end';
      Qcurve {ctrl= l_ctrl2; end'= l_ctrl1};
      Qcurve {ctrl= c5; end'= p5};
      Line start;
      ]
    in
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

  let from_d ?(width=Width.width) d=
    let open Path in
    let open Point in
    let length_limit= width *. sqrt 18. in
    let start= d.d_start in
    let end_= get_adjust d.end_
      ~f:(fun()->
        let x= start.x +. 2. *. width
        and y= start.y +. 2. *. width in
        Point.{x;y}
      )
    in
    let vec_d= Ops.(end_ - start) in
    let length, end_=
      let length_specified= Point.distance ~from:start end_ in
      if length_specified > length_limit  then
        invalid_arg "this extended dot is not short enough"
        (* length_limit, Ops.(+) start (Line.extended_vec ~vec:vec_d length_limit) *)
      else
        (length_specified, end_)
    in
    let vec_d_anti90= Matrix.(apply anticlock_90 vec_d) in
    let vec_d_anti45= Matrix.(apply (anticlock ~radian:(pi /. 4.)) vec_d) in
    let vec_d_anti45_neg= neg vec_d_anti45 in
    let p0= start in
    let p1= Ops.(start + Line.extended_vec ~vec:vec_d_anti90 (max 1. (width/.8.))) in
    let r_ctrl1=
      let p= Ops.(p1 + Line.extended_vec ~vec:vec_d (length*.0.9)) in
      Ops.(p + Line.extended_vec ~vec:vec_d_anti90 (width*.1.5)) in
    let r_ctrl2=
      Ops.(end_ + Line.extended_vec ~vec:vec_d_anti45 (width*.0.6)) in
    let l_ctrl1=
      Ops.(end_ + Line.extended_vec ~vec:vec_d_anti45_neg (width*.0.2)) in
    let l_ctrl2=
      let p= Ops.(p0 + Line.extended_vec ~vec:vec_d (length*.0.75)) in
      Ops.(p + Line.extended_vec ~vec:vec_d_anti90 (length*.0.08)) in
    let segments= [
      Line p1;
      Ccurve { ctrl1= r_ctrl1; ctrl2= r_ctrl2; end'= end_ };
      Ccurve { ctrl1= l_ctrl1; ctrl2= l_ctrl2; end'= p0 };
      ] in
    {
      start= p0;
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

  let from_ed ?(width=Width.width) ed=
    let open Path in
    let open Point in
    let length_limit= width *. sqrt 18. in
    let start= ed.ed_start in
    let end_= get_adjust ed.end_
      ~f:(fun()->
        let x= start.x +. 4. *. width
        and y= start.y +. 4. *. width in
        Point.{x;y}
      )
    in
    let vec_d= Ops.(end_ - start) in
    let length, end_=
      let length_specified= Point.distance ~from:start end_ in
      if length_specified < length_limit then
        invalid_arg "this extended dot is not long enough"
        (* length_limit, Ops.(+) start (Line.extended_vec ~vec:vec_d length_limit) *)
      else
        (length_specified, end_)
    in
    let vec_d_anti90= Matrix.(apply anticlock_90 vec_d) in
    let vec_d_anti45= Matrix.(apply (anticlock ~radian:(pi /. 4.)) vec_d) in
    let vec_d_anti45_neg= neg vec_d_anti45 in
    let p0= start in
    let p1= Ops.(start + Line.extended_vec ~vec:vec_d_anti90 (max 1. (width/.8.))) in
    let r_ctrl1=
      let p= Ops.(p1 + Line.extended_vec ~vec:vec_d (length*.0.9)) in
      Ops.(p + Line.extended_vec ~vec:vec_d_anti90 (width*.1.5)) in
    let r_ctrl2=
      Ops.(end_ + Line.extended_vec ~vec:vec_d_anti45 (width*.0.6)) in
    let l_ctrl1=
      Ops.(end_ + Line.extended_vec ~vec:vec_d_anti45_neg (width*.0.2)) in
    let l_ctrl2=
      let p= Ops.(p0 + Line.extended_vec ~vec:vec_d (length*.0.75)) in
      Ops.(p + Line.extended_vec ~vec:vec_d_anti90 (length*.0.08)) in
    let segments= [
      Line p1;
      Ccurve { ctrl1= r_ctrl1; ctrl2= r_ctrl2; end'= end_ };
      Ccurve { ctrl1= l_ctrl1; ctrl2= l_ctrl2; end'= p0 };
      ] in
    {
      start= p0;
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

  let from_ld ?(width=Width.width) ld=
    let open Path in
    let open Point in
    let start= ld.ld_start in
    let end_= get_adjust ld.end_
      ~f:(fun()->
        let x= start.x -. 2. *. width
        and y= start.y +. 3. *. width in
        Point.{x;y}
      )
    in
    let vec_d= Ops.(end_ - start) in
    let length= Point.distance ~from:start end_ in
    let vec_d_anti90= Matrix.(apply anticlock_90 vec_d) in
    let vec_d_anti45= Matrix.(apply (anticlock ~radian:(pi /. 4.)) vec_d) in
    let vec_d_anti45_neg= neg vec_d_anti45 in
    let p0= start in
    let p1= Ops.(start + Line.extended_vec ~vec:vec_d_anti90 (max 1. (width/.8.))) in
    let r_ctrl1=
      let p= Ops.(p1 + Line.extended_vec ~vec:vec_d (length*.0.9)) in
      Ops.(p + Line.extended_vec ~vec:vec_d_anti90 (width*.1.5)) in
    let r_ctrl2=
      Ops.(end_ + Line.extended_vec ~vec:vec_d_anti45 (width*.0.6)) in
    let l_ctrl1=
      Ops.(end_ + Line.extended_vec ~vec:vec_d_anti45_neg (width*.0.2)) in
    let l_ctrl2=
      let p= Ops.(p0 + Line.extended_vec ~vec:vec_d (length*.0.75)) in
      Ops.(p + Line.extended_vec ~vec:vec_d_anti90 (length*.0.1)) in
    let segments= [
      Line p1;
      Ccurve { ctrl1= r_ctrl1; ctrl2= r_ctrl2; end'= end_ };
      Ccurve { ctrl1= l_ctrl1; ctrl2= l_ctrl2; end'= p0 };
      ] in
    {
      start= p0;
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

  let from_wd ?(width=Width.width) wd=
    let open Path in
    let open Point in
    let start= wd.wd_start in
    let length= get_adjust wd.length
      ~f:(fun()-> width*.2.) in
    let p0= start in
    let p1= { x= p0.x +. width*.1.5; y= p0.y+.width*.0.5 } in
    let p2= { x= p1.x -. width*.0.5; y= p1.y+.width*.0.3 } in
    let p3= { p2 with y= p0.y +. length } in
    let p4= { p3 with x= p0.x } in
    let segments= [
      Line p1;
      Line p2;
      Line p3;
      Line p4;
      Line p0
      ] in
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

  let from_p ?(width=Width.width) (p:p)=
    let open Path in
    let open Point in
    let start= p.p_start
    and end'= p.end_ in
    let length= Point.distance ~from:start end' in
    let vec_p= Ops.(end' - start) in
    let vec_p_anti90= Matrix.(apply anticlock_90 vec_p) in
    let vec_p_clock90= Matrix.(apply clockwise_90 vec_p) in
    let p0= start in
    let p1= Ops.(p0 + Line.extended_vec ~vec:vec_p_anti90 (width/.8.)) in
    let ctrl1= get_adjust p.ctrl1
      ~f:(fun()-> Ops.(p1
      + Line.extended_vec ~vec:vec_p (length *. 0.25)
      + Line.extended_vec ~vec:vec_p_clock90 (width *. 1.2)
      )) in
    let ctrl2= get_adjust p.ctrl2
      ~f:(fun()-> Ops.(p1
      + Line.extended_vec ~vec:vec_p (length *. 0.7)
      + Line.extended_vec ~vec:vec_p_clock90 (width *. 1.2)
      )) in
    let r_ctrl1= ctrl1 in
    let r_ctrl2= ctrl2 in
    let p2= Ops.(+) end' @@
      Line.extended_vec
        ~vec:(Matrix.(apply (anticlock ~radian:(pi *. 0.45)) vec_p))
        (width*.1.2) in
    let c2=
      let vec_end_p2= Ops.(p2 - end') in
      let length= distance vec_end_p2 in
      let p= Ops.(end' + Line.extended_vec ~vec:vec_end_p2 (length*.0.4)) in
      Ops.(p +
        Line.extended_vec
          ~vec:Matrix.(apply anticlock_90 vec_end_p2)
          (width*.0.3))
    in
    let l_ctrl2=
      Ops.(ctrl1 + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.6))
    in
    let l_ctrl1=
      Ops.(ctrl2 + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.8))
    in
    let segments= [
      Line p1;
      Ccurve {ctrl1= r_ctrl1; ctrl2= r_ctrl2; end'= p2};
      Qcurve {ctrl= c2; end'};
      Ccurve {ctrl1= l_ctrl1; ctrl2= l_ctrl2; end'= p0};
    ] in
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

  let from_up ?(width=Width.width) (up:up)=
    let open Path in
    let open Point in
    let p_start= up.p_start
    and end'= up.end_ in
    let length= Point.distance ~from:p_start end' in
    let vec_p= Ops.(end' - p_start) in
    let vec_p_anti90= Matrix.(apply anticlock_90 vec_p) in
    let vec_p_clock90= Matrix.(apply clockwise_90 vec_p) in
    let start= get_adjust up.up_start
      ~f:Ops.(fun()-> p_start + {x=width*. -1.;y=width*.1.}) in
    let vec_u= Ops.(p_start - start) in
    let vec_u_anti90= Matrix.(apply anticlock_90 vec_u) in
    let vec_u_clock90= Matrix.(apply clockwise_90 vec_u) in
    let u0= Ops.(start + Line.extended_vec ~vec:vec_u_anti90 (width*.0.3))
    and u1= Ops.(start + Line.extended_vec ~vec:vec_u_clock90 (width*.0.3)) in
    let p1= Ops.(p_start + Line.extended_vec ~vec:vec_p_anti90 (width/.8.)) in
    let ctrl1= get_adjust up.ctrl1
      ~f:(fun()-> Ops.(p1
      + Line.extended_vec ~vec:vec_p (length *. 0.25)
      + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.50)
      )) in
    let ctrl2= get_adjust up.ctrl2
      ~f:(fun()-> Ops.(p1
      + Line.extended_vec ~vec:vec_p (length *. 0.7)
      + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.55)
      )) in
    let r_ctrl1= ctrl1 in
    let r_ctrl2= ctrl2 in
    let p2= Ops.(+) end' @@
      Line.extended_vec
        ~vec:(Matrix.(apply (anticlock ~radian:(pi *. 0.45)) vec_p))
        (width*.1.2) in
    let c2=
      let vec_end_p2= Ops.(p2 - end') in
      let length= distance vec_end_p2 in
      let p= Ops.(end' + Line.extended_vec ~vec:vec_end_p2 (length*.0.4)) in
      Ops.(p +
        Line.extended_vec
          ~vec:Matrix.(apply anticlock_90 vec_end_p2)
          (width*.0.3))
    in
    let l_ctrl2=
      Ops.(ctrl1 + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.2))
    in
    let l_ctrl1=
      Ops.(ctrl2 + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.45))
    in
    let vec_u_rev= Ops.(start - p_start) in
    let vec_p_inital= Ops.(l_ctrl2 - p1) in
    let vec_u_p_mean= Line.([vec_u_rev; vec_p_inital]
      |> List.map unit_vector
      |> vector_mean) in
    let p4= Ops.(p1 + Line.extended_vec ~vec:vec_u_p_mean (width *. 0.25)) in
    let segments= [
      Line p1;
      Ccurve {ctrl1= r_ctrl1; ctrl2= r_ctrl2; end'= p2};
      Qcurve {ctrl= c2; end'};
      Ccurve {ctrl1= l_ctrl1; ctrl2= l_ctrl2; end'= p4};
      Line u1;
      Line u0;
    ] in
    {
      start= u0;
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

  let from_hp ?(width=Width.width) hp=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= hp.hp_start in
    let h0= start in
    let h1= { h0 with x= h0.x +. hp.length -. width*._a } in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h3= {
      x= h2.x +. width*._h;
      y= start.y +. width*._h*.0.5 } in
    (* insert p *)
    let h4= {
      x= min (h0.x +. width*.2.) h1.x;
      y= h0.y +. width *. _h;
      } in
    let h5= {
      x= h0.x +. width*._sah;
      y= h0.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let p_start= { x= h1.x +. width *.0.2; y= h1.y +. width *. _h}
    and end'= hp.end_ in
    let p_length= Point.distance ~from:p_start end' in
    let vec_p= Ops.(end' - p_start) in
    let vec_p_clock90= Matrix.(apply clockwise_90 vec_p) in
    let p1= { p_start with x= p_start.x +. width *. _h } in
    let ctrl1= get_adjust hp.ctrl1
      ~f:(fun()-> Ops.(p1
      + Line.extended_vec ~vec:vec_p (p_length *. 0.25)
      + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.50)
      )) in
    let ctrl2= get_adjust hp.ctrl2
      ~f:(fun()-> Ops.(p1
      + Line.extended_vec ~vec:vec_p (p_length *. 0.7)
      + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.75)
      )) in
    let r_ctrl1= ctrl1 in
    let r_ctrl2= ctrl2 in
    let p2= Ops.(+) end' @@
      Line.extended_vec
        ~vec:(Matrix.(apply (anticlock ~radian:(pi *. 0.45)) vec_p))
        (width*.1.8) in
    let c2=
      let vec_end_p2= Ops.(p2 - end') in
      let length= distance vec_end_p2 in
      let p= Ops.(end' + Line.extended_vec ~vec:vec_end_p2 (length*.0.4)) in
      Ops.(p +
        Line.extended_vec
          ~vec:Matrix.(apply anticlock_90 vec_end_p2)
          (width*.0.3))
    in
    let l_ctrl2= let open Ops in
      ctrl1 + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.2)
    in
    let l_ctrl1= let open Ops in
      ctrl2 + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.25)
    in

    let segments= [
      Line h1;
      Line h2;
      Line h3;

      Line p1;
      Ccurve {ctrl1= r_ctrl1; ctrl2= r_ctrl2; end'= p2};
      Qcurve {ctrl= c2; end'};
      Ccurve {ctrl1= l_ctrl1; ctrl2= l_ctrl2; end'= p_start};

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h0
      ] in
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

  let from_fp ?(width=Width.width) (fp:fp)=
    let open Path in
    let open Point in
    let start= fp.fp_start
    and end'= fp.end_ in
    let length= Point.distance ~from:start end' in
    let vec_p= Ops.(end' - start) in
    let vec_p_anti90= Matrix.(apply anticlock_90 vec_p) in
    let vec_p_clock90= Matrix.(apply clockwise_90 vec_p) in
    let p0= start in
    let p1= Ops.(p0 + Line.extended_vec ~vec:vec_p_anti90 (width/.8.)) in
    let ctrl1= get_adjust fp.ctrl1
      ~f:(fun()-> Ops.(p1
      + Line.extended_vec ~vec:vec_p (length *. 0.25)
      + Line.extended_vec ~vec:vec_p_clock90 (width *. 1.75)
      )) in
    let ctrl2= get_adjust fp.ctrl2
      ~f:(fun()-> Ops.(p1
      + Line.extended_vec ~vec:vec_p (length *. 0.7)
      + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.25)
      )) in
    let r_ctrl1= ctrl1 in
    let r_ctrl2= ctrl2 in
    let p2= Ops.(+) end' @@
      Line.extended_vec
        ~vec:(Matrix.(apply (anticlock ~radian:(pi *. 0.45)) vec_p))
        (width*.1.3) in
    let c2=
      let vec_end_p2= Ops.(p2 - end') in
      let length= distance vec_end_p2 in
      let p= Ops.(end' + Line.extended_vec ~vec:vec_end_p2 (length*.0.4)) in
      Ops.(p +
        Line.extended_vec
          ~vec:Matrix.(apply anticlock_90 vec_end_p2)
          (width*.0.3))
    in
    let l_ctrl2=
      Ops.(ctrl1 + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.45))
    in
    let l_ctrl1=
      Ops.(ctrl2 + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.55))
    in
    let segments= [
      Line p1;
      Ccurve {ctrl1= r_ctrl1; ctrl2= r_ctrl2; end'= p2};
      Qcurve {ctrl= c2; end'};
      Ccurve {ctrl1= l_ctrl1; ctrl2= l_ctrl2; end'= p0};
    ] in
    {
      start;
      segments;
    }

  (*
    type ufp= {
      ufp_start: point adjust;
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

  let from_ufp ?(width=Width.width) (ufp:ufp)=
    let open Path in
    let open Point in
    let p_start= ufp.p_start
    and end'= ufp.end_ in
    let length= Point.distance ~from:p_start end' in
    let vec_p= Ops.(end' - p_start) in
    let vec_p_anti90= Matrix.(apply anticlock_90 vec_p) in
    let vec_p_clock90= Matrix.(apply clockwise_90 vec_p) in
    let start= get_adjust ufp.ufp_start
      ~f:Ops.(fun()-> p_start + {x=width*. -1.;y=width*.1.}) in
    let vec_u= Ops.(p_start - start) in
    let vec_u_anti90= Matrix.(apply anticlock_90 vec_u) in
    let vec_u_clock90= Matrix.(apply clockwise_90 vec_u) in
    let u0= Ops.(start + Line.extended_vec ~vec:vec_u_anti90 (width*.0.3))
    and u1= Ops.(start + Line.extended_vec ~vec:vec_u_clock90 (width*.0.3)) in
    let p1= Ops.(p_start + Line.extended_vec ~vec:vec_p_anti90 (width/.8.)) in
    let ctrl1= get_adjust ufp.ctrl1
      ~f:(fun()-> Ops.(p1
      + Line.extended_vec ~vec:vec_p (length *. 0.25)
      + Line.extended_vec ~vec:vec_p_clock90 (width *. 1.75)
      )) in
    let ctrl2= get_adjust ufp.ctrl1
      ~f:(fun()-> Ops.(p1
      + Line.extended_vec ~vec:vec_p (length *. 0.7)
      + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.25)
      )) in
    let r_ctrl1= ctrl1 in
    let r_ctrl2= ctrl2 in
    let p2= Ops.(+) end' @@
      Line.extended_vec
        ~vec:(Matrix.(apply (anticlock ~radian:(pi *. 0.45)) vec_p))
        (width*.1.3) in
    let c2=
      let vec_end_p2= Ops.(p2 - end') in
      let length= distance vec_end_p2 in
      let p= Ops.(end' + Line.extended_vec ~vec:vec_end_p2 (length*.0.4)) in
      Ops.(p +
        Line.extended_vec
          ~vec:Matrix.(apply anticlock_90 vec_end_p2)
          (width*.0.3))
    in
    let l_ctrl2=
      Ops.(ctrl1 + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.45))
    in
    let l_ctrl1=
      Ops.(ctrl2 + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.55))
    in
    let vec_u_rev= Ops.(start - p_start) in
    let vec_p_inital= Ops.(l_ctrl2 - p1) in
    let vec_u_p_mean= Line.([vec_u_rev; vec_p_inital]
      |> List.map unit_vector
      |> vector_mean) in
    let p4= Ops.(p1 + Line.extended_vec ~vec:vec_u_p_mean (width *. 0.25)) in
    let segments= [
      Line p1;
      Ccurve {ctrl1= r_ctrl1; ctrl2= r_ctrl2; end'= p2};
      Qcurve {ctrl= c2; end'};
      Ccurve {ctrl1= l_ctrl1; ctrl2= l_ctrl2; end'= p4};
      Line u1;
      Line u0;
    ] in
    {
      start= u0;
      segments;
    }

  (*
    type c= {
      c_start: point;
      ctrl1: point adjust;
      ctrl2: point adjust;
      end_: point;
    } (* Clockwise curve *)
  *)

  let from_c ?(width=Width.width) c=
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
    let vec_down_end=
      let arc= Ops.(end' - ctrl2) in
      Matrix.(apply clockwise_90 arc)
    and vec_up_end=
      let arc= Ops.(start - ctrl1) in
      Matrix.(apply anticlock_90 arc) in
    let arc_right=
      let ctrl1= Ops.(ctrl1 + Line.extended_vec ~vec:vec_anti90 (width*.0.5))
      and ctrl2= Ops.(ctrl2 + Line.extended_vec ~vec:vec_anti90 (width*.0.5)) in
      Ccurve {ctrl1;ctrl2;end'} in
    let line_down=
      let p= Ops.(+) end' @@
        Line.extended_vec ~vec:vec_down_end (width *. 0.5) in
      Line p in
    let arc_left=
      let end'= Ops.(+) start @@
        Line.extended_vec ~vec:vec_up_end (width *. 0.5)
      in
      let ctrl1= Ops.(ctrl2 - Line.extended_vec ~vec:vec_anti90 (width*.0.5))
      and ctrl2= Ops.(ctrl1 - Line.extended_vec ~vec:vec_anti90 (width*.0.5)) in
      Ccurve {ctrl1;ctrl2;end'} in
    let line_up= Line start in
    let segments= [arc_right; line_down; arc_left; line_up] in
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

  let from_a ?(width=Width.width) a=
    let open Path in
    let open Point in
    let start= a.a_start
    and end'= a.end_ in
    let vec_a= Ops.(end' - start) in
    let vec_clock90= Matrix.(apply clockwise_90 vec_a) in
    let length= distance vec_a in
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
    let vec_down_end=
      let arc= Ops.(end' - ctrl2) in
      Matrix.(apply anticlock_90 arc)
    and vec_up_end=
      let arc= Ops.(start - ctrl1) in
      Matrix.(apply clockwise_90 arc) in
    let arc_left=
      let ctrl1= Ops.(ctrl1 - Line.extended_vec ~vec:vec_clock90 (width*.0.5))
      and ctrl2= Ops.(ctrl2 - Line.extended_vec ~vec:vec_clock90 (width*.0.5)) in
      Ccurve {ctrl1;ctrl2;end'} in
    let line_down=
      let p= Ops.(+) end' @@
        Line.extended_vec ~vec:vec_down_end (width *. 0.5) in
      Line p in
    let arc_right=
      let end'= Ops.(+) start @@
        Line.extended_vec ~vec:vec_up_end (width *. 0.5)
      in
      let ctrl1= Ops.(ctrl2 + Line.extended_vec ~vec:vec_clock90 (width*.0.5))
      and ctrl2= Ops.(ctrl1 + Line.extended_vec ~vec:vec_clock90 (width*.0.5)) in
      Ccurve {ctrl1;ctrl2;end'} in
    let line_up= Line start in
    let segments= [arc_left; line_down; arc_right; line_up] in
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

  let from_o ?(width=Width.width) o=
    let open Path in
    let open Point in
    let start= o.o_start
    and end'= o.end_ in
    let vec_o= Ops.(end' - start) in
    let vec_o_rev= neg vec_o in
    let length= distance vec_o in
    let height= abs_float vec_o.y in
    let orth_width= o.width *. height /. length in
    let center= Ops.((start + end') /< 2.) in
    let adjust p= Ops.(Matrix.apply {
        c1= {r1=(orth_width-.width*.2.)/.orth_width; r2=0.};
        c2= {r1=0.; r2=(height-.width)/.height};
      }
      (p-center) + center)
    in
    let vec_clock90= Matrix.(apply clockwise_90 vec_o)
    and vec_anti90= Matrix.(apply anticlock_90 vec_o) in
    let vec_right= Line.extended_vec ~vec:vec_anti90 (o.width/.2.)
    and vec_left= Line.extended_vec ~vec:vec_clock90 (o.width/.2.)
    and vec_down= Line.extended_vec ~vec:vec_o (length/.2.)
    and vec_up= Line.extended_vec ~vec:vec_o_rev (length/.2.) in
    let left, right=
      let middle= Ops.((start + end') /< 2.) in
      Ops.(middle + vec_left, middle + vec_right)
    in
    let curve_u_r, curve_u_r_inner=
      let ctrl1= Ops.(start + vec_right /< 2.)
      and ctrl2= Ops.(right + vec_up /< 2.) in
      let curve=
        let end'= right in
        Ccurve {ctrl1; ctrl2;end'}
      and inner=
        let ctrl1= adjust ctrl2
        and ctrl2= adjust ctrl1
        and end'= adjust start in
        Ccurve {ctrl1; ctrl2;end'} in
      curve, inner
    and curve_d_r,curve_d_r_inner=
      let ctrl1= Ops.(right + vec_down /< 2.)
      and ctrl2= Ops.(end' + vec_right /< 2.) in
      let curve=
        let end'= end' in
        Ccurve {ctrl1; ctrl2;end'}
      and inner=
        let ctrl1= adjust ctrl2
        and ctrl2= adjust ctrl1
        and end'= adjust right in
        Ccurve {ctrl1; ctrl2;end'} in
      curve, inner
    and curve_d_l,curve_d_l_inner=
      let ctrl1= Ops.(end' + vec_left /< 2.)
      and ctrl2= Ops.(left + vec_down /< 2.) in
      let curve=
        let end'= left in
        Ccurve {ctrl1; ctrl2;end'}
      and inner=
        let ctrl1= adjust ctrl2
        and ctrl2= adjust ctrl1
        and end'= adjust end' in
        Ccurve {ctrl1; ctrl2;end'} in
      curve, inner
    and curve_u_l,curve_u_l_inner=
      let ctrl1= Ops.(left + vec_up /< 2.)
      and ctrl2= Ops.(start + vec_left /< 2.) in
      let curve=
        let end'= start in
        Ccurve {ctrl1; ctrl2;end'}
      and inner=
        let ctrl1= adjust ctrl2
        and ctrl2= adjust ctrl1
        and end'= adjust left in
        Ccurve {ctrl1; ctrl2;end'} in
      curve, inner
    in
    let path=
      let segments= [curve_u_r; curve_d_r; curve_d_l; curve_u_l] in
      {
        start;
        segments;
      }
    and path_inner=
      let start= adjust start
      and segments= [
        curve_u_l_inner;
        curve_d_l_inner;
        curve_d_r_inner;
        curve_u_r_inner;
        ] in
      {
        start;
        segments;
      }
    in
    [path; path_inner]

  (*
    type hj= {
      hj_start: point;
      length: float;
      end_: point adjust;
    } (* Horizontal – J hook *)
  *)

  let from_hj ?(width=Width.width) hj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= hj.hj_start in
    let right= { start with x= start.x +. hj.length } in
    let end'= get_adjust hj.end_
      ~f:(fun()-> let delta= width *. sqrt (3.*.3. /. 2.) in
        {x= right.x -. delta; y= right.y +. delta}) in
    let h0= start in
    let h1= { h0 with x= h0.x +. hj.length -. width*._a } in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h3= {
      x= h2.x +. width*.0.8;
      y= start.y +. width*._h*.0.8 } in
    (* insert p *)
    let h4= {
      x= min (h0.x +. width*.2.) h1.x;
      y= h0.y +. width *. _h;
      } in
    let h5= {
      x= h0.x +. width*._sah;
      y= h0.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let j_start= { x= h1.x -. width*.0.2; y= h1.y +. width *. _h} in
    let j_length= Point.distance ~from:j_start end' in
    let vec_p= Ops.(end' - j_start) in
    let vec_p_clock90= Matrix.(apply clockwise_90 vec_p) in
    let p1= { j_start with x= j_start.x +. width*.1.1 } in
    let r_ctrl=
      let p= Ops.(p1 + Line.extended_vec ~vec:vec_p (j_length *. 0.5)) in
      Ops.(p + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.4))
    in
    let segments= [
      Line h1;
      Line h2;
      Line h3;

      Line p1;
      Qcurve {ctrl= r_ctrl; end'};
      Line j_start;

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h0
      ] in
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

  let from_uj ?(width=Width.width) uj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= uj.uj_start in
    let vec_u= Ops.(uj.u_end - start) in
    let length= distance vec_u in
    let adjust p=
      let rotate= Matrix.{
        c1={r1=vec_u.x/.length;r2=vec_u.y/.length};
        c2={r1= -. vec_u.y/.length;r2= vec_u.x/.length}}
      in
      Ops.(Matrix.apply rotate (p-start) + start)
    in
    let right= { start with x= start.x +. length } in
    let end'= get_adjust uj.end_
      ~f:(fun()-> let delta= width *. sqrt (3.*.3. /. 2.) in
        {x= right.x -. delta; y= right.y +. delta}) in
    let h0= start in
    let h1= { h0 with x= h0.x +. length -. width*._a } in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h3= {
      x= h2.x +. width*.0.8;
      y= start.y +. width*._h*.0.8 } in
    (* insert p *)
    let h4= {
      x= min (h0.x +. width*.2.) h1.x;
      y= h0.y +. width *. _h;
      } in
    let h5= {
      x= h0.x +. width*._sah;
      y= h0.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let j_start= { x= h1.x -. width*.0.2; y= h1.y +. width *. _h} in
    let j_length= Point.distance ~from:j_start end' in
    let p1= { j_start with x= j_start.x +. width*.1.1 } in

    let h1, h2, h3, h4, h5, c5, p1, j_start=
      adjust h1, adjust h2, adjust h3, adjust h4, adjust h5, adjust c5, adjust p1, adjust j_start
    in
    let vec_p= Ops.(end' - j_start) in
    let vec_p_clock90= Matrix.(apply clockwise_90 vec_p) in
    let r_ctrl=
      let p= Ops.(p1 + Line.extended_vec ~vec:vec_p (j_length *. 0.5)) in
      Ops.(p + Line.extended_vec ~vec:vec_p_clock90 (width *. 0.4))
    in
    let segments= [
      Line h1;
      Line h2;
      Line h3;

      Line p1;
      Qcurve {ctrl= r_ctrl; end'};
      Line j_start;

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h0
      ] in
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

  let from_ht ?(width=Width.width) ht=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= ht.ht_start in
    let h_right= { start with x= start.x +. ht.length } in
    let end'= ht.end_ in

    let h1= { h_right with x= h_right.x -. width } in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h3= {
      x= h2.x +. width*.0.8;
      y= start.y +. width*._h*.0.8 } in
    let vec_t= Ops.(end' - h_right) in
    let t_length= Point.distance vec_t in
    let vec_t_clock90= Matrix.(apply clockwise_90 vec_t) in
    let vec_t_anti90= Matrix.(apply anticlock_90 vec_t) in
    let end_post= Ops.(end' + Line.extended_vec ~vec:vec_t_clock90 (width *.0.3)) in
    let ctrl1= get_adjust ht.ctrl1
      ~f:(fun()-> Ops.(h_right
      + Line.extended_vec ~vec:vec_t (t_length *. 0.3)
      + Line.extended_vec ~vec:vec_t_anti90 (width *. 1.0)
      )) in
    let ctrl2= get_adjust ht.ctrl2
      ~f:(fun()-> Ops.(h_right
      + Line.extended_vec ~vec:vec_t (t_length *. 0.7)
      + Line.extended_vec ~vec:vec_t_anti90 (width *. 0.9)
      )) in
    let r_ctrl1= ctrl1 in
    let r_ctrl2= ctrl2 in
    let l_ctrl2= Ops.(ctrl1 - Line.extended_vec ~vec:vec_t_anti90 (width *. 0.9)) in
    let l_ctrl1= Ops.(ctrl2 - Line.extended_vec ~vec:vec_t_anti90 (width *. 0.7)) in

    let projection=
      let vec= Ops.(r_ctrl1 - h_right) in
      let d= distance vec in
      let r= Float.abs(d /. vec.y) in
      width *. r
    in
    let t_top_left= { x= h1.x -. projection +. width ; y= h1.y +. width *. _h} in
    let t_top_right= { t_top_left with x= t_top_left.x +. projection } in

    (* insert t *)
    let h4= {
      x= min (start.x +. width*.2.) h1.x;
      y= start.y +. width *. _h;
      } in
    let h5= {
      x= start.x +. width*._sah;
      y= start.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let segments= [
      Line h1;
      Line h2;
      Line h3;

      Line t_top_right;
      Ccurve {ctrl1= r_ctrl1; ctrl2= r_ctrl2; end'};
      Line end_post;
      Ccurve {ctrl1= l_ctrl1; ctrl2= l_ctrl2; end'= t_top_left};

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line start
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

  let from_hsv ?(width=Width.width) hsv=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= hsv.hsv_start in
    let right= { start with x= start.x +. hsv.h_length } in
    let end'= get_adjust hsv.end_ ~f:(fun()->
      let base= hsv.h_length *. 0.8 in
      {x= right.x -. base*.2./.3.; y= right.y +. base}) in
    let h0= start in
    let h1= { h0 with x= h0.x +. hsv.h_length -. width*._a } in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h3= {
      x= h2.x +. width*.0.8;
      y= start.y +. width*._h*.0.8 } in
    (* insert v *)
    let h4= {
      x= min (h0.x +. width*.2.) h1.x;
      y= h0.y +. width *. _h;
      } in
    let h5= {
      x= h0.x +. width*._sah;
      y= h0.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let v_start= { x= h1.x -. width*.0.3; y= h1.y +. width *. _h} in
    let vec_p= Ops.(end' - v_start) in
    let vec_p_clock90= Matrix.(apply clockwise_90 vec_p) in
    let t1= { v_start with x= v_start.x +. width*.1.2 } in
    let t2= Ops.(end' + Line.extended_vec ~vec:vec_p_clock90 (width *.0.8)) in
    let segments= [
      Line h1;
      Line h2;
      Line h3;

      Line t1;
      Line end';
      Line t2;
      Line v_start;

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h0
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

  let from_hv ?(width=Width.width) hv=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= hv.hv_start in
    let v_length= get_adjust hv.v_length ~f:(fun ()-> hv.h_length) in
    let right= { start with x= start.x +. hv.h_length } in
    let end'= { right with y= right.y +. v_length } in
    let h0= start in
    let h1= {right with x= right.x -. width} in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h3= {
      x= h2.x +. width*.0.8;
      y= start.y +. width*._h*.0.8 } in
    (* insert v *)
    let h4= {
      x= min (h0.x +. width*.2.) h1.x;
      y= h0.y +. width *. _h;
      } in
    let h5= {
      x= h0.x +. width*._sah;
      y= h0.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let v_top_left= { x= h1.x ; y= h1.y +. width *. _h} in
    let v_top_right= { v_top_left with x= v_top_left.x +. width*.1. } in
    let pre_end= { end' with y= end'.y -. width *. 0.25 } in
    let post_end= { end' with x= end'.x -. width } in
    let segments= [
      Line h1;
      Line h2;
      Line h3;

      Line v_top_right;
      Line pre_end;
      Line post_end;
      Line v_top_left;

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h0
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

  let from_hvj ?(width=Width.width) hvj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= hvj.hvj_start in
    let v_length= get_adjust hvj.v_length ~f:(fun ()-> hvj.h_length) in
    let right= { start with x= start.x +. hvj.h_length } in
    let v_end= { right with y= right.y +. v_length } in
    let h0= start in
    let h1= {right with x= right.x -. width} in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h_right= {
      x= h2.x +. width*.0.8;
      y= start.y +. width*._h*.0.8 } in
    (* insert v j *)
    let h4= {
      x= min (h0.x +. width*.2.) h1.x;
      y= h0.y +. width *. _h;
      } in
    let h5= {
      x= h0.x +. width*._sah;
      y= h0.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in
    let v_top_left= { x= h1.x ; y= h1.y +. width *. _h} in
    let v_top_right= { v_top_left with x= v_top_left.x +. width*.1. } in
    let j_start= { v_end with y= v_end.y -. width *. 2. } in
    let j_down= {x= j_start.x -. width *. 1.5; y= v_end.y} in
    let c_j_start_down= {x=j_start.x; y= j_down.y} in
    let j_end= get_adjust hvj.end_ ~f:(fun()->
      {x= j_start.x -. width *. 4.; y= j_down.y -. width *. 1.5}
    ) in
    let c_j_end= {x= j_down.x; y= j_down.y-. width*.0.8} in
    let vec_j= Ops.(j_end - j_start) in
    let pre_end= Ops.(j_end +
      Line.extended_vec ~vec:(Matrix.(apply anticlock_90 vec_j)) (width*.0.2)
    ) in
    let vec_j_up= let open Ops in
      let p_toward= { x= j_start.x; y= v_end.y -. width *. 1. } in
      p_toward - j_end in
    let j_v_left= {x= j_start.x -. width; y= j_start.y} in
    let pre_j_v_left= let open Ops in
      let line1= Line.of_points j_end (j_end + Line.extended_vec ~vec:vec_j_up 1.)
      and line2= Line.of_points v_top_left j_v_left in
      let x= Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
      x - Line.extended_vec ~vec:vec_j_up (width*.0.5)
    in
    let c_j_v_left=
      let line1= Line.of_points v_top_left j_v_left
      and line2= Line.of_points j_end pre_j_v_left in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point)
    in
    let segments= [
      Line h1;
      Line h2;
      Line h_right;

      Line v_top_right;
      Line j_start;
      Qcurve {ctrl= c_j_start_down; end'= j_down};
      Qcurve {ctrl= c_j_end; end'=pre_end};
      Line j_end;
      Line pre_j_v_left;
      Qcurve {ctrl= c_j_v_left; end'=j_v_left};
      Line v_top_left;

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h0
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

  let from_htj ?(width=Width.width) htj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= htj.htj_start in
    let right= { start with x= start.x +. htj.length } in
    let t_end= htj.t_end in
    let vec_t= Ops.(t_end - right) in
    let vec_t_anti90= Matrix.(apply anticlock_90 vec_t) in
    let t_length= distance vec_t in
    let h0= start in
    let h1= {right with x= right.x -. width} in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h_right= {
      x= h2.x +. width*.0.8;
      y= start.y +. width*._h*.0.8 } in
    (* insert t j *)
    let h4= {
      x= min (h0.x +. width*.2.) h1.x;
      y= h0.y +. width *. _h;
      } in
    let h5= {
      x= h0.x +. width*._sah;
      y= h0.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in
    let t_top_left= { x= h1.x ; y= h1.y +. width *. _h} in
    let t_top_right= { t_top_left with x= t_top_left.x +. width*.1. } in
    let ctrl1= get_adjust htj.ctrl1
      ~f:(fun()-> Ops.(right
      + Line.extended_vec ~vec:vec_t (t_length *. 0.5)
      + Line.extended_vec ~vec:vec_t_anti90 (width *. 1.5)
      )) in
    let ctrl2= get_adjust htj.ctrl2
      ~f:(fun()-> Ops.(right
      + Line.extended_vec ~vec:vec_t (t_length *. 0.9)
      + Line.extended_vec ~vec:vec_t_anti90 (width *. 0.8)
      )) in
    let r_ctrl1= ctrl1 in
    let r_ctrl2= ctrl2 in
    let l_t_ctrl= {r_ctrl1 with x= r_ctrl1.x -. width} in
    let j_start= t_end in
    let j_end= get_adjust htj.end_ ~f:(fun()->
      {x= j_start.x -. width *. 2.0; y= j_start.y -. width *. 1.5}
    ) in
    let c_j_end= {x= j_start.x; y= j_start.y-. width*.0.8} in
    let vec_j= Ops.(j_end - j_start) in
    let j_length= distance vec_j in
    let pre_end= Ops.(j_end +
      Line.extended_vec ~vec:(Matrix.(apply anticlock_90 vec_j)) (width*.0.2)
    ) in
    let vec_j_up= let open Ops in
      let p_toward= { x= t_top_right.x; y= t_end.y -. width *. 1. } in
      p_toward - j_end in
    let j_t_left=
      let right= Bezier.lerp3 t_top_right r_ctrl1 r_ctrl2 t_end 0.7 in
      let before= Bezier.lerp3 t_top_right r_ctrl1 r_ctrl2 t_end 0.6999 in
      let after= Bezier.lerp3 t_top_right r_ctrl1 r_ctrl2 t_end 0.7001 in
      let vec_t_0d8= Ops.(after - before) in
      Ops.(right +
        Line.extended_vec ~vec:Matrix.(apply clockwise_90 vec_t_0d8) (width*.1.0))
    in
    let pre_j_v_left= let open Ops in
      j_end + Line.extended_vec ~vec:vec_j_up (j_length -. width*.1.5)
    in
    let c_j_t_left=
      let line1= Line.of_points l_t_ctrl j_t_left
      and line2= Line.of_points j_end pre_j_v_left in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point)
    in
    let segments= [
      Line h1;
      Line h2;
      Line h_right;

      Line t_top_right;
      Ccurve {ctrl1=r_ctrl1; ctrl2=r_ctrl2; end'= t_end};
      Qcurve {ctrl= c_j_end; end'=pre_end};
      Line j_end;
      Line pre_j_v_left;
      Qcurve {ctrl= c_j_t_left; end'=j_t_left};
      Qcurve {ctrl= l_t_ctrl; end'= t_top_left};

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h0
      ] in
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

  let from_utj ?(width=Width.width) utj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= utj.utj_start in
    let vec_u= Ops.(utj.t_start - start) in
    let u_length= distance vec_u in
    let adjust p=
      let matrix= Matrix.{
        c1= { r1= vec_u.x /. u_length; r2= vec_u.y /. u_length };
        c2= { r1= -. vec_u.y /. u_length; r2= vec_u.x /. u_length};
        }
      in
      Ops.(Matrix.apply matrix (p - start) + start)
    in
    let right= { start with x= start.x +. u_length } in
    let t_end= utj.t_end in
    let h0= start in
    let h1= {right with x= right.x -. width} in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h_right= {
      x= h2.x +. width*.0.8;
      y= start.y +. width*._h*.0.8 } in
    (* insert t j *)
    let h4= {
      x= min (h0.x +. width*.2.) h1.x;
      y= h0.y +. width *. _h;
      } in
    let h5= {
      x= h0.x +. width*._sah;
      y= h0.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in
    let t_top_left= { x= h1.x ; y= h1.y +. width *. _h} in
    let t_top_right= { t_top_left with x= t_top_left.x +. width*.1. } in
    let right, h1, h2, h_right, h4, h5, c5, t_top_left, t_top_right=
      adjust right, adjust h1, adjust h2, adjust h_right,
      adjust h4, adjust h5, adjust c5,
      adjust t_top_left,adjust t_top_right
    in
    let vec_t= Ops.(t_end - right) in
    let vec_t_anti90= Matrix.(apply anticlock_90 vec_t) in
    let t_length= distance vec_t in
    let ctrl1= get_adjust utj.ctrl1
      ~f:(fun()-> Ops.(right
      + Line.extended_vec ~vec:vec_t (t_length *. 0.5)
      + Line.extended_vec ~vec:vec_t_anti90 (width *. 1.5)
      )) in
    let ctrl2= get_adjust utj.ctrl2
      ~f:(fun()-> Ops.(right
      + Line.extended_vec ~vec:vec_t (t_length *. 0.9)
      + Line.extended_vec ~vec:vec_t_anti90 (width *. 0.8)
      )) in
    let r_ctrl1= ctrl1 in
    let r_ctrl2= ctrl2 in
    let l_t_ctrl= {r_ctrl1 with x= r_ctrl1.x -. width} in
    let j_start= t_end in
    let j_end= get_adjust utj.end_ ~f:(fun()->
      {x= j_start.x -. width *. 2.5; y= j_start.y -. width *. 1.5}
    ) in
    let c_j_end= {x= j_start.x; y= j_start.y-. width*.0.8} in
    let vec_j= Ops.(j_end - j_start) in
    let j_length= distance vec_j in
    let pre_end= Ops.(j_end +
      Line.extended_vec ~vec:(Matrix.(apply anticlock_90 vec_j)) (width*.0.2)
    ) in
    let vec_j_up= let open Ops in
      let p_toward= { x= t_top_right.x; y= t_end.y -. width *. 1. } in
      p_toward - j_end in
    let j_t_left=
      let right= Bezier.lerp3 t_top_right r_ctrl1 r_ctrl2 t_end 0.7 in
      let before= Bezier.lerp3 t_top_right r_ctrl1 r_ctrl2 t_end 0.6999 in
      let after= Bezier.lerp3 t_top_right r_ctrl1 r_ctrl2 t_end 0.7001 in
      let vec_t_0d8= Ops.(after - before) in
      Ops.(right +
        Line.extended_vec ~vec:Matrix.(apply clockwise_90 vec_t_0d8) (width*.1.0))
    in
    let pre_j_v_left= let open Ops in
      j_end + Line.extended_vec ~vec:vec_j_up (j_length -. width*.1.5)
    in
    let c_j_t_left=
      let line1= Line.of_points l_t_ctrl j_t_left
      and line2= Line.of_points j_end pre_j_v_left in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point)
    in
    let segments= [
      Line h1;
      Line h2;
      Line h_right;

      Line t_top_right;
      Ccurve {ctrl1=r_ctrl1; ctrl2=r_ctrl2; end'= t_end};
      Qcurve {ctrl= c_j_end; end'=pre_end};
      Line j_end;
      Line pre_j_v_left;
      Qcurve {ctrl= c_j_t_left; end'=j_t_left};
      Qcurve {ctrl= l_t_ctrl; end'= t_top_left};

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h0
      ] in
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

  let from_hvh ?(width=Width.width) hvh=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= hvh.hvh_start in
    let h1_length= hvh.h1_length
    and h2_length= hvh.h2_length
    and v_length= hvh.v_length in
    let h1_right= { start with x= start.x +. h1_length } in
    let down= { h1_right with y= h1_right.y +. v_length } in
    let end'= { down with x= down.x +. h2_length} in
    let h1_0= start in
    let h1_1= {h1_right with x= h1_right.x -. width} in
    let h1_2= {
      x= h1_1.x +. width*._a;
      y= h1_1.y -. width*._a; } in
    let h1_3= {
      x= h1_2.x +. width*.0.8;
      y= start.y +. width*._h*.0.8 } in
    (* insert v *)
    let h1_4= {
      x= min (h1_0.x +. width*.2.) h1_1.x;
      y= h1_0.y +. width *. _h;
      } in
    let h1_5= {
      x= h1_0.x +. width*._sah;
      y= h1_0.y +. width*._sah;
      } in
    let h1_c5= { h1_4 with
      x= (h1_5.x +. h1_4.x) *. 0.5;
      } in

    let h2_right= end' in
    let h2_1= {h2_right with x= h2_right.x -. width} in
    let h2_2= {
      x= h2_1.x +. width*._a;
      y= h2_1.y -. width*._a; } in
    let h2_3= {
      x= h2_2.x +. width*.1.;
      y= down.y +. width*._h*.1. } in
    let h2_4= {
      x= down.x;
      y= down.y +. width*._h } in
    let h2_5= {
      x= h2_4.x -. width*._a;
      y= h2_4.y +. width*._h } in
    let h2_6= {
      x= h2_5.x -. width;
      y= h2_5.y -. width*._h*.2. } in
    let h2_7= {
      x= down.x -. width;
      y= down.y -. width*._h } in

    let v_top_left= { x= h1_1.x ; y= h1_1.y +. width *. _h} in
    let v_top_right= { v_top_left with x= v_top_left.x +. width*.1. } in
    let segments= [
      Line h1_1;
      Line h1_2;
      Line h1_3;
      Line v_top_right;

      Line down;
      Line h2_1;
      Line h2_2;
      Line h2_3;
      Line h2_4;
      Line h2_5;
      Line h2_6;
      Line h2_7;

      Line v_top_left;
      Line h1_4;
      Qcurve { ctrl= h1_c5; end'= h1_5 };
      Line h1_0
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

  let from_hvu ?(width=Width.width) hvu=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= hvu.hvu_start in
    let v_length= hvu.v_length in
    let right= { start with x= start.x +. hvu.h_length } in
    let down= { right with y= right.y +. v_length } in
    let end'= get_adjust hvu.end_ ~f:(fun()->
      { x= down.x +. width *. 2.5;
        y= down.y -. width *. 4.;
      }) in
    let h0= start in
    let h1= {right with x= right.x -. width} in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h3= {
      x= h2.x +. width*.0.8;
      y= start.y +. width*._h*.0.8 } in
    (* insert v *)
    let h4= {
      x= min (h0.x +. width*.2.) h1.x;
      y= h0.y +. width *. _h;
      } in
    let h5= {
      x= h0.x +. width*._sah;
      y= h0.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let v_top_left= { x= h1.x ; y= h1.y +. width *. _h} in
    let v_top_right= { v_top_left with x= v_top_left.x +. width*.1. } in

    let vec_u= Ops.(end' - down) in
    let vec_u_clock90= Matrix.(apply clockwise_90 vec_u) in
    let u_start=
      let r_u= radian vec_u in
      let r_u= if r_u > pi then pi *. 2. -. r_u  else r_u in
      let adjust= Float.pow (r_u /. (pi/.2.)*.1.2) 2. in
      { down with y= down.y -. width *. (1.2+.adjust) } in
    let post_end= Ops.(end' + Line.extended_vec ~vec:vec_u_clock90 (width*.0.2)) in
    let c_u_down=
      let p= Ops.(down *< 0.7 + end' *< 0.3) in
      Ops.(p - Line.extended_vec ~vec:vec_u_clock90 (width*.0.1)) in
    let u_left= {
      x= down.x -. width *. 1.2;
      y= down.y -. width *. 1.4;
    } in
    let u_v= {
      x= down.x -. width;
      y= u_left.y -. width;
    } in
    let c_u_left= {
      x= down.x -. width;
      y= (u_left.y +. u_v.y)/.2.;
    } in

    let segments= [
      Line h1;
      Line h2;
      Line h3;

      Line v_top_right;
      Line u_start;
      Line end';
      Line post_end;
      Qcurve { ctrl= c_u_down; end'= down };
      Line u_left;
      Qcurve { ctrl= c_u_left; end'= u_v };

      Line v_top_left;

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h0
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

  let from_ha ?(width=Width.width) ha=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= ha.ha_start in
    let v_length= ha.v_length in
    let right= { start with x= start.x +. ha.h1_length } in
    let down= { right with y= right.y +. v_length } in
    let end'= { down with x= down.x +. ha.h2_length } in
    let radius= get_adjust ha.a_radius ~f:(fun()-> width) in
    let h0= start in
    let h1= {right with x= right.x -. width} in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h3= {
      x= h2.x +. width*.0.8;
      y= start.y +. width*._h*.0.8 } in
    (* insert v *)
    let h4= {
      x= min (h0.x +. width*.2.) h1.x;
      y= h0.y +. width *. _h;
      } in
    let h5= {
      x= h0.x +. width*._sah;
      y= h0.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let v_top_left= { x= h1.x ; y= h1.y +. width *. _h} in
    let v_top_right= { v_top_left with x= v_top_left.x +. width } in

    let a_start_right=
      { down with y= down.y -. width*.0.8 -. radius *. 0.9 } in
    let a_start_left=
      { x= down.x -. width; y= down.y -. radius *. 1.2 } in
    let h2_up_left=
      { x= right.x +. radius *. 0.8; y= down.y -. width *.0.9 } in
    let h2_up_right=
      { x= end'.x -. width; y= end'.y -. width } in
    let h2_up_c=
      let middle= Ops.((h2_up_left + h2_up_right) /< 2.) in
      { middle with y= middle.y +. width *. 0.2 } in
    let h2_down_left=
      { x= h2_up_left.x -. width*.0.6; y = down.y } in
    let h2_down_right=
      { h2_up_right with y = h2_up_right.y +. width } in
    let h2_down_c=
      let middle= Ops.((h2_down_left + h2_down_right) /< 2.) in
      { middle with y= middle.y +. width *. 0.2 } in
    let v_h2_up_c=
      let line1= Line.of_points v_top_right a_start_right
      and line2= Line.of_points h2_up_left h2_up_c in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
    let v_h2_down_c=
      let line1= Line.of_points v_top_left a_start_left
      and line2= Line.of_points h2_down_left h2_down_c in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
    let end_up_c= Ops.(h2_up_right +
      Line.extended_vec ~vec:(h2_up_right - h2_up_c) (width *. 0.4)
      )
    and end_down_c= Ops.(h2_down_right +
      Line.extended_vec ~vec:(h2_down_right - h2_down_c) (width *. 1.2)
      ) in
    let segments= [
      Line h1;
      Line h2;
      Line h3;

      Line v_top_right;
      Line a_start_right;
      Qcurve { ctrl= v_h2_up_c; end'= h2_up_left };
      Qcurve { ctrl= h2_up_c; end'= h2_up_right };

      Ccurve { ctrl1= end_up_c; ctrl2= end_down_c; end'= h2_down_right; };

      Qcurve { ctrl= h2_down_c; end'= h2_down_left };
      Qcurve { ctrl= v_h2_down_c; end'= a_start_left };

      Line v_top_left;

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h0
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

  let from_haj ?(width=Width.width) haj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= haj.haj_start in
    let v_length= haj.v_length in
    let right= { start with x= start.x +. haj.h1_length } in
    let down= { right with y= right.y +. v_length } in
    let h2_end= { down with x= down.x +. haj.h2_length } in
    let j_start= {x= h2_end.x -. width; y= h2_end.y -. width} in
    let j_right= {x= h2_end.x; y= j_start.y -. width *. 0.1 } in
    let end'= get_adjust haj.end_ ~f:(fun()->
      { x= h2_end.x -. width; y= h2_end.y -. width *. 3.5 }) in
    let vec_j= Ops.(end' - j_start) in
    let vec_j_clock90= Matrix.(apply clockwise_90 vec_j) in
    let j_length= distance vec_j in
    let end_post= Ops.(end' +
      Line.extended_vec ~vec:vec_j_clock90 (width*.0.2)) in
    let radius= get_adjust haj.a_radius ~f:(fun()-> width) in
    let h0= start in
    let h1= {right with x= right.x -. width} in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h3= {
      x= h2.x +. width*.0.8;
      y= start.y +. width*._h*.0.8 } in
    (* insert v *)
    let h4= {
      x= min (h0.x +. width*.2.) h1.x;
      y= h0.y +. width *. _h;
      } in
    let h5= {
      x= h0.x +. width*._sah;
      y= h0.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let v_top_left= { x= h1.x ; y= h1.y +. width *. _h} in
    let v_top_right= { v_top_left with x= v_top_left.x +. width } in

    let a_start_right=
      { down with y= down.y -. width*.0.8 -. radius *. 0.9 } in
    let a_start_left=
      { x= down.x -. width; y= down.y -. radius *. 1.2 } in
    let h2_up_left=
      { x= right.x +. radius *. 0.8; y= down.y -. width *.0.9 } in
    let h2_up_right=
      { x= j_start.x -. width*.0.8; y= j_start.y } in
    let h2_up_c=
      let middle= Ops.((h2_up_left + h2_up_right) /< 2.) in
      { middle with y= middle.y +. width *. 0.1 } in
    let h2_down_left=
      { x= h2_up_left.x -. width*.0.6; y = down.y } in
    let h2_down_right=
      { h2_up_right with y = h2_up_right.y +. width } in
    let h2_down_c=
      let middle= Ops.((h2_down_left + h2_down_right) /< 2.) in
      { middle with y= middle.y +. width *. 0.2 } in
    let v_h2_up_c=
      let line1= Line.of_points v_top_right a_start_right
      and line2= Line.of_points h2_up_left h2_up_c in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
    let vec_h2_up_right= Ops.(h2_up_right - h2_up_c) in
    let h2_j_c1= Ops.( h2_up_right+
      Line.extended_vec ~vec:vec_h2_up_right (j_length*.0.25)) in
    let h2_j_c2= Ops.(end' -
      Line.extended_vec ~vec:vec_j (j_length*.0.133)) in
    let j_h2_right_c= {
      x= end_post.x*.0.95+.j_right.x*.0.05;
      y= j_right.y+.width*.0.1} in
    let v_h2_down_c=
      let line1= Line.of_points v_top_left a_start_left
      and line2= Line.of_points h2_down_left h2_down_c in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
    let end_down_c= Ops.(h2_down_right +
      Line.extended_vec ~vec:(h2_down_right - h2_down_c) (width *. 1.6)
      ) in
    let segments= [
      Line h1;
      Line h2;
      Line h3;

      Line v_top_right;
      Line a_start_right;
      Qcurve { ctrl= v_h2_up_c; end'= h2_up_left };
      Qcurve { ctrl= h2_up_c; end'= h2_up_right };

      Ccurve { ctrl1= h2_j_c1; ctrl2= h2_j_c2; end'; };
      Line end_post;
      Qcurve { ctrl= j_h2_right_c; end'= j_right };

      Qcurve { ctrl= end_down_c; end'= h2_down_right; };

      Qcurve { ctrl= h2_down_c; end'= h2_down_left };
      Qcurve { ctrl= v_h2_down_c; end'= a_start_left };

      Line v_top_left;

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h0
      ] in
    {
      start;
      segments;
    }

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

  let from_hpj ?(width=Width.width) hpj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= hpj.hpj_start in
    let h0= start in
    let h1= { h0 with x= h0.x +. hpj.length -. width*._a } in
    let h2= {
      x= h1.x +. width*.0.6;
      y= h1.y -. width*.0.6; } in
    let h3= {
      x= h2.x +. width*.0.7;
      y= start.y +. width*._h*.0.5 } in
    (* insert p *)
    let h4= {
      x= min (h0.x +. width*.2.) h1.x;
      y= h0.y +. width *. _h;
      } in
    let h5= {
      x= h0.x +. width*._sah;
      y= h0.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let p_start= { x= h1.x; y= h1.y +. width *. _h}
    and p_end= hpj.p_end in
    let p_length= Point.distance ~from:p_start p_end in
    let vec_p= Ops.(p_end - p_start) in
    let vec_p_clock90= Matrix.(apply clockwise_90 vec_p) in
    let p1= { p_start with x= p_start.x +. width} in
    let ctrl1= get_adjust hpj.ctrl1
      ~f:(fun()-> {x= p1.x; y= p_start.y+.(p_end.y-.p_start.y)*.0.7}) in
    let ctrl2= get_adjust hpj.ctrl2 ~f:(fun()->
      { x= (p_start.x +. p_end.x) /. 2.;
        y= p_start.y+.(p_end.y-.p_start.y)*.0.8;
      }) in
    let _up_ctrl2= ctrl2 in
    let r_ctrl1= ctrl1 in
    let r_ctrl2= ctrl2 in
    let p2= {
      x= p_end.x +. width*.0.5;
      y= p_end.y -. width;
    } in
    let lerp_p_up= Bezier.lerp3 p1 r_ctrl1 r_ctrl2 p2 in
    let end'= get_adjust hpj.end_ ~f:(fun()->
      let j_base= lerp_p_up (1. -. width*.(0.8+.0.1) /. p_length) in
      { x= j_base.x;
        y= p2.y -. width *. 3.5;
      }) in
    let pre_end= {end' with x= end'.x -. width*.0.1}
    and post_end= {end' with x= end'.x +. width*.0.1} in
    let j_base_left= lerp_p_up (1. -. width*.(0.8+.0.2+.0.5) /. p_length) in
    let j_start= lerp_p_up (1. -. width*.(0.8+.0.2+.1.0) /. p_length) in
    let p_j_left_c=
      let line1= Line.of_points r_ctrl2 j_start
      and line2= Line.of_points pre_end j_base_left in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
    let vec_end_p2= Ops.(p2 - post_end) in
    let p_j_right_c=
      let open Ops in
      let vec= Matrix.(apply clockwise_90 vec_end_p2) in
      let base= post_end *< 0.05 + p2 *< 0.95 in
      base + Line.extended_vec ~vec (width*.0.6)
    in
    let l_ctrl1= Ops.(ctrl2 +
      Line.extended_vec ~vec:vec_p_clock90 (width *. 1.50)) in
    let l_ctrl2= Ops.(ctrl1 +
      Line.extended_vec ~vec:vec_p_clock90 (width *. 1.0)) in
    let lerp_p_down= Bezier.lerp3 p_start l_ctrl2 l_ctrl1 p_end in
    let p_end_post=lerp_p_down 0.9 in
    let c2= let open Ops in
      let vec= p_end_post - l_ctrl1 in
      p_end_post + Line.extended_vec ~vec width
    in
    let segments= [
      Line h1;
      Line h2;
      Line h3;

      Line p1;
      Ccurve {ctrl1= r_ctrl1; ctrl2= r_ctrl2; end'= j_start};

      Qcurve {ctrl= p_j_left_c; end'= pre_end};
      Line post_end;
      Qcurve {ctrl= p_j_right_c; end'= p2};

      Qcurve {ctrl= c2; end'= p_end_post};
      Ccurve {ctrl1= l_ctrl1; ctrl2= l_ctrl2; end'= p_start};

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h0
      ] in
    {
      start;
      segments;
    }

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

  let from_htaj ?(width=Width.width) (htaj:htaj)=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let a_radius= get_adjust htaj.a_radius ~f:(fun ()->
      let avg= (htaj.h1_length +. htaj.h2_length) /. 2. in
      avg /. 6.)
    in
    let start= htaj.htaj_start in
    let h1_end= { start with x= start.x +. htaj.h1_length } in
    let a_end= htaj.a_end in
    let h2_end= { a_end with y= a_end.y -. width} in
    let h2_start= { h2_end with
      x= h2_end.x -. htaj.h2_length;
    } in
    let vec_h2= Ops.(h2_end - h2_start) in
    let h2_length= distance vec_h2 in
    let end'= get_adjust htaj.end_ ~f:(fun()->
      { x= h2_end.x -. width; y= h2_end.y -. width *. 3.5}) in
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
      (h2_start+h2_end) /< 2. +
      Line.extended_vec
        ~vec:Matrix.(apply clockwise_90 vec_h2)
        (h2_length*.0.05)) in
    let a_c=
      let line1= Line.of_points t_c t_end
      and line2= Line.of_points h2_start h2_c in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point)in

    let h0= start in
    let h1= {h1_end with x= h1_end.x -. width} in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h3= {
      x= h2.x +. width*.0.8;
      y= start.y +. width*._h*.0.8 } in
    (* insert v *)
    let h4= {
      x= min (h0.x +. width*.2.) h1.x;
      y= h0.y +. width *. _h;
      } in
    let h5= {
      x= h0.x +. width*._sah;
      y= h0.y +. width*._sah;
      } in
    let h5_c= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let projection=
      let vec= Ops.(t_c - h1_end) in
      let d= distance vec in
      let r= Float.abs(d /. vec.y) in
      width *. r
    in
    let t_top_left= { x= h1.x -. projection +. width ; y= h1.y +. width *. _h} in
    let t_top_right= { t_top_left with x= t_top_left.x +. projection } in
    let r_t_c= t_c in
    let r_a_c= a_c in
    let up_h2_c= h2_c in
    let l_t_c= Ops.(t_c +
      Line.extended_vec ~vec:Matrix.(apply clockwise_90 vec_t) width) in
    let l_a_c= Ops.(a_c +
      Line.extended_vec ~vec:(a_c - a_center) (width*.1.5)) in
    let down_h2_c= {h2_c with y= h2_c.y+.width} in

    let h2_start_down= let open Ops in
      let vec= Matrix.(apply clockwise_90 (up_h2_c - h2_start)) in
      h2_start + Line.extended_vec ~vec (width) in
    let t_end_l= let open Ops in
      let vec= Matrix.(apply clockwise_90 (t_end - r_t_c)) in
      Ops.(t_end + Line.extended_vec ~vec width) in

    let j_start= {h2_end with x= h2_end.x -. width*.1.2} in
    let j_right= {x= h2_end.x; y= j_start.y -. width *. 0.1 } in
    let a_end_post= { x= j_start.x-.width*.0.6; y= h2_end.y +. width } in
    let vec_j= Ops.(end' - j_start) in
    let vec_j_clock90= Matrix.(apply clockwise_90 vec_j) in
    let j_length= distance vec_j in
    let end_post= Ops.(end' +
      Line.extended_vec ~vec:vec_j_clock90 (width*.0.2)) in

    let h2_up_right=
      { x= j_start.x -. width*.0.8; y= j_start.y } in

    let vec_h2_up_right= Ops.(h2_up_right - up_h2_c) in
    let h2_j_c1= Ops.( h2_up_right+
      Line.extended_vec ~vec:vec_h2_up_right (j_length*.0.25)) in
    let h2_j_c2= Ops.(end' -
      Line.extended_vec ~vec:vec_j (j_length*.0.133)) in
    let j_h2_right_c= {
      x= end_post.x*.0.95+.j_right.x*.0.05;
      y= j_right.y+.width*.0.1} in
    let end_down_c= Ops.(a_end_post +
      Line.extended_vec ~vec:(a_end_post - down_h2_c) (width *. 1.6)
      ) in
    let segments= [
      Line h1;
      Line h2;
      Line h3;
      Line t_top_right;
      Qcurve {ctrl= r_t_c; end'= t_end};
      Qcurve {ctrl= r_a_c; end'= h2_start};

      Qcurve { ctrl= up_h2_c; end'= h2_up_right };

      Ccurve { ctrl1= h2_j_c1; ctrl2= h2_j_c2; end'; };
      Line end_post;
      Qcurve { ctrl= j_h2_right_c; end'= j_right };

      Qcurve { ctrl= end_down_c; end'=a_end_post; };

      Qcurve {ctrl= down_h2_c; end'= h2_start_down};
      Qcurve {ctrl= l_a_c; end'= t_end_l};
      Qcurve {ctrl= l_t_c; end'= t_top_left};

      Line h4;
      Qcurve { ctrl= h5_c; end'= h5 };
      Line h0
    ] in
    {
      start;
      segments;
    }

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

  let from_htc ?(width=Width.width) htc=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= htc.htc_start in
    let t_end= htc.t_end in
    let h0= start in
    let h1= { h0 with x= h0.x +. htc.h_length -. width*._a } in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h3= {
      x= h2.x +. width*.0.8;
      y= start.y +. width*._h*.0.8 } in
    (* insert p *)
    let h4= {
      x= min (h0.x +. width*.2.) h1.x;
      y= h0.y +. width *. _h;
      } in
    let h5= {
      x= h0.x +. width*._sah;
      y= h0.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let t_start= { x= h1.x -. width*.0.1; y= h1.y +. width *. _h} in
    let t1= { t_start with x= t_start.x +. width*.1. } in
    let vec_t= Ops.(t_end - t1) in
    let t_length= Point.distance vec_t in
    let vec_t_anti90= Matrix.(apply anticlock_90 vec_t) in
    let t_ctrl1= get_adjust htc.t_ctrl1
      ~f:(fun()-> Ops.(t1
      + Line.extended_vec ~vec:vec_t (t_length *. 0.3)
      + Line.extended_vec ~vec:vec_t_anti90 (width *. 0.5)
      )) in
    let t_ctrl2= get_adjust htc.t_ctrl2
      ~f:(fun()-> Ops.(t1
      + Line.extended_vec ~vec:vec_t (t_length *. 0.7)
      + Line.extended_vec ~vec:vec_t_anti90 (width *. 0.6)
      )) in
    let t_r_ctrl1= t_ctrl1 in
    let t_r_ctrl2= t_ctrl2 in
    let t_l_ctrl2= Ops.(t_ctrl1 - Line.extended_vec ~vec:vec_t_anti90 (width *. 0.5)) in
    let t_l_ctrl1= Ops.(t_ctrl2 - Line.extended_vec ~vec:vec_t_anti90 (width *. 0.4)) in

    let c_start= htc.t_end
    and end'= htc.end_ in
    let c_vec= Ops.(end' - c_start) in
    let c_vec_anti90= Matrix.(apply anticlock_90 c_vec) in
    let c_vec_anti75= Matrix.(apply (anticlock ~radian:(pi*.75./.180.)) c_vec) in
    let c_length= distance c_vec in
    let c_r= c_length /. 2. in
    let c_ctrl1, c_ctrl2=
      let ctrl_vec=
        Line.extended_vec ~vec:c_vec_anti90
          (circle_ctrl_distance_raw ~seg:2.3 c_r) in
      let ctrl_vec_80=
        Line.extended_vec ~vec:c_vec_anti75
          (circle_ctrl_distance_raw ~seg:2.1 c_r) in
      let ctrl1= get_adjust htc.c_ctrl1 ~f:(fun ()->
        Ops.(c_start + ctrl_vec_80)) in
      let ctrl2= get_adjust htc.c_ctrl2 ~f:(fun ()->
        Ops.(end' + ctrl_vec)) in
      (ctrl1, ctrl2)
    in
    let c_vec_down_end=
      let arc= Ops.(end' - c_ctrl2) in
      Matrix.(apply clockwise_90 arc)
    in
    let c_arc_right=
      let ctrl1= Ops.(c_ctrl1 + Line.extended_vec ~vec:c_vec_anti90 (width*.0.5))
      and ctrl2= Ops.(c_ctrl2 + Line.extended_vec ~vec:c_vec_anti90 (width*.0.5)) in
      Ccurve {ctrl1;ctrl2;end'} in
    let c_line_down=
      let p= Ops.(+) end' @@
        Line.extended_vec ~vec:c_vec_down_end (width *. 0.2) in
      Line p in
    let c_arc_left=
      let end'= { t_end with x= t_end.x -. width*.0.5 } in
      let ctrl1= Ops.(c_ctrl2 - Line.extended_vec ~vec:c_vec_anti90 (width*.0.5))
      and ctrl2= Ops.(c_ctrl1 - Line.extended_vec ~vec:c_vec_anti90 (width*.0.5)) in
      Ccurve {ctrl1;ctrl2;end'} in

    let segments= [
      Line h1;
      Line h2;
      Line h3;

      Line t1;
      Ccurve {ctrl1= t_r_ctrl1; ctrl2= t_r_ctrl2; end'= t_end};

      c_arc_right; c_line_down; c_arc_left;

      Ccurve {ctrl1= t_l_ctrl1; ctrl2= t_l_ctrl2; end'= t_start};

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h0
      ] in
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

  let from_htht ?(width=Width.width) htht=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= htht.htht_start in

    let ht1_start= htht.htht_start in
    let ht1_right= { ht1_start with x= ht1_start.x +. htht.h1_length } in
    let ht1_end= htht.t1_end in
    let ht1_end_h2= {ht1_end with x= ht1_end.x +. width } in

    let h1_1= { ht1_right with x= ht1_right.x -. width } in
    let h1_2= {
      x= h1_1.x +. width*._a;
      y= h1_1.y -. width*._a; } in
    let h1_3= {
      x= h1_2.x +. width*.0.8;
      y= ht1_start.y +. width*._h*.0.8 } in
    let t1_vec= Ops.(ht1_end - ht1_right) in
    let t1_length= Point.distance t1_vec in
    let t1_vec_anti90= Matrix.(apply anticlock_90 t1_vec) in
    let t1_ctrl1= get_adjust htht.t1_ctrl1
      ~f:(fun()-> Ops.(ht1_right
      + Line.extended_vec ~vec:t1_vec (t1_length *. 0.3)
      + Line.extended_vec ~vec:t1_vec_anti90 (width *. 0.6)
      )) in
    let t1_ctrl2= get_adjust htht.t1_ctrl2
      ~f:(fun()-> Ops.(ht1_right
      + Line.extended_vec ~vec:t1_vec (t1_length *. 0.7)
      + Line.extended_vec ~vec:t1_vec_anti90 (width *. 0.9)
      )) in
    let t1_r_ctrl1= t1_ctrl1 in
    let t1_r_ctrl2= t1_ctrl1 in
    let t1_l_ctrl2= Ops.(t1_ctrl1 - Line.extended_vec ~vec:t1_vec_anti90 (width *. 0.8)) in
    let t1_l_ctrl1= Ops.(t1_ctrl2 - Line.extended_vec ~vec:t1_vec_anti90 (width *. 0.8)) in

    let ht1_projection=
      let vec= Ops.(t1_r_ctrl1 - ht1_right) in
      let d= distance vec in
      let r= Float.abs(d /. vec.y) in
      width *. r
    in
    let t1_top_left= { x= h1_1.x -. ht1_projection +. width ; y= h1_1.y +. width *. _h} in
    let t1_top_right= { t1_top_left with x= t1_top_left.x +. ht1_projection } in

    (* insert t *)
    let h1_4= {
      x= min (ht1_start.x +. width*.2.) h1_1.x;
      y= ht1_start.y +. width *. _h;
      } in
    let h1_5= {
      x= ht1_start.x +. width*._sah;
      y= ht1_start.y +. width*._sah;
      } in
    let h1_c5= { h1_4 with
      x= (h1_5.x +. h1_4.x) *. 0.5;
      } in


    let ht2_start= htht.t1_end in
    let ht2_right= { ht2_start with x= ht2_start.x +. htht.h2_length } in
    let ht2_end= htht.end_ in

    let h2_1= { ht2_right with x= ht2_right.x -. width } in
    let h2_2= {
      x= h2_1.x +. width*._a;
      y= h2_1.y -. width*._a; } in
    let h2_3= {
      x= h2_2.x +. width*.0.8;
      y= ht2_start.y +. width*._h*.0.8 } in
    let t2_vec= Ops.(ht2_end - ht2_right) in
    let t2_length= Point.distance t2_vec in
    let t2_vec_clock90= Matrix.(apply clockwise_90 t2_vec) in
    let t2_vec_anti90= Matrix.(apply anticlock_90 t2_vec) in
    let t2_end_post= Ops.(ht2_end + Line.extended_vec ~vec:t2_vec_clock90 (width *.0.3)) in
    let t2_ctrl1= get_adjust htht.t1_ctrl1
      ~f:(fun()-> Ops.(ht2_right
      + Line.extended_vec ~vec:t2_vec (t2_length *. 0.3)
      + Line.extended_vec ~vec:t2_vec_anti90 (width *. 1.2)
      )) in
    let t2_ctrl2= get_adjust htht.t1_ctrl2
      ~f:(fun()-> Ops.(ht2_right
      + Line.extended_vec ~vec:t2_vec (t2_length *. 0.7)
      + Line.extended_vec ~vec:t2_vec_anti90 (width *. 0.9)
      )) in
    let t2_r_ctrl1= t2_ctrl1 in
    let t2_r_ctrl2= t2_ctrl2 in
    let t2_l_ctrl2= Ops.(t2_ctrl1 - Line.extended_vec ~vec:t2_vec_anti90 (width *. 1.0)) in
    let t2_l_ctrl1= Ops.(t2_ctrl2 - Line.extended_vec ~vec:t2_vec_anti90 (width *. 0.7)) in

    let ht2_projection=
      let vec= Ops.(t2_r_ctrl1 - ht2_right) in
      let d= distance vec in
      let r= Float.abs(d /. vec.y) in
      width *. r
    in
    let t2_top_left= { x= h2_1.x -. ht2_projection +. width ; y= h2_1.y +. width *. _h} in
    let t2_top_right= { t2_top_left with x= t2_top_left.x +. ht2_projection } in

    (* insert t *)
    let h2_4= {
      x= min (ht2_start.x +. width*.2.) h2_1.x;
      y= ht2_start.y +. width *. _h;
      } in
    let h2_5= {
      x= ht2_start.x +. width*._sah;
      y= ht2_start.y +. width*._sah;
      } in
    let h2_c5= { h2_4 with
      x= (h2_5.x +. h2_4.x) *. 0.5;
      } in

    let segments= [
      Line h1_1;
      Line h1_2;
      Line h1_3;

      Line t1_top_right;
      Ccurve {ctrl1= t1_r_ctrl1; ctrl2= t1_r_ctrl2; end'= ht1_end_h2};

      (* ht2 begin *)
      Line h2_1;
      Line h2_2;
      Line h2_3;

      Line t2_top_right;
      Ccurve {ctrl1= t2_r_ctrl1; ctrl2= t2_r_ctrl2; end'= ht2_end};
      Line t2_end_post;
      Ccurve {ctrl1= t2_l_ctrl1; ctrl2= t2_l_ctrl2; end'= t2_top_left};

      Line h2_4;
      Qcurve { ctrl= h2_c5; end'= h2_5 };
      Line ht2_start;
      (* ht2 end *)

      Ccurve {ctrl1= t1_l_ctrl1; ctrl2= t1_l_ctrl2; end'= t1_top_left};

      Line h1_4;
      Qcurve { ctrl= h1_c5; end'= h1_5 };
      Line ht1_start
      ] in
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
      c_ctrl1: point adjust;
      c_ctrl2: point adjust;
      c_end: point;
      end_: point adjust;
    } (* Horizontal – Throw – Clockwise curve – J hook *)
  *)

  let from_htcj ?(width=Width.width) htcj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= htcj.htcj_start in

    let ht_start= htcj.htcj_start in
    let ht_right= { ht_start with x= ht_start.x +. htcj.h_length } in
    let ht_end= htcj.t_end in
    let ht_end_cj= {ht_end with x= ht_end.x +. width } in

    let h1= { ht_right with x= ht_right.x -. width } in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h3= {
      x= h2.x +. width*.0.8;
      y= ht_start.y +. width*._h*.0.8 } in
    let t_vec= Ops.(ht_end - ht_right) in
    let t_length= Point.distance t_vec in
    let t_vec_anti90= Matrix.(apply anticlock_90 t_vec) in
    let t_ctrl1= get_adjust htcj.t_ctrl1
      ~f:(fun()-> Ops.(ht_right
      + Line.extended_vec ~vec:t_vec (t_length *. 0.3)
      + Line.extended_vec ~vec:t_vec_anti90 (width *. 1.0)
      )) in
    let t_ctrl2= get_adjust htcj.t_ctrl2
      ~f:(fun()-> Ops.(ht_right
      + Line.extended_vec ~vec:t_vec (t_length *. 0.7)
      + Line.extended_vec ~vec:t_vec_anti90 (width *. 1.0)
      )) in
    let t_r_ctrl1= Ops.(t_ctrl1 + Line.extended_vec ~vec:t_vec_anti90 (width *. 0.0)) in
    let t_r_ctrl2= Ops.(t_ctrl2 + Line.extended_vec ~vec:t_vec_anti90 (width *. 0.0)) in
    let t_l_ctrl2= Ops.(t_ctrl1 - Line.extended_vec ~vec:t_vec_anti90 (width *. 0.8)) in
    let t_l_ctrl1= Ops.(t_ctrl2 - Line.extended_vec ~vec:t_vec_anti90 (width *. 0.8)) in

    let ht_projection=
      let vec= Ops.(t_r_ctrl1 - ht_right) in
      let d= distance vec in
      let r= Float.abs(d /. vec.y) in
      width *. r
    in
    let t_top_left= { x= h1.x -. ht_projection +. width ; y= h1.y +. width *. _h} in
    let t_top_right= { t_top_left with x= t_top_left.x +. ht_projection } in

    (* insert tcj *)
    let h4= {
      x= min (ht_start.x +. width*.2.) h1.x;
      y= ht_start.y +. width *. _h;
      } in
    let h5= {
      x= ht_start.x +. width*._sah;
      y= ht_start.y +. width*._sah;
      } in
    let h5_c= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let c_end= htcj.c_end in
    let c_vec= Ops.(c_end - ht_end_cj) in
    let c_vec_anti90= Matrix.(apply anticlock_90 c_vec) in
    let c_length= distance c_vec in
    let c_ctrl1= get_adjust htcj.c_ctrl1
      ~f:(fun()-> Ops.(ht_end_cj
      + Line.extended_vec ~vec:c_vec (c_length *. 0.3)
      + Line.extended_vec ~vec:c_vec_anti90 (width*.1.0)
      )) in
    let c_ctrl2= get_adjust htcj.c_ctrl2
      ~f:(fun()-> Ops.(ht_end_cj
      + Line.extended_vec ~vec:c_vec (c_length *. 0.7)
      + Line.extended_vec ~vec:c_vec_anti90 (width*.1.0)
      )) in
    let c_r_ctrl1= Ops.(c_ctrl1 + Line.extended_vec ~vec:c_vec_anti90 (width *. 1.2)) in
    let c_r_ctrl2= Ops.(c_ctrl2 + Line.extended_vec ~vec:c_vec_anti90 (width *. 1.0)) in
    let c_l_ctrl2= Ops.(c_ctrl1 + Line.extended_vec ~vec:c_vec_anti90 (width *. 0.2)) in
    let c_l_ctrl1= Ops.(c_ctrl2 + Line.extended_vec ~vec:c_vec_anti90 (width *. 0.2)) in

    let j_start= c_end in
    let j_end= get_adjust htcj.end_ ~f:(fun()->
      {x= j_start.x -. width *. 2.5; y= j_start.y -. width *. 1.5}
    ) in
    let j_end_c= {x= j_start.x; y= j_start.y-. width*.0.8} in
    let vec_j= Ops.(j_end - j_start) in
    let j_length= distance vec_j in
    let j_pre_end= Ops.(j_end +
      Line.extended_vec ~vec:(Matrix.(apply anticlock_90 vec_j)) (width*.0.2)
    ) in
    let vec_j_up= let open Ops in
      let p_toward= { x= t_top_right.x; y= c_end.y -. width *. 1. } in
      p_toward - j_end in
    let j_t_left=
      let right= Bezier.lerp3 ht_end_cj c_ctrl1 c_ctrl2 c_end 0.8 in
      let before= Bezier.lerp3 ht_end_cj c_ctrl1 c_ctrl2 c_end 0.7999 in
      let after= Bezier.lerp3 ht_end_cj c_ctrl1 c_ctrl2 c_end 0.8001 in
      let vec_t_0d8= Ops.(after - before) in
      Ops.(right +
        Line.extended_vec ~vec:Matrix.(apply clockwise_90 vec_t_0d8) (width*.0.8))
    in
    let pre_j_v_left= let open Ops in
      j_end + Line.extended_vec ~vec:vec_j_up (j_length -. width *. 1.5)
    in
    let c_j_t_left=
      let line1= Line.of_points c_l_ctrl1 j_t_left
      and line2= Line.of_points j_end pre_j_v_left in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point)
    in

    let segments= [
      Line h1;
      Line h2;
      Line h3;

      Line t_top_right;
      Ccurve {ctrl1= t_r_ctrl1; ctrl2= t_r_ctrl2; end'= ht_end_cj};

      (* c begin *)
      Ccurve {ctrl1= c_r_ctrl1; ctrl2= c_r_ctrl2; end'= c_end};

      (* j begin *)
      Qcurve {ctrl= j_end_c; end'= j_pre_end};
      Line j_end;
      Line pre_j_v_left;
      Qcurve {ctrl= c_j_t_left; end'=j_t_left};
      (* j end *)

      Ccurve {ctrl1= c_l_ctrl1; ctrl2= c_l_ctrl2; end'= ht_end};
      (* c end *)

      Ccurve {ctrl1= t_l_ctrl1; ctrl2= t_l_ctrl2; end'= t_top_left};

      Line h4;
      Qcurve { ctrl= h5_c; end'= h5 };
      Line ht_start
      ] in
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

  let from_hvhv ?(width=Width.width) hvhv=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= hvhv.hvhv_start in
    let h1_length= hvhv.h1_length
    and h2_length= hvhv.h2_length
    and v1_length= hvhv.v1_length
    and v2_length= hvhv.v2_length in
    let h1_right= { start with x= start.x +. h1_length } in
    let v1_end= { h1_right with y= h1_right.y +. v1_length } in
    let h2_end= { v1_end with x= v1_end.x +. h2_length} in
    let h1_0= start in
    let h1_1= {h1_right with x= h1_right.x -. width} in
    let h1_2= {
      x= h1_1.x +. width*._a;
      y= h1_1.y -. width*._a; } in
    let h1_3= {
      x= h1_2.x +. width*.0.8;
      y= start.y +. width*._h*.0.8 } in
    (* insert v *)
    let h1_4= {
      x= min (h1_0.x +. width*.2.) h1_1.x;
      y= h1_0.y +. width *. _h;
      } in
    let h1_5= {
      x= h1_0.x +. width*._sah;
      y= h1_0.y +. width*._sah;
      } in
    let h1_c5= { h1_4 with
      x= (h1_5.x +. h1_4.x) *. 0.5;
      } in

    let h2_right= h2_end in
    let h2_1= {h2_right with x= h2_right.x -. width} in
    let h2_2= {
      x= h2_1.x +. width*._a;
      y= h2_1.y -. width*._a; } in
    let h2_3= {
      x= h2_2.x +. width*.0.8;
      y= v1_end.y +. width*._h*.0.8 } in
    (* insert v2 *)
    let h2_4= {
      x= v1_end.x;
      y= v1_end.y +. width*._h } in
    let h2_5= {
      x= h2_4.x -. width*._a;
      y= h2_4.y +. width*._h } in
    let h2_6= {
      x= h2_5.x -. width;
      y= h2_5.y -. width*._h*.2. } in
    let h2_7= {
      x= v1_end.x -. width;
      y= v1_end.y -. width*._h } in

    let v2_end= { h2_right with y= h2_right.y +. v2_length } in
    let v2_end_pre= {v2_end with y= v2_end.y -. width*.0.2} in
    let v2_end_post= {v2_end with x= v2_end.x -. width} in

    let v1_top_left= { x= h1_1.x ; y= h1_1.y +. width *. _h} in
    let v1_top_right= { v1_top_left with x= v1_top_left.x +. width*.1. } in

    let v2_top_left= { x= h2_1.x ; y= h2_1.y +. width *. _h} in
    let v2_top_right= { v2_top_left with x= v2_top_left.x +. width*.1. } in

    let segments= [
      Line h1_1;
      Line h1_2;
      Line h1_3;
      Line v1_top_right;

      Line v1_end;
      Line h2_1;
      Line h2_2;
      Line h2_3;

      Line v2_top_right;
      Line v2_end_pre;
      Line v2_end_post;
      Line v2_top_left;

      Line h2_4;
      Line h2_5;
      Line h2_6;
      Line h2_7;

      Line v1_top_left;
      Line h1_4;
      Qcurve { ctrl= h1_c5; end'= h1_5 };
      Line h1_0
      ] in
    {
      start;
      segments;
    }

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

  let from_hthtj ?(width=Width.width) (hthtj:hthtj)=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= hthtj.hthtj_start in

    let ht1_start= start in
    let ht1_right= { ht1_start with x= ht1_start.x +. hthtj.h1_length } in
    let ht1_end= hthtj.t1_end in
    let ht1_end_h2= {ht1_end with x= ht1_end.x +. width } in

    let h1_1= { ht1_right with x= ht1_right.x -. width } in
    let h1_2= {
      x= h1_1.x +. width*._a;
      y= h1_1.y -. width*._a; } in
    let h1_3= {
      x= h1_2.x +. width*.0.8;
      y= ht1_start.y +. width*._h*.0.8 } in
    let t1_vec= Ops.(ht1_end - ht1_right) in
    let t1_length= Point.distance t1_vec in
    let t1_vec_anti90= Matrix.(apply anticlock_90 t1_vec) in
    let t1_ctrl1= get_adjust hthtj.t1_ctrl1
      ~f:(fun()-> Ops.(ht1_right
      + Line.extended_vec ~vec:t1_vec (t1_length *. 0.3)
      + Line.extended_vec ~vec:t1_vec_anti90 (width *. 0.6)
      )) in
    let t1_ctrl2= get_adjust hthtj.t1_ctrl2
      ~f:(fun()-> Ops.(ht1_right
      + Line.extended_vec ~vec:t1_vec (t1_length *. 0.7)
      + Line.extended_vec ~vec:t1_vec_anti90 (width *. 0.9)
      )) in
    let t1_r_ctrl1= t1_ctrl1 in
    let t1_r_ctrl2= t1_ctrl1 in
    let t1_l_ctrl2= Ops.(t1_ctrl1 - Line.extended_vec ~vec:t1_vec_anti90 (width *. 0.8)) in
    let t1_l_ctrl1= Ops.(t1_ctrl2 - Line.extended_vec ~vec:t1_vec_anti90 (width *. 0.8)) in

    let ht1_projection=
      let vec= Ops.(t1_r_ctrl1 - ht1_right) in
      let d= distance vec in
      let r= Float.abs(d /. vec.y) in
      width *. r
    in
    let t1_top_left= { x= h1_1.x -. ht1_projection +. width ; y= h1_1.y +. width *. _h} in
    let t1_top_right= { t1_top_left with x= t1_top_left.x +. ht1_projection } in

    (* insert t *)
    let h1_4= {
      x= min (ht1_start.x +. width*.2.) h1_1.x;
      y= ht1_start.y +. width *. _h;
      } in
    let h1_5= {
      x= ht1_start.x +. width*._sah;
      y= ht1_start.y +. width*._sah;
      } in
    let h1_c5= { h1_4 with
      x= (h1_5.x +. h1_4.x) *. 0.5;
      } in


    let ht2_start= hthtj.t1_end in
    let ht2_right= { ht2_start with x= ht2_start.x +. hthtj.h2_length } in
    let ht2_end= hthtj.t2_end in

    let h2_1= { ht2_right with x= ht2_right.x -. width } in
    let h2_2= {
      x= h2_1.x +. width*._a;
      y= h2_1.y -. width*._a; } in
    let h2_3= {
      x= h2_2.x +. width*.0.8;
      y= ht2_start.y +. width*._h*.0.8 } in
    let t2_vec= Ops.(ht2_end - ht2_right) in
    let t2_length= Point.distance t2_vec in
    let t2_vec_anti90= Matrix.(apply anticlock_90 t2_vec) in
    let t2_end= ht2_end in
    let t2_ctrl1= get_adjust hthtj.t2_ctrl1
      ~f:(fun()-> Ops.(ht2_right
      + Line.extended_vec ~vec:t2_vec (t2_length *. 0.3)
      + Line.extended_vec ~vec:t2_vec_anti90 (width *. 0.9)
      )) in
    let t2_ctrl2= get_adjust hthtj.t2_ctrl2
      ~f:(fun()-> Ops.(ht2_right
      + Line.extended_vec ~vec:t2_vec (t2_length *. 0.7)
      + Line.extended_vec ~vec:t2_vec_anti90 (width *. 1.0)
      )) in
    let t2_r_ctrl1= t2_ctrl1 in
    let t2_r_ctrl2= t2_ctrl2 in
    let t2_l_ctrl2= Ops.(t2_ctrl1 - Line.extended_vec ~vec:t2_vec_anti90 (width *. 1.0)) in

    let ht2_projection=
      let vec= Ops.(t2_r_ctrl1 - ht2_right) in
      let d= distance vec in
      let r= Float.abs(d /. vec.y) in
      width *. r
    in
    let t2_top_left= { x= h2_1.x -. ht2_projection +. width ; y= h2_1.y +. width *. _h} in
    let t2_top_right= { t2_top_left with x= t2_top_left.x +. ht2_projection } in

    (* insert t *)
    let h2_4= {
      x= min (ht2_start.x +. width*.2.) h2_1.x;
      y= ht2_start.y +. width *. _h;
      } in
    let h2_5= {
      x= ht2_start.x +. width*._sah;
      y= ht2_start.y +. width*._sah;
      } in
    let h2_c5= { h2_4 with
      x= (h2_5.x +. h2_4.x) *. 0.5;
      } in

    let j_start= ht2_end in
    let j_end= get_adjust hthtj.end_ ~f:(fun()->
      {x= j_start.x -. width *. 2.5; y= j_start.y -. width *. 1.5}
    ) in
    let c_j_end= {x= j_start.x; y= j_start.y-. width*.0.8} in
    let vec_j= Ops.(j_end - j_start) in
    let j_length= distance vec_j in
    let pre_end= Ops.(j_end +
      Line.extended_vec ~vec:(Matrix.(apply anticlock_90 vec_j)) (width*.0.2)
    ) in
    let vec_j_up= let open Ops in
      let p_toward= { x= t2_top_right.x; y= ht2_end.y -. width *. 1. } in
      p_toward - j_end in
    let j_t_left=
      let length= distance ~from: t2_top_right t2_end in
      let pos= (length -. width*.2.5) /. length in
      let right= Bezier.lerp3 t2_top_right t2_r_ctrl1 t2_r_ctrl2 ht2_end pos in
      let before= Bezier.lerp3 t2_top_right t2_r_ctrl1 t2_r_ctrl2 ht2_end (pos -. 0.0001) in
      let after= Bezier.lerp3 t2_top_right t2_r_ctrl1 t2_r_ctrl2 ht2_end (pos +. 0.0001) in
      let vec_t_0d8= Ops.(after - before) in
      Ops.(right +
        Line.extended_vec ~vec:Matrix.(apply clockwise_90 vec_t_0d8) (width*.1.0))
    in
    let pre_j_v_left= let open Ops in
      j_end + Line.extended_vec ~vec:vec_j_up (j_length *. 0.6)
    in
    let c_j_t_left=
      let line1= Line.of_points t2_l_ctrl2 j_t_left
      and line2= Line.of_points j_end pre_j_v_left in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point)
    in

    let segments= [
      Line h1_1;
      Line h1_2;
      Line h1_3;

      Line t1_top_right;
      Ccurve {ctrl1= t1_r_ctrl1; ctrl2= t1_r_ctrl2; end'= ht1_end_h2};

      (* ht2 begin *)
      Line h2_1;
      Line h2_2;
      Line h2_3;

      Line t2_top_right;
      Ccurve {ctrl1= t2_r_ctrl1; ctrl2= t2_r_ctrl2; end'= ht2_end};

      (* j begin *)
      Qcurve {ctrl= c_j_end; end'=pre_end};
      Line j_end;
      Line pre_j_v_left;
      Qcurve {ctrl= c_j_t_left; end'=j_t_left};
      (* j end *)

      Qcurve {ctrl= t2_l_ctrl2; end'= t2_top_left};

      Line h2_4;
      Qcurve { ctrl= h2_c5; end'= h2_5 };
      Line ht2_start;
      (* ht2 end *)

      Ccurve {ctrl1= t1_l_ctrl1; ctrl2= t1_l_ctrl2; end'= t1_top_left};

      Line h1_4;
      Qcurve { ctrl= h1_c5; end'= h1_5 };
      Line ht1_start
      ] in
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

  let from_vu ?(width=Width.width) vu=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= vu.vu_start in
    let v_end= { start with y= start.y +. vu.length } in
    let u_end= get_adjust vu.end_ ~f:(fun ()->
      let d= Float.pow (vu.length /. 3.) 2. /. 2. |> sqrt in
      PointF.{x= v_end.x +. d; y= v_end.y -. d}
      ) in
    let down= {v_end with x= v_end.x+.width} in
    let end'= u_end in
    let v_top_left= start in
    let v_top_right= {x= start.x+.width; y= start.y +. width*.0.5} in
    (* insert v *)
    let h4= {
      x= min (start.x +. width*.2.) v_top_right.x;
      y= start.y +. width *. _h;
      } in
    let h5= {
      x= start.x +. width*._sah;
      y= start.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let vec_u= Ops.(end' - down) in
    let vec_u_clock90= Matrix.(apply clockwise_90 vec_u) in
    let u_start=
      let r_u= radian vec_u in
      let r_u= if r_u > pi then pi *. 2. -. r_u  else r_u in
      let adjust= Float.pow (r_u /. (pi/.2.)*.1.2) 2. in
      { down with y= down.y -. width *. (1.2+.adjust) } in
    let post_end= Ops.(end' + Line.extended_vec ~vec:vec_u_clock90 (width*.0.2)) in
    let c_u_down=
      let p= Ops.(down *< 0.7 + end' *< 0.3) in
      Ops.(p - Line.extended_vec ~vec:vec_u_clock90 (width*.0.1)) in
    let u_left= {
      x= down.x -. width *. 1.2;
      y= down.y -. width *. 1.4;
    } in
    let u_v= {
      x= down.x -. width;
      y= u_left.y -. width;
    } in
    let c_u_left= {
      x= down.x -. width;
      y= (u_left.y +. u_v.y)/.2.;
    } in

    let segments= [
      Line v_top_right;
      Line u_start;
      Line end';
      Line post_end;
      Qcurve { ctrl= c_u_down; end'= down };
      Line u_left;
      Qcurve { ctrl= c_u_left; end'= u_v };

      Line v_top_left;

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line start
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

  let from_vh ?(width=Width.width) vh=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= vh.vh_start in
    let h_length= vh.h_length
    and v_length= vh.v_length in
    let down= {x= start.x+.width; y= start.y+.v_length} in
    let end'= {down with x= down.x +.h_length} in

    let h2_right= end' in
    let h2_1= {h2_right with x= h2_right.x -. width} in
    let h2_2= {
      x= h2_1.x +. width*._a;
      y= h2_1.y -. width*._a; } in
    let h2_3= {
      x= h2_2.x +. width*.1.;
      y= down.y +. width*._h*.1. } in
    let h2_4= {
      x= down.x;
      y= down.y +. width*._h } in
    let h2_5= {
      x= h2_4.x -. width*._a;
      y= h2_4.y +. width*._h } in
    let h2_6= {
      x= h2_5.x -. width;
      y= h2_5.y -. width*._h*.2. } in
    let h2_7= {
      x= down.x -. width;
      y= down.y -. width*._h } in

    let v_top_left= start in
    let v_top_right= { x= v_top_left.x +. width*.1.; y= v_top_left.y +. width } in
    let segments= [
      Line v_top_right;

      Line down;
      Line h2_1;
      Line h2_2;
      Line h2_3;
      Line h2_4;
      Line h2_5;
      Line h2_6;
      Line h2_7;

      Line v_top_left;
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

  let from_va ?(width=Width.width) va=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= va.va_start in
    let v_length= va.v_length in
    let right= { start with x= start.x +. width } in
    let down= { right with y= right.y +. v_length } in
    let end'= { down with x= down.x +. va.h_length } in
    let radius= get_adjust va.a_radius ~f:(fun()-> width) in

    let v_top_left= start in
    let v_top_right= { v_top_left with x= v_top_left.x +. width } in

    let v1= {x= v_top_right.x +. width*.0.3; y= v_top_right.y +. width*.0.4} in
    let v2= {x= v_top_right.x; y= v1.y +. width*.0.4} in

    let a_start_right=
      { down with y= down.y -. width*.0.8 -. radius *. 0.9 } in
    let a_start_left=
      { x= down.x -. width; y= down.y -. radius *. 1.2 } in
    let h2_up_left=
      { x= right.x +. radius *. 0.8; y= down.y -. width *.0.9 } in
    let h2_up_right=
      { x= end'.x -. width; y= end'.y -. width } in
    let h2_up_c=
      let middle= Ops.((h2_up_left + h2_up_right) /< 2.) in
      { middle with y= middle.y +. width *. 0.2 } in
    let h2_down_left=
      { x= h2_up_left.x -. width*.0.6; y = down.y } in
    let h2_down_right=
      { h2_up_right with y = h2_up_right.y +. width } in
    let h2_down_c=
      let middle= Ops.((h2_down_left + h2_down_right) /< 2.) in
      { middle with y= middle.y +. width *. 0.2 } in
    let v_h2_up_c=
      let line1= Line.of_points v_top_right a_start_right
      and line2= Line.of_points h2_up_left h2_up_c in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
    let v_h2_down_c=
      let line1= Line.of_points v_top_left a_start_left
      and line2= Line.of_points h2_down_left h2_down_c in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
    let end_up_c= Ops.(h2_up_right +
      Line.extended_vec ~vec:(h2_up_right - h2_up_c) (width *. 0.4)
      )
    and end_down_c= Ops.(h2_down_right +
      Line.extended_vec ~vec:(h2_down_right - h2_down_c) (width *. 1.2)
      ) in
    let segments= [
      Line v1;
      Line v2;
      Line a_start_right;
      Qcurve { ctrl= v_h2_up_c; end'= h2_up_left };
      Qcurve { ctrl= h2_up_c; end'= h2_up_right };

      Ccurve { ctrl1= end_up_c; ctrl2= end_down_c; end'= h2_down_right; };

      Qcurve { ctrl= h2_down_c; end'= h2_down_left };
      Qcurve { ctrl= v_h2_down_c; end'= a_start_left };

      Line v_top_left;
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

  let from_vaj ?(width=Width.width) vaj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= vaj.vaj_start in
    let v_length= vaj.v_length in
    let right= { start with x= start.x +. width } in
    let down= { right with y= right.y +. v_length } in
    let h_end= { down with x= down.x +. vaj.h_length } in
    let j_start= {x= h_end.x -. width; y= h_end.y -. width} in
    let j_right= {x= h_end.x; y= j_start.y -. width *. 0.1 } in
    let end'= get_adjust vaj.end_ ~f:(fun()->
      { x= h_end.x -. width; y= h_end.y -. width *. 3.5 }) in
    let vec_j= Ops.(end' - j_start) in
    let vec_j_clock90= Matrix.(apply clockwise_90 vec_j) in
    let j_length= distance vec_j in
    let end_post= Ops.(end' +
      Line.extended_vec ~vec:vec_j_clock90 (width*.0.2)) in
    let radius= get_adjust vaj.a_radius ~f:(fun()-> width) in
    let v_top_left= start in
    let v_top_right= { v_top_left with x= v_top_left.x +. width } in

    let v1= {x= v_top_right.x +. width*.0.3; y= v_top_right.y +. width*.0.4} in
    let v2= {x= v_top_right.x; y= v1.y +. width*.0.4} in

    let a_start_right=
      { down with y= down.y -. width*.0.8 -. radius *. 0.9 } in
    let a_start_left=
      { x= down.x -. width; y= down.y -. radius *. 1.2 } in
    let h2_up_left=
      { x= right.x +. radius *. 0.8; y= down.y -. width *.0.9 } in
    let h2_up_right=
      { x= j_start.x -. width*.0.8; y= j_start.y } in
    let h2_up_c=
      let middle= Ops.((h2_up_left + h2_up_right) /< 2.) in
      { middle with y= middle.y +. width *. 0.1 } in
    let h2_down_left=
      { x= h2_up_left.x -. width*.0.6; y = down.y } in
    let h2_down_right=
      { h2_up_right with y = h2_up_right.y +. width } in
    let h2_down_c=
      let middle= Ops.((h2_down_left + h2_down_right) /< 2.) in
      { middle with y= middle.y +. width *. 0.2 } in
    let v_h2_up_c=
      let line1= Line.of_points v_top_right a_start_right
      and line2= Line.of_points h2_up_left h2_up_c in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
    let vec_h2_up_right= Ops.(h2_up_right - h2_up_c) in
    let h2_j_c1= Ops.( h2_up_right+
      Line.extended_vec ~vec:vec_h2_up_right (j_length*.0.25)) in
    let h2_j_c2= Ops.(end' -
      Line.extended_vec ~vec:vec_j (j_length*.0.133)) in
    let j_h2_right_c= {
      x= end_post.x*.0.95+.j_right.x*.0.05;
      y= j_right.y+.width*.0.1} in
    let v_h2_down_c=
      let line1= Line.of_points v_top_left a_start_left
      and line2= Line.of_points h2_down_left h2_down_c in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
    let end_down_c= Ops.(h2_down_right +
      Line.extended_vec ~vec:(h2_down_right - h2_down_c) (width *. 1.6)
      ) in
    let segments= [
      Line v1;
      Line v2;
      Line a_start_right;
      Qcurve { ctrl= v_h2_up_c; end'= h2_up_left };
      Qcurve { ctrl= h2_up_c; end'= h2_up_right };

      Ccurve { ctrl1= h2_j_c1; ctrl2= h2_j_c2; end'; };
      Line end_post;
      Qcurve { ctrl= j_h2_right_c; end'= j_right };

      Qcurve { ctrl= end_down_c; end'= h2_down_right; };

      Qcurve { ctrl= h2_down_c; end'= h2_down_left };
      Qcurve { ctrl= v_h2_down_c; end'= a_start_left };

      Line v_top_left;
      ] in
    {
      start;
      segments;
    }

  (*
    type vhv= {
      vhv_start: point;
      v1_length: float;
      h_length: float;
      v2_length: float;
    } (* Vertical – Horizontal – Vertical *)
  *)

  let from_vhv ?(width=Width.width) vhv=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= vhv.vhv_start in
    let v1_length= vhv.v1_length
    and h_length= vhv.h_length
    and v2_length= vhv.v2_length in
    let v1_top_right= { start with x= start.x +. width } in
    let v1_end= { v1_top_right with y= v1_top_right.y +. v1_length } in
    let h2_end= { v1_end with x= v1_end.x +. h_length} in
    let h_right= h2_end in
    let h1= {h_right with x= h_right.x -. width} in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h3= {
      x= h2.x +. width*.0.8;
      y= v1_end.y +. width*._h*.0.8 } in
    (* insert v2 *)
    let h4= {
      x= v1_end.x;
      y= v1_end.y +. width*._h } in
    let h5= {
      x= h4.x -. width*._a;
      y= h4.y +. width*._h } in
    let h6= {
      x= h5.x -. width;
      y= h5.y -. width*._h*.2. } in
    let h7= {
      x= v1_end.x -. width;
      y= v1_end.y -. width*._h } in

    let v2_end= { h_right with y= h_right.y +. v2_length } in
    let v2_end_pre= {v2_end with y= v2_end.y -. width*.0.2} in
    let v2_end_post= {v2_end with x= v2_end.x -. width} in

    let v1_top_left= start in
    let v1_top_right= { v1_top_left with x= v1_top_left.x +. width*.1. } in

    let v1= {x= v1_top_right.x +. width*.0.3; y= v1_top_right.y +. width*.0.4} in
    let v2= {x= v1_top_right.x; y= v1.y +. width*.0.4} in

    let v2_top_left= { x= h1.x ; y= h1.y +. width *. _h} in
    let v2_top_right= { v2_top_left with x= v2_top_left.x +. width*.1. } in

    let segments= [
      Line v1;
      Line v2;

      Line v1_end;
      Line h1;
      Line h2;
      Line h3;

      Line v2_top_right;
      Line v2_end_pre;
      Line v2_end_post;
      Line v2_top_left;

      Line h4;
      Line h5;
      Line h6;
      Line h7;

      Line v1_top_left;
      ] in
    {
      start;
      segments;
    }

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

  let from_vht ?(width=Width.width) vht=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= vht.vht_start in
    let v_length= vht.v_length
    and h_length= vht.h_length in
    let h_start= { start with y= start.y +.v_length } in
    let h_right= { h_start with x= h_start.x +. h_length } in
    let end'= vht.end_ in

    let v_top_left= start in
    let v_top_right= { v_top_left with x= v_top_left.x +. width } in

    let v1= {x= v_top_right.x +. width*.0.3; y= v_top_right.y +. width*.0.4} in
    let v2= {x= v_top_right.x; y= v1.y +. width*.0.4} in

    let h0= { v2 with y= h_start.y } in
    let h1= { h_right with x= h_right.x -. width } in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h3= {
      x= h2.x +. width*.0.8;
      y= h_start.y +. width*._h*.0.8 } in
    let vec_t= Ops.(end' - h_right) in
    let t_length= Point.distance vec_t in
    let vec_t_clock90= Matrix.(apply clockwise_90 vec_t) in
    let vec_t_anti90= Matrix.(apply anticlock_90 vec_t) in
    let end_post= Ops.(end' + Line.extended_vec ~vec:vec_t_clock90 (width *.0.3)) in
    let ctrl1= get_adjust vht.ctrl1
      ~f:(fun()-> Ops.(h_right
      + Line.extended_vec ~vec:vec_t (t_length *. 0.3)
      + Line.extended_vec ~vec:vec_t_anti90 (width *. 1.0)
      )) in
    let ctrl2= get_adjust vht.ctrl2
      ~f:(fun()-> Ops.(h_right
      + Line.extended_vec ~vec:vec_t (t_length *. 0.7)
      + Line.extended_vec ~vec:vec_t_anti90 (width *. 0.9)
      )) in
    let r_ctrl1= ctrl1 in
    let r_ctrl2= ctrl2 in
    let l_ctrl2= Ops.(ctrl1 - Line.extended_vec ~vec:vec_t_anti90 (width *. 0.9)) in
    let l_ctrl1= Ops.(ctrl2 - Line.extended_vec ~vec:vec_t_anti90 (width *. 0.7)) in

    let projection=
      let vec= Ops.(r_ctrl1 - h_right) in
      let d= distance vec in
      let r= Float.abs(d /. vec.y) in
      width *. r
    in
    let t_top_left= { x= h1.x -. projection +. width ; y= h1.y +. width *. _h} in
    let t_top_right= { t_top_left with x= t_top_left.x +. projection } in

    (* insert t *)
    let h4= {
      x= min (h_start.x +. width*.2.) h1.x;
      y= h_start.y +. width *. _h;
      } in
    let h5= {
      x= h_start.x +. width*._sah;
      y= h_start.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let segments= [
      Line v1;
      Line v2;

      Line h0;
      Line h1;
      Line h2;
      Line h3;

      Line t_top_right;
      Ccurve {ctrl1= r_ctrl1; ctrl2= r_ctrl2; end'};
      Line end_post;
      Ccurve {ctrl1= l_ctrl1; ctrl2= l_ctrl2; end'= t_top_left};

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h_start;
      Line start;
      ] in
    {
      start;
      segments;
    }

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

  let from_vhtj ?(width=Width.width) vhtj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= vhtj.vhtj_start in
    let h_start= {start with y= start.y +. vhtj.v_length} in

    let v_top_left= start in
    let v_top_right= { v_top_left with x= v_top_left.x +. width } in

    let v1= {x= v_top_right.x +. width*.0.3; y= v_top_right.y +. width*.0.4} in
    let v2= {x= v_top_right.x; y= v1.y +. width*.0.4} in

    let right= { h_start with x= h_start.x +. vhtj.h_length } in
    let t_end= vhtj.t_end in
    let vec_t= Ops.(t_end - right) in
    let vec_t_anti90= Matrix.(apply anticlock_90 vec_t) in
    let vec_t_clock90= Matrix.(apply clockwise_90 vec_t) in
    let t_length= distance vec_t in
    let h_start= h_start in
    let h0= { h_start with x= v2.x } in
    let h1= {right with x= right.x -. width} in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h_right= {
      x= h2.x +. width*.0.8;
      y= h_start.y +. width*._h*.0.8 } in
    (* insert t j *)
    let h4= {
      x= min (h_start.x +. width*.2.) h1.x;
      y= h_start.y +. width *. _h;
      } in
    let h5= {
      x= h_start.x +. width*._sah;
      y= h_start.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in
    let t_top_left= { x= h1.x ; y= h1.y +. width *. _h} in
    let t_top_right= { t_top_left with x= t_top_left.x +. width*.1. } in
    let ctrl1= get_adjust vhtj.ctrl1
      ~f:(fun()-> Ops.(right
      + Line.extended_vec ~vec:vec_t (t_length *. 0.5)
      + Line.extended_vec ~vec:vec_t_anti90 (width *. 1.5)
      )) in
    let ctrl2= get_adjust vhtj.ctrl2
      ~f:(fun()-> Ops.(right
      + Line.extended_vec ~vec:vec_t (t_length *. 0.9)
      + Line.extended_vec ~vec:vec_t_anti90 (width *. 0.8)
      )) in
    let r_ctrl1= ctrl1 in
    let r_ctrl2= ctrl2 in
    let l_t_ctrl= let open Ops in
      r_ctrl1 + Line.extended_vec ~vec:vec_t_clock90 (width*.1.2)
    in
    let j_start= t_end in
    let j_end= get_adjust vhtj.end_ ~f:(fun()->
      (* adjust j angle *)
      let vec= Matrix.(apply (clockwise ~radian:(pi*.0.55)) vec_t) in
      Ops.(j_start + Line.extended_vec ~vec (width *. 2.7))
    ) in
    let j_end_c= {x= j_start.x; y= j_start.y-. width*.0.8} in
    let vec_j= Ops.(j_end - j_start) in
    let j_length= distance vec_j in
    let pre_end= Ops.(j_end +
      Line.extended_vec ~vec:(Matrix.(apply anticlock_90 vec_j)) (width*.0.2)
    ) in
    let vec_j_up= Matrix.(apply (clockwise ~radian:(pi*.0.85)) vec_j) in
    let j_t_left=
      let right= Bezier.lerp3 t_top_right r_ctrl1 r_ctrl2 t_end 0.6 in
      let before= Bezier.lerp3 t_top_right r_ctrl1 r_ctrl2 t_end 0.5999 in
      let after= Bezier.lerp3 t_top_right r_ctrl1 r_ctrl2 t_end 0.6001 in
      let vec_t_0d8= Ops.(after - before) in
      (* adjust j_up_line *)
      Ops.(right +
        Line.extended_vec ~vec:Matrix.(apply clockwise_90 vec_t_0d8) (width*.1.0))
    in
    let pre_j_v_left= let open Ops in
      j_end + Line.extended_vec ~vec:vec_j_up (j_length -. width*.1.0)
    in
    let j_t_left_c=
      let line1= Line.of_points l_t_ctrl j_t_left
      and line2= Line.of_points j_end pre_j_v_left in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point)
    in
    let segments= [
      Line v1;
      Line v2;
      Line h0;
      Line h1;
      Line h2;
      Line h_right;

      Line t_top_right;
      Ccurve {ctrl1=r_ctrl1; ctrl2=r_ctrl2; end'= t_end};
      Qcurve {ctrl= j_end_c; end'=pre_end};
      Line j_end;
      Line pre_j_v_left;
      Qcurve {ctrl= j_t_left_c; end'=j_t_left};
      Qcurve {ctrl= l_t_ctrl; end'= t_top_left};

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h_start
      ] in
    {
      start;
      segments;
    }

  (*
    type vj= {
      vj_start: point;
      length: float;
      end_: point adjust;
    } (* Vertical – J hook *)
  *)

  let from_vj ?(width=Width.width) vj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= vj.vj_start in
    let length= vj.length in
    let v_top_left= start in
    let v_top_right= { v_top_left with x= v_top_left.x +. width } in

    let v1= {x= v_top_right.x +. width*.0.3; y= v_top_right.y +. width*.0.4} in
    let v2= {x= v_top_right.x; y= v1.y +. width*.0.4} in

    let v_top_right= { start with x= start.x +. width*.1. } in
    let v_end= { v_top_right with y= start.y +. length } in
    let j_start= { v_end with y= v_end.y -. width *. 2. } in
    let j_down= {x= j_start.x -. width *. 1.5; y= v_end.y} in
    let c_j_start_down= {x=j_start.x; y= j_down.y} in
    let j_end= get_adjust vj.end_ ~f:(fun()->
      {x= j_start.x -. width *. 4.; y= j_down.y -. width *. 1.5}
    ) in
    let c_j_end= {x= j_down.x; y= j_down.y-. width*.0.8} in
    let vec_j= Ops.(j_end - j_start) in
    let pre_end= Ops.(j_end +
      Line.extended_vec ~vec:(Matrix.(apply anticlock_90 vec_j)) (width*.0.2)
    ) in
    let vec_j_up= let open Ops in
      let p_toward= { x= j_start.x; y= v_end.y -. width *. 1. } in
      p_toward - j_end in
    let j_v_left= {x= j_start.x -. width; y= j_start.y} in
    let pre_j_v_left= let open Ops in
      let line1= Line.of_points j_end (j_end + Line.extended_vec ~vec:vec_j_up 1.)
      and line2= Line.of_points start j_v_left in
      let x= Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
      x - Line.extended_vec ~vec:vec_j_up (width*.0.5)
    in
    let c_j_v_left=
      let line1= Line.of_points start j_v_left
      and line2= Line.of_points j_end pre_j_v_left in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point)
    in
    let segments= [
      Line v1;
      Line v2;
      Line j_start;
      Qcurve {ctrl= c_j_start_down; end'= j_down};
      Qcurve {ctrl= c_j_end; end'=pre_end};
      Line j_end;
      Line pre_j_v_left;
      Qcurve {ctrl= c_j_v_left; end'=j_v_left};
      Line start;
      ] in
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

  let from_vc ?(width=Width.width) vc=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= vc.vc_start in
    let v_length= vc.v_length in
    let left= { start with x= start.x -. width } in
    let down= { left with y= left.y +. v_length } in
    let end'= { down with x= down.x -. vc.h_length } in
    let radius= get_adjust vc.a_radius ~f:(fun()-> width) in

    let v1= {x= start.x +. width*.0.3; y= left.y +. width*.0.4} in
    let v2= {x= start.x; y= v1.y +. width*.0.4} in

    let a_start_left=
      { down with y= down.y -. width*.0.8 -. radius *. 0.9 } in
    let a_start_right=
      { x= down.x +. width; y= down.y -. radius *. 1.2 } in
    let h2_up_right=
      { x= left.x -. radius *. 0.8; y= down.y -. width *.0.9 } in
    let h2_up_left=
      { x= end'.x +. width; y= end'.y -. width } in
    let h2_up_c=
      let middle= Ops.((h2_up_right + h2_up_left) /< 2.) in
      { middle with y= middle.y +. width *. 0.2 } in
    let h2_down_left=
      { x= h2_up_right.x +. width*.0.6; y = down.y } in
    let h2_down_right=
      { h2_up_left with y = h2_up_left.y +. width } in
    let h2_down_c=
      let middle= Ops.((h2_down_left + h2_down_right) /< 2.) in
      { middle with y= middle.y +. width *. 0.2 } in
    let v_h2_up_c=
      let line1= Line.of_points left a_start_left
      and line2= Line.of_points h2_up_right h2_up_c in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
    let v_h2_down_c=
      let line1= Line.of_points start a_start_right
      and line2= Line.of_points h2_down_left h2_down_c in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
    let end_up_c= Ops.(h2_up_left +
      Line.extended_vec ~vec:(h2_up_left - h2_up_c) (width *. 0.4)
      )
    and end_down_c= Ops.(h2_down_right +
      Line.extended_vec ~vec:(h2_down_right - h2_down_c) (width *. 1.2)
      ) in
    let segments= [
      Line a_start_left;
      Qcurve { ctrl= v_h2_up_c; end'= h2_up_right };
      Qcurve { ctrl= h2_up_c; end'= h2_up_left };

      Ccurve { ctrl1= end_up_c; ctrl2= end_down_c; end'= h2_down_right; };

      Qcurve { ctrl= h2_down_c; end'= h2_down_left };
      Qcurve { ctrl= v_h2_down_c; end'= a_start_right };

      Line v2;
      Line v1;
      Line left;
      ] in
    {
      start=left;
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

  let from_vcj ?(width=Width.width) vcj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= vcj.vcj_start in
    let v_length= vcj.v_length in
    let left= { start with x= start.x -. width } in
    let down= { left with y= left.y +. v_length } in
    let h_end= { down with x= down.x -. vcj.h_length } in
    let j_start= {x= h_end.x +. width; y= h_end.y -. width} in
    let j_left= {x= h_end.x; y= j_start.y -. width *. 0.1 } in
    let end'= get_adjust vcj.end_ ~f:(fun()->
      { x= h_end.x +. width; y= h_end.y -. width *. 3.5 }) in
    let vec_j= Ops.(end' - j_start) in
    let vec_j_anti90= Matrix.(apply anticlock_90 vec_j) in
    let j_length= distance vec_j in
    let end_post= Ops.(end' +
      Line.extended_vec ~vec:vec_j_anti90 (width*.0.2)) in
    let radius= get_adjust vcj.a_radius ~f:(fun()-> width) in
    let v_top_right= start in
    let v_top_left= { v_top_right with x= v_top_right.x -. width } in

    let v1= {x= v_top_right.x +. width*.0.3; y= v_top_left.y +. width*.0.4} in
    let v2= {x= v_top_right.x; y= v1.y +. width*.0.4} in

    let a_start_left=
      { down with y= down.y -. width*.0.8 -. radius *. 0.9 } in
    let a_start_right=
      { x= down.x +. width; y= down.y -. radius *. 1.2 } in
    let h2_up_right=
      { x= left.x -. radius *. 0.8; y= down.y -. width *.0.9 } in
    let h2_up_left=
      { x= j_start.x +. width*.0.8; y= j_start.y } in
    let h2_up_c=
      let middle= Ops.((h2_up_right + h2_up_left) /< 2.) in
      { middle with y= middle.y +. width *. 0.1 } in
    let h2_down_right=
      { x= h2_up_right.x +. width*.0.6; y = down.y } in
    let h2_down_left=
      { h2_up_left with y = h2_up_left.y +. width } in
    let h2_down_c=
      let middle= Ops.((h2_down_right + h2_down_left) /< 2.) in
      { middle with y= middle.y +. width *. 0.2 } in
    let v_h2_up_c=
      let line1= Line.of_points v_top_left a_start_left
      and line2= Line.of_points h2_up_right h2_up_c in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
    let vec_h2_up_left= Ops.(h2_up_left - h2_up_c) in
    let h2_j_c1= Ops.( h2_up_left+
      Line.extended_vec ~vec:vec_h2_up_left (j_length*.0.25)) in
    let h2_j_c2= Ops.(end' -
      Line.extended_vec ~vec:vec_j (j_length*.0.133)) in
    let j_h2_left_c= {
      x= end_post.x*.0.95+.j_left.x*.0.05;
      y= j_left.y+.width*.0.1} in
    let v_h2_down_c=
      let line1= Line.of_points v_top_right a_start_right
      and line2= Line.of_points h2_down_right h2_down_c in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
    let end_down_c= Ops.(h2_down_left +
      Line.extended_vec ~vec:(h2_down_left - h2_down_c) (width *. 1.6)
      ) in
    let segments= [
      Line a_start_left;
      Qcurve { ctrl= v_h2_up_c; end'= h2_up_right };
      Qcurve { ctrl= h2_up_c; end'= h2_up_left };

      Ccurve { ctrl1= h2_j_c1; ctrl2= h2_j_c2; end'; };
      Line end_post;
      Qcurve { ctrl= j_h2_left_c; end'= j_left };

      Qcurve { ctrl= end_down_c; end'= h2_down_left; };

      Qcurve { ctrl= h2_down_c; end'= h2_down_right };
      Qcurve { ctrl= v_h2_down_c; end'= a_start_right };

      Line v2;
      Line v1;
      Line v_top_left;
      ] in
    {
      start= left;
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

  let from_tu ?(width=Width.width) tu=
    let open Path in
    let open PointF in
    let start= tu.tu_start
    and t_end= tu.t_end in
    let vec_t= Ops.(t_end - start) in
    let t_length= distance vec_t in
    let angle_t= angle vec_t in
    let u_start= t_end in
    let u_end= get_adjust tu.end_ ~f:(fun()->
      {x= u_start.x +. t_length; y= u_start.y -. width}
      ) in
    let vec_u= Ops.(u_end - u_start) in
    let angle_u= angle vec_u in
    let t_p1_angle= angle_t -. pi/.1.8 in
    let t_p2_angle= t_p1_angle +. pi*.0.65 in
    let t_p1= Ops.(+) start @@
      Line.extended_angle ~angle:t_p1_angle (width*. 1.3)
    in
    let t_p2= Ops.(+) t_p1 @@
      Line.extended_angle ~angle:t_p2_angle (width*. 0.4)
    in
    let t_p3= Ops.(+) t_end @@
      Line.extended_angle ~angle:angle_u (width*. 0.4)
    in
    let t_down=
      let calc_ctrl= template_curve ~start:t_p2 ~end':t_p3 in
      let ctrl1= match tu.ctrl1 with
        | Auto-> calc_ctrl ~ratio:{x= 0.07; y= 0.3}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(angle_t-. (pi*.0.5)) (width*. 0.8)
      and ctrl2= match tu.ctrl2 with
        | Auto-> calc_ctrl ~ratio:{x= 0.07; y= 0.7}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(angle_t-. (pi*.0.5)) (width*. 0.6)
      in
      Ccurve {ctrl1; ctrl2; end'= t_p3 }
    in
    let t_up=
      let calc_ctrl= template_curve ~start ~end':t_end in
      let ctrl1= get_adjust tu.ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.06; y= 0.3})
      and ctrl2= get_adjust tu.ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.08; y= 0.7})
      in
      Ccurve {ctrl1= ctrl2; ctrl2= ctrl1; end'= start}
    in

    let u_p0= u_start in
    let u_p1= u_end in
    let u_slope=
      let l= Ops.(u_p1 - u_p0) in
      l.y /. l.x
    in
    let u_orth= Line.orth u_slope in
    let u_c1=
      let c= Ops.((u_p0 + u_p1) / {x=2.;y=2.}) in
      Ops.(Line.extended_slope ~slope:u_orth (width*.0.3) + c)
    in
    let u_p2= Ops.(Line.extended_slope ~slope:u_orth (width*.0.2) + u_p1) in
    let u_p3=
      let c= Ops.(Line.extended_slope ~slope:u_slope (width*.0.8) + u_p0) in
      Ops.(Line.extended_slope ~slope:u_orth (width*.1.0) + c)
    in
    let u_c3=
      let c= Ops.((u_p2 + u_p3) / {x=2.1;y=2.}) in
      Ops.(Line.extended_slope ~slope:u_orth (width*.0.4) + c)
    in
    let u_p4=
      let c= Ops.(Line.extended_slope ~slope:u_slope (width*.0.4) + u_p0) in
      Ops.(Line.extended_slope ~slope:u_orth (width*.1.3) + c)
    in
    let u_c4= Ops.(Line.extended_slope ~slope:u_slope (width/.4.) + u_p4) in
    let u_c5= Ops.(Line.extended_slope ~slope:u_slope (-.width/.4.) + u_p4) in

    let segments= [
      Line t_p1;
      Line t_p2;
      t_down;

      Qcurve { ctrl= u_c1; end'= u_p1 };
      Line u_p2;
      Qcurve { ctrl= u_c3; end'= u_p3 };
      Qcurve { ctrl= u_c4; end'= u_p4 };
      Qcurve { ctrl= u_c5; end'= u_p0 };

      Line t_end;
      t_up;
      ] in
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

  let from_th ?(width=Width.width) th=
    let _h= 0.5
    and _a= 1. in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa *. _h in
    let open Path in
    let open Point in
    let start= th.th_start
    and t_end= th.t_end in
    let vec_t= Ops.(t_end - start) in
    let t_length= distance vec_t in
    let angle_t= angle vec_t in
    let h_start= t_end in
    let h_length= get_adjust th.length ~f:(fun()-> t_length) in
    let t_p1_angle= angle_t -. pi/.1.8 in
    let t_p2_angle= t_p1_angle +. pi*.0.65 in
    let t_p1= Ops.(+) start @@
      Line.extended_angle ~angle:t_p1_angle (width*. 1.3)
    in
    let t_p2= Ops.(+) t_p1 @@
      Line.extended_angle ~angle:t_p2_angle (width*. 0.4)
    in
    let t_p3= {t_end with x= t_end.x +. width *.0.4} in
    let t_down=
      let calc_ctrl= template_curve ~start:t_p2 ~end':t_p3 in
      let ctrl1= match th.ctrl1 with
        | Auto-> calc_ctrl ~ratio:{x= 0.07; y= 0.3}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(angle_t-. (pi*.0.5)) (width*. 0.8)
      and ctrl2= match th.ctrl2 with
        | Auto-> calc_ctrl ~ratio:{x= 0.07; y= 0.7}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(angle_t-. (pi*.0.5)) (width*. 0.6)
      in
      Ccurve {ctrl1; ctrl2; end'= t_p3 }
    in
    let t_up=
      let calc_ctrl= template_curve ~start ~end':t_end in
      let ctrl1= get_adjust th.ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.06; y= 0.3})
      and ctrl2= get_adjust th.ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.08; y= 0.7})
      in
      Ccurve {ctrl1= ctrl2; ctrl2= ctrl1; end'= start}
    in

    let h0= h_start in
    let h1= { h0 with x= h0.x +. h_length -. width*._a } in
    let h2= {
      x= h1.x +. width*.0.5;
      y= h1.y -. width*.0.5; } in
    let h3= {
      x= h2.x +. width*.1.0;
      y= h_start.y +. width*._h } in
    let h4= { h3 with
      x= min (h0.x +. width*.2.) h1.x;
      } in
    let h5_c= { h4 with
      x= h4.x -. width*._sah;
      } in
    let h5= {
      x= h0.x +. width*._sah;
      y= h0.y +. width*._sah;
      } in

    let segments= [
      Line t_p1;
      Line t_p2;
      t_down;

      Line h1;
      Line h2;
      Line h3;
      Line h4;
      Qcurve { ctrl= h5_c; end'= h5 };
      Line h0;

      Line t_end;
      t_up;
      ] in
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

  let from_td ?(width=Width.width) td=
    let _h= 0.5
    and _a= 1. in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa *. _h in
    let open Path in
    let open Point in
    let start= td.td_start
    and t_end= td.t_end in
    let vec_t= Ops.(t_end - start) in
    let angle_t= angle vec_t in
    let t_p1_angle= angle_t -. pi/.1.8 in
    let t_p2_angle= t_p1_angle +. pi*.0.65 in
    let t_p1= Ops.(+) start @@
      Line.extended_angle ~angle:t_p1_angle (width*. 1.3)
    in
    let t_p2= Ops.(+) t_p1 @@
      Line.extended_angle ~angle:t_p2_angle (width*. 0.4)
    in
    let t_p3= {t_end with x= t_end.x +. width *.0.4} in
    let t_down=
      let calc_ctrl= template_curve ~start:t_p2 ~end':t_p3 in
      let ctrl1= match td.ctrl1 with
        | Auto-> calc_ctrl ~ratio:{x= 0.07; y= 0.3}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(angle_t-. (pi*.0.5)) (width*. 0.8)
      and ctrl2= match td.ctrl2 with
        | Auto-> calc_ctrl ~ratio:{x= 0.07; y= 0.7}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(angle_t-. (pi*.0.5)) (width*. 0.6)
      in
      Ccurve {ctrl1; ctrl2; end'= t_p3 }
    in
    let t_up=
      let calc_ctrl= template_curve ~start ~end':t_end in
      let ctrl1= get_adjust td.ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.06; y= 0.3})
      and ctrl2= get_adjust td.ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.08; y= 0.7})
      in
      Ccurve {ctrl1= ctrl2; ctrl2= ctrl1; end'= start}
    in

    let d_start= t_end in
    let end_= get_adjust td.end_ ~f:(fun()->
      { x= start.x; y= d_start.y+.(t_end.y-.start.y)})
    in
    let vec_d= Ops.(end_ - d_start) in
    let d_length= distance vec_d in
    let vec_d_anti90= Matrix.(apply anticlock_90 vec_d) in
    let vec_d_anti45= Matrix.(apply (anticlock ~radian:(pi /. 4.)) vec_d) in
    let vec_d_anti45_neg= neg vec_d_anti45 in
    let d0= d_start in
    let d1= Ops.(d0 + Line.extended_vec ~vec:vec_d_anti90 (max 1. (width/.8.))) in
    let d_r_ctrl1=
      let p= Ops.(d1 + Line.extended_vec ~vec:vec_d (d_length*.0.8)) in
      Ops.(p + Line.extended_vec ~vec:vec_d_anti90 (width*.2.0)) in
    let d_r_ctrl2=
      Ops.(end_ + Line.extended_vec ~vec:vec_d_anti45 (width*.0.6)) in
    let d_l_ctrl1=
      Ops.(end_ + Line.extended_vec ~vec:vec_d_anti45_neg (width*.0.2)) in
    let d_l_ctrl2=
      let p= Ops.(d0 + Line.extended_vec ~vec:vec_d (d_length*.0.75)) in
      Ops.(p + Line.extended_vec ~vec:vec_d_anti90 (d_length*.0.08)) in

    let segments= [
      Line t_p1;
      Line t_p2;
      t_down;

      Ccurve { ctrl1= d_r_ctrl1; ctrl2= d_r_ctrl2; end'= end_ };
      Ccurve { ctrl1= d_l_ctrl1; ctrl2= d_l_ctrl2; end'= d0 };

      t_up;
      ] in
    {
      start;
      segments;
    }

  (*
    type wtd= {
      wtd_start: point;
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

  let from_wtd ?(width=Width.width) wtd=
    let _h= 0.5
    and _a= 1. in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa *. _h in
    let open Path in
    let open Point in
    let start= wtd.wtd_start
    and t_end= wtd.t_end in
    let vec_t= Ops.(t_end - start) in
    let t_length= distance vec_t in
    let angle_t= angle vec_t in
    let t_p1_angle= angle_t -. pi/.1.8 in
    let t_p2_angle= t_p1_angle +. pi*.0.65 in
    let t_p1= Ops.(+) start @@
      Line.extended_angle ~angle:t_p1_angle (width*. 1.3)
    in
    let t_p2= Ops.(+) t_p1 @@
      Line.extended_angle ~angle:t_p2_angle (width*. 0.4)
    in
    let t_p3= {t_end with x= t_end.x +. width *.0.4} in
    let t_down=
      let calc_ctrl= template_curve ~start:t_p2 ~end':t_p3 in
      let ctrl1= match wtd.ctrl1 with
        | Auto-> calc_ctrl ~ratio:{x= 0.04; y= 0.3}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(angle_t-. (pi*.0.5)) (width*. 0.5)
      and ctrl2= match wtd.ctrl2 with
        | Auto-> calc_ctrl ~ratio:{x= 0.04; y= 0.7}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(angle_t-. (pi*.0.5)) (width*. 0.4)
      in
      Ccurve {ctrl1; ctrl2; end'= t_p3 }
    in
    let t_up=
      let calc_ctrl= template_curve ~start ~end':t_end in
      let ctrl1= get_adjust wtd.ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.03; y= 0.3})
      and ctrl2= get_adjust wtd.ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.04; y= 0.7})
      in
      Ccurve {ctrl1= ctrl2; ctrl2= ctrl1; end'= start}
    in

    let d_start= t_end in
    let end_= get_adjust wtd.end_ ~f:(fun()->
      let adjust= t_length *. 0.6 in
      { x= t_end.x+.adjust; y= t_end.y+.adjust})
    in
    let vec_d= Ops.(end_ - d_start) in
    let d_length= distance vec_d in
    let vec_d_anti90= Matrix.(apply anticlock_90 vec_d) in
    let vec_d_anti45= Matrix.(apply (anticlock ~radian:(pi /. 4.)) vec_d) in
    let vec_d_anti45_neg= neg vec_d_anti45 in
    let d0= d_start in
    let d1= Ops.(d0 + Line.extended_vec ~vec:vec_d_anti90 (max 1. (width/.8.))) in
    let d_r_ctrl1=
      let p= Ops.(d1 + Line.extended_vec ~vec:vec_d (d_length*.0.8)) in
      Ops.(p + Line.extended_vec ~vec:vec_d_anti90 (width*.2.0)) in
    let d_r_ctrl2=
      Ops.(end_ + Line.extended_vec ~vec:vec_d_anti45 (width*.0.6)) in
    let d_l_ctrl1=
      Ops.(end_ + Line.extended_vec ~vec:vec_d_anti45_neg (width*.0.2)) in
    let d_l_ctrl2=
      let p= Ops.(d0 + Line.extended_vec ~vec:vec_d (d_length*.0.75)) in
      Ops.(p + Line.extended_vec ~vec:vec_d_anti90 (d_length*.0.08)) in

    let segments= [
      Line t_p1;
      Line t_p2;
      t_down;

      Ccurve { ctrl1= d_r_ctrl1; ctrl2= d_r_ctrl2; end'= end_ };
      Ccurve { ctrl1= d_l_ctrl1; ctrl2= d_l_ctrl2; end'= d0 };

      t_up;
      ] in
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

  let from_tht ?(width=Width.width) tht=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= tht.tht_start in
    let t1_end= tht.t1_end in
    let h_length= tht.length in
    let h_start= t1_end in
    let h_right= { h_start with x= h_start.x +. h_length } in
    let end'= tht.end_ in

    let vec_t1= Ops.(t1_end - start) in
    let t1_angle= angle vec_t1 in
    let t1_p1_angle= t1_angle -. pi/.1.8 in
    let t1_p2_angle= t1_p1_angle +. pi*.0.65 in
    let t1_p1= Ops.(+) start @@
      Line.extended_angle ~angle:t1_p1_angle (width*. 1.3)
    in
    let t1_p2= Ops.(+) t1_p1 @@
      Line.extended_angle ~angle:t1_p2_angle (width*. 0.4)
    in
    let t1_p3= {t1_end with x= t1_end.x +. width *.0.4} in
    let t1_down=
      let calc_ctrl= template_curve ~start:t1_p2 ~end':t1_p3 in
      let ctrl1= match tht.t1_ctrl1 with
        | Auto-> calc_ctrl ~ratio:{x= 0.07; y= 0.3}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(t1_angle-. (pi*.0.5)) (width*. 0.8)
      and ctrl2= match tht.t1_ctrl2 with
        | Auto-> calc_ctrl ~ratio:{x= 0.07; y= 0.7}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(t1_angle-. (pi*.0.5)) (width*. 0.6)
      in
      Ccurve {ctrl1; ctrl2; end'= t1_p3 }
    in
    let t1_up=
      let calc_ctrl= template_curve ~start ~end':t1_end in
      let ctrl1= get_adjust tht.t1_ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.06; y= 0.3})
      and ctrl2= get_adjust tht.t1_ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.08; y= 0.7})
      in
      Ccurve {ctrl1= ctrl2; ctrl2= ctrl1; end'= start}
    in

    let h0= t1_p3 in
    let h1= { h_right with x= h_right.x -. width } in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h3= {
      x= h2.x +. width*.0.8;
      y= h_start.y +. width*._h*.0.8 } in
    let t2_vec= Ops.(end' - h_right) in
    let t2_length= Point.distance t2_vec in
    let t2_vec_clock90= Matrix.(apply clockwise_90 t2_vec) in
    let t2_vec_anti90= Matrix.(apply anticlock_90 t2_vec) in
    let t2_end_post= Ops.(end' + Line.extended_vec ~vec:t2_vec_clock90 (width *.0.3)) in
    let t2_ctrl1= get_adjust tht.t2_ctrl1
      ~f:(fun()-> Ops.(h_right
      + Line.extended_vec ~vec:t2_vec (t2_length *. 0.3)
      + Line.extended_vec ~vec:t2_vec_anti90 (width *. 1.0)
      )) in
    let t2_ctrl2= get_adjust tht.t2_ctrl2
      ~f:(fun()-> Ops.(h_right
      + Line.extended_vec ~vec:t2_vec (t2_length *. 0.7)
      + Line.extended_vec ~vec:t2_vec_anti90 (width *. 0.9)
      )) in
    let t2_r_ctrl1= t2_ctrl1 in
    let t2_r_ctrl2= t2_ctrl2 in
    let t2_l_ctrl2= Ops.(t2_ctrl1 - Line.extended_vec ~vec:t2_vec_anti90 (width *. 0.9)) in
    let t2_l_ctrl1= Ops.(t2_ctrl2 - Line.extended_vec ~vec:t2_vec_anti90 (width *. 0.7)) in

    let projection=
      let vec= Ops.(t2_r_ctrl1 - h_right) in
      let d= distance vec in
      let r= Float.abs(d /. vec.y) in
      width *. r
    in
    let t_top_left= { x= h1.x -. projection +. width ; y= h1.y +. width *. _h} in
    let t_top_right= { t_top_left with x= t_top_left.x +. projection } in

    (* insert t *)
    let h4= {
      x= min (h_start.x +. width*.2.) h1.x;
      y= h_start.y +. width *. _h;
      } in
    let h5= {
      x= h_start.x +. width*._sah;
      y= h_start.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let segments= [
      Line t1_p1;
      Line t1_p2;
      t1_down;

      Line h0;
      Line h1;
      Line h2;
      Line h3;

      Line t_top_right;
      Ccurve {ctrl1= t2_r_ctrl1; ctrl2= t2_r_ctrl2; end'};
      Line t2_end_post;
      Ccurve {ctrl1= t2_l_ctrl1; ctrl2= t2_l_ctrl2; end'= t_top_left};

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h_start;

      t1_up;
      ] in
    {
      start;
      segments;
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

  let from_thtj ?(width=Width.width) thtj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= thtj.thtj_start in

    let t1_end= thtj.t1_end in

    let vec_t1= Ops.(t1_end - start) in
    let t1_angle= angle vec_t1 in
    let t1_p1_angle= t1_angle -. pi/.1.8 in
    let t1_p2_angle= t1_p1_angle +. pi*.0.65 in
    let t1_p1= Ops.(+) start @@
      Line.extended_angle ~angle:t1_p1_angle (width*. 1.3)
    in
    let t1_p2= Ops.(+) t1_p1 @@
      Line.extended_angle ~angle:t1_p2_angle (width*. 0.4)
    in
    let t1_p3= {t1_end with x= t1_end.x +. width *.0.4} in
    let t1_down=
      let calc_ctrl= template_curve ~start:t1_p2 ~end':t1_p3 in
      let ctrl1= match thtj.t1_ctrl1 with
        | Auto-> calc_ctrl ~ratio:{x= 0.07; y= 0.3}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(t1_angle-. (pi*.0.5)) (width*. 0.8)
      and ctrl2= match thtj.t1_ctrl2 with
        | Auto-> calc_ctrl ~ratio:{x= 0.07; y= 0.7}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(t1_angle-. (pi*.0.5)) (width*. 0.6)
      in
      Ccurve {ctrl1; ctrl2; end'= t1_p3 }
    in
    let t1_up=
      let calc_ctrl= template_curve ~start ~end':t1_end in
      let ctrl1= get_adjust thtj.t1_ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.06; y= 0.3})
      and ctrl2= get_adjust thtj.t1_ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.08; y= 0.7})
      in
      Ccurve {ctrl1= ctrl2; ctrl2= ctrl1; end'= start}
    in
    let h_start= t1_end in
    let right= { h_start with x= h_start.x +. thtj.length } in
    let t2_end= thtj.t2_end in
    let t2_vec= Ops.(t2_end - right) in
    let t2_vec_anti90= Matrix.(apply anticlock_90 t2_vec) in
    let t2_vec_clock90= Matrix.(apply clockwise_90 t2_vec) in
    let t2_length= distance t2_vec in
    let h1= {right with x= right.x -. width} in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h_right= {
      x= h2.x +. width*.0.8;
      y= h_start.y +. width*._h*.0.8 } in
    (* insert t j *)
    let h4= {
      x= min (h_start.x +. width*.2.) h1.x;
      y= h_start.y +. width *. _h;
      } in
    let h5= {
      x= h_start.x +. width*._sah;
      y= h_start.y +. width*._sah;
      } in
    let c5= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let t2_top_left= { x= h1.x ; y= h1.y +. width *. _h} in
    let t2_top_right= { t2_top_left with x= t2_top_left.x +. width*.1. } in
    let t2_ctrl1= get_adjust thtj.t2_ctrl1
      ~f:(fun()-> Ops.(right
      + Line.extended_vec ~vec:t2_vec (t2_length *. 0.5)
      + Line.extended_vec ~vec:t2_vec_anti90 (width *. 1.5)
      )) in
    let t2_ctrl2= get_adjust thtj.t2_ctrl2
      ~f:(fun()-> Ops.(right
      + Line.extended_vec ~vec:t2_vec (t2_length *. 0.9)
      + Line.extended_vec ~vec:t2_vec_anti90 (width *. 0.8)
      )) in
    let t2_r_ctrl1= t2_ctrl1 in
    let t2_r_ctrl2= t2_ctrl2 in
    let t2_l_ctrl= let open Ops in
      t2_r_ctrl1 + Line.extended_vec ~vec:t2_vec_clock90 (width*.1.2)
    in
    let j_start= t2_end in
    let j_end= get_adjust thtj.end_ ~f:(fun()->
      (* adjust j angle *)
      let vec= Matrix.(apply (clockwise ~radian:(pi*.0.55)) t2_vec) in
      Ops.(j_start + Line.extended_vec ~vec (width *. 2.7))
    ) in
    let j2_end_c= {x= j_start.x; y= j_start.y-. width*.0.8} in
    let vec_j= Ops.(j_end - j_start) in
    let j_length= distance vec_j in
    let pre_end= Ops.(j_end +
      Line.extended_vec ~vec:(Matrix.(apply anticlock_90 vec_j)) (width*.0.2)
    ) in
    let vec_j_up= Matrix.(apply (clockwise ~radian:(pi*.0.85)) vec_j) in
    let j_t2_left=
      let right= Bezier.lerp3 t2_top_right t2_r_ctrl1 t2_r_ctrl2 t2_end 0.6 in
      let before= Bezier.lerp3 t2_top_right t2_r_ctrl1 t2_r_ctrl2 t2_end 0.5999 in
      let after= Bezier.lerp3 t2_top_right t2_r_ctrl1 t2_r_ctrl2 t2_end 0.6001 in
      let vec_t_0d8= Ops.(after - before) in
      (* adjust j_up_line *)
      Ops.(right +
        Line.extended_vec ~vec:Matrix.(apply clockwise_90 vec_t_0d8) (width*.1.0))
    in
    let pre_j2_v_left= let open Ops in
      j_end + Line.extended_vec ~vec:vec_j_up (j_length -. width*.1.0)
    in
    let j_t2_left_c=
      let line1= Line.of_points t2_l_ctrl j_t2_left
      and line2= Line.of_points j_end pre_j2_v_left in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point)
    in

    let segments= [
      Line t1_p1;
      Line t1_p2;
      t1_down;

      Line h1;
      Line h2;
      Line h_right;

      Line t2_top_right;
      Ccurve {ctrl1=t2_r_ctrl1; ctrl2=t2_r_ctrl2; end'= t2_end};
      Qcurve {ctrl= j2_end_c; end'=pre_end};
      Line j_end;
      Line pre_j2_v_left;
      Qcurve {ctrl= j_t2_left_c; end'=j_t2_left};
      Qcurve {ctrl= t2_l_ctrl; end'= t2_top_left};

      Line h4;
      Qcurve { ctrl= c5; end'= h5 };
      Line h_start;

      t1_up;
      ] in
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

  let from_tj ?(width=Width.width) tj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= tj.tj_start in
    let right= {start with x= start.x +. width} in
    let t_end= tj.t_end in
    let vec_t= Ops.(t_end - right) in
    let vec_t_anti90= Matrix.(apply anticlock_90 vec_t) in
    let vec_t_clock90= Matrix.(apply clockwise_90 vec_t) in
    let t_length= distance vec_t in
    let t_p1_vec= Matrix.(apply (anticlock ~radian:(pi*.0.45)) vec_t) in
    let t_p2_vec= Matrix.(apply (clockwise ~radian:(pi*.0.65)) t_p1_vec) in
    let t_p1= Ops.(+) start @@
      Line.extended_vec ~vec:t_p1_vec (width*. 1.5)
    in
    let t_p2= Ops.(+) t_p1 @@
      Line.extended_vec ~vec:t_p2_vec (width*. 0.45)
    in

    let ctrl1= get_adjust tj.ctrl1
      ~f:(fun()-> Ops.(right
      + Line.extended_vec ~vec:vec_t (t_length *. 0.5)
      + Line.extended_vec ~vec:vec_t_anti90 (width *. 1.5)
      )) in
    let ctrl2= get_adjust tj.ctrl2
      ~f:(fun()-> Ops.(right
      + Line.extended_vec ~vec:vec_t (t_length *. 0.9)
      + Line.extended_vec ~vec:vec_t_anti90 (width *. 0.8)
      )) in
    let r_ctrl1= ctrl1 in
    let r_ctrl2= ctrl2 in
    let l_t_ctrl= let open Ops in
      r_ctrl1 + Line.extended_vec ~vec:vec_t_clock90 (width*.1.2)
    in
    let j_start= t_end in
    let j_end= get_adjust tj.end_ ~f:(fun()->
      (* adjust j angle *)
      let vec= Matrix.(apply (clockwise ~radian:(pi*.0.6)) vec_t) in
      Ops.(j_start + Line.extended_vec ~vec (width *. 2.7))
    ) in
    let j_end_c= {x= j_start.x; y= j_start.y-. width*.0.8} in
    let vec_j= Ops.(j_end - j_start) in
    let j_length= distance vec_j in
    let pre_end= Ops.(j_end +
      Line.extended_vec ~vec:(Matrix.(apply anticlock_90 vec_j)) (width*.0.2)
    ) in
    let vec_j_up= Matrix.(apply (clockwise ~radian:(pi*.0.825)) vec_j) in
    let j_t_left=
      let right= Bezier.lerp3 right r_ctrl1 r_ctrl2 t_end 0.6 in
      let before= Bezier.lerp3 right r_ctrl1 r_ctrl2 t_end 0.5999 in
      let after= Bezier.lerp3 right r_ctrl1 r_ctrl2 t_end 0.6001 in
      let vec_t_0d8= Ops.(after - before) in
      (* adjust j_up_line *)
      Ops.(right +
        Line.extended_vec ~vec:Matrix.(apply clockwise_90 vec_t_0d8) (width*.1.0))
    in
    let pre_j_v_left= let open Ops in
      j_end + Line.extended_vec ~vec:vec_j_up (j_length -. width*.1.1)
    in
    let j_t_left_c=
      let line1= Line.of_points l_t_ctrl j_t_left
      and line2= Line.of_points j_end pre_j_v_left in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point)
    in
    let segments= [
      Line t_p1;
      Line t_p2;
      Ccurve {ctrl1=r_ctrl1; ctrl2=r_ctrl2; end'= t_end};
      Qcurve {ctrl= j_end_c; end'=pre_end};
      Line j_end;
      Line pre_j_v_left;
      Qcurve {ctrl= j_t_left_c; end'=j_t_left};
      Qcurve {ctrl= l_t_ctrl; end'= start};
      ] in
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

  let from_cj ?(width=Width.width) cj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= cj.cj_start in
    let right= {start with x= start.x +. width} in
    let c_end= cj.c_end in
    let vec_c= Ops.(c_end - right) in
    let vec_c_anti90= Matrix.(apply anticlock_90 vec_c) in
    let vec_c_clock90= Matrix.(apply clockwise_90 vec_c) in
    let c_length= distance vec_c in
    let c_p1_vec= Matrix.(apply (anticlock ~radian:(pi*.0.45)) vec_c) in
    let c_p2_vec= Matrix.(apply (clockwise ~radian:(pi*.0.65)) c_p1_vec) in
    let c_p1= Ops.(+) start @@
      Line.extended_vec ~vec:c_p1_vec (width*. 1.5)
    in
    let c_p2= Ops.(+) c_p1 @@
      Line.extended_vec ~vec:c_p2_vec (width*. 0.45)
    in

    let ctrl1= get_adjust cj.ctrl1
      ~f:(fun()-> Ops.(right
      + Line.extended_vec ~vec:vec_c (c_length *. 0.5)
      + Line.extended_vec ~vec:vec_c_anti90 (width *. 2.)
      )) in
    let ctrl2= get_adjust cj.ctrl2
      ~f:(fun()-> Ops.(right
      + Line.extended_vec ~vec:vec_c (c_length *. 0.9)
      + Line.extended_vec ~vec:Matrix.(apply (anticlock ~radian:(pi*.0.3)) vec_c) (width *. 3.0)
      )) in
    let r_ctrl1= ctrl1 in
    let r_ctrl2= ctrl2 in
    let l_c_ctrl= let open Ops in
      r_ctrl1 + Line.extended_vec ~vec:vec_c_clock90 (width*.1.2)
    in
    let j_start= c_end in
    let j_end= get_adjust cj.end_ ~f:(fun()->
      (* adjust j angle *)
      let vec= Matrix.(apply (clockwise ~radian:(pi*.0.75)) vec_c) in
      Ops.(j_start + Line.extended_vec ~vec (width *. 3.0))
    ) in
    let j_end_c= {x= j_start.x; y= j_start.y-. width*.0.8} in
    let vec_j= Ops.(j_end - j_start) in
    let j_length= distance vec_j in
    let pre_end= Ops.(j_end +
      Line.extended_vec ~vec:(Matrix.(apply anticlock_90 vec_j)) (width*.0.2)
    ) in
    let vec_j_up= Matrix.(apply (clockwise ~radian:(pi*.0.825)) vec_j) in
    let j_c_left=
      let right= Bezier.lerp3 right r_ctrl1 r_ctrl2 c_end 0.6 in
      let before= Bezier.lerp3 right r_ctrl1 r_ctrl2 c_end 0.5999 in
      let after= Bezier.lerp3 right r_ctrl1 r_ctrl2 c_end 0.6001 in
      let vec_t_0d8= Ops.(after - before) in
      (* adjust j_up_line *)
      Ops.(right +
        Line.extended_vec ~vec:Matrix.(apply clockwise_90 vec_t_0d8) (width*.1.0))
    in
    let pre_j_v_left= let open Ops in
      j_end + Line.extended_vec ~vec:vec_j_up (j_length -. width*.1.0)
    in
    let j_c_left_c=
      let line1= Line.of_points l_c_ctrl j_c_left
      and line2= Line.of_points j_end pre_j_v_left in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point)
    in
    let segments= [
      Line c_p1;
      Line c_p2;
      Ccurve {ctrl1=r_ctrl1; ctrl2=r_ctrl2; end'= c_end};
      Qcurve {ctrl= j_end_c; end'=pre_end};
      Line j_end;
      Line pre_j_v_left;
      Qcurve {ctrl= j_c_left_c; end'=j_c_left};
      Qcurve {ctrl= l_c_ctrl; end'= start};
      ] in
    {
      start;
      segments;
    }

  (*
    type fpj= {
      fpj_start: point;
      ctrl1: point adjust;
      ctrl2: point adjust;
      p_end: point;
      end_: point adjust;
    } (* Flat Press – J hook *)
      (* 1. the angle is limited between 5 to 40 degrees *)
  *)

  let from_fpj ?(width=Width.width) fpj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= fpj.fpj_start in
    let p_start= start
    and p_end= fpj.p_end in
    let p_length= Point.distance ~from:p_start p_end in
    let vec_p= Ops.(p_end - p_start) in
    let vec_p_clock90= Matrix.(apply clockwise_90 vec_p) in
    let p_p1_vec= Matrix.(apply (anticlock ~radian:(pi*.0.3)) vec_p) in
    let p_p2_vec= Matrix.(apply (clockwise ~radian:(pi*.0.65)) p_p1_vec) in
    let p_p1= Ops.(+) start @@
      Line.extended_vec ~vec:p_p1_vec (width*. 1.5)
    in
    let p_p2= Ops.(+) p_p1 @@
      Line.extended_vec ~vec:p_p2_vec (width*. 0.45)
    in

    let ctrl1= get_adjust fpj.ctrl1
      ~f:(fun()-> Ops.(p_p2
      + Line.extended_vec ~vec:vec_p (p_length *. 0.4)
      + Line.extended_vec ~vec:vec_p_clock90 (p_length *. 0.18)
      )) in
    let ctrl2= get_adjust fpj.ctrl2
      ~f:(fun()-> Ops.(p_p2
      + Line.extended_vec ~vec:vec_p (p_length *. 0.625)
      + Line.extended_vec ~vec:vec_p_clock90 (p_length *. 0.12)
      )) in
    let _up_ctrl2= ctrl2 in
    let r_ctrl1= ctrl1 in
    let r_ctrl2= ctrl2 in
    let p2= {
      x= p_end.x +. width*.0.5;
      y= p_end.y -. width;
    } in
    let lerp_p_up= Bezier.lerp3 p_p1 r_ctrl1 r_ctrl2 p2 in
    let end'= get_adjust fpj.end_ ~f:(fun()->
      let j_base= lerp_p_up (1. -. width*.(0.8+.0.1) /. p_length) in
      { x= j_base.x;
        y= p2.y -. width *. 2.0;
      }) in
    let pre_end= {end' with x= end'.x -. width*.0.1}
    and post_end= {end' with x= end'.x +. width*.0.1} in
    let j_base_left= lerp_p_up (1. -. width*.(0.8+.0.2) /. p_length) in
    let j_start= lerp_p_up (1. -. width*.(0.8+.0.2+.1.0) /. p_length) in
    let p_j_left_c=
      let line1= Line.of_points r_ctrl2 j_start
      and line2= Line.of_points pre_end j_base_left in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
    let vec_end_p2= Ops.(p2 - post_end) in
    let p_j_right_c=
      let open Ops in
      let vec= Matrix.(apply clockwise_90 vec_end_p2) in
      let base= post_end *< 0.3 + p2 *< 0.7 in
      base + Line.extended_vec ~vec (width*.0.6)
    in
    let l_ctrl1= Ops.(ctrl2 +
      Line.extended_vec ~vec:vec_p_clock90 (width *. 1.50)) in
    let l_ctrl2= Ops.(ctrl1 +
      Line.extended_vec ~vec:vec_p_clock90 (width *. 1.0)) in
    let lerp_p_down= Bezier.lerp3 p_start l_ctrl2 l_ctrl1 p_end in
    let p_end_post=lerp_p_down 0.9 in
    let c2= let open Ops in
      let vec= p_end_post - l_ctrl1 in
      p_end_post + Line.extended_vec ~vec (width*.1.5)
    in
    let segments= [
      Line p_p1;
      Line p_p2;
      Ccurve {ctrl1= r_ctrl1; ctrl2= r_ctrl2; end'= j_start};

      Qcurve {ctrl= p_j_left_c; end'= pre_end};
      Line post_end;
      Qcurve {ctrl= p_j_right_c; end'= p2};

      Qcurve {ctrl= c2; end'= p_end_post};
      Ccurve {ctrl1= l_ctrl1; ctrl2= l_ctrl2; end'= p_start};
      ] in
    {
      start;
      segments;
    }

  (*
    type pj= {
      pj_start: point;
      ctrl1: point adjust;
      ctrl2: point adjust;
      p_end: point;
      end_: point adjust;
    } (* Press – J hook *)
      (* 1. the angle is limited between 45 to 85 degrees *)
  *)

  let from_pj ?(width=Width.width) pj=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= pj.pj_start in
    let p_start= start
    and p_end= pj.p_end in
    let p_length= Point.distance ~from:p_start p_end in
    let vec_p= Ops.(p_end - p_start) in
    let vec_p_clock90= Matrix.(apply clockwise_90 vec_p) in
    let p_p1_vec= Matrix.(apply (anticlock ~radian:(pi*.0.25)) vec_p) in
    let p_p2_vec= Matrix.(apply (clockwise ~radian:(pi*.0.65)) p_p1_vec) in
    let p_p1= Ops.(+) start @@
      Line.extended_vec ~vec:p_p1_vec (width*. 1.5)
    in
    let p_p2= Ops.(+) p_p1 @@
      Line.extended_vec ~vec:p_p2_vec (width*. 0.45)
    in

    let ctrl1= get_adjust pj.ctrl1
      ~f:(fun()-> Ops.(p_p2
      + Line.extended_vec ~vec:vec_p (p_length *. 0.4)
      + Line.extended_vec ~vec:vec_p_clock90 (p_length *. 0.18)
      )) in
    let ctrl2= get_adjust pj.ctrl2
      ~f:(fun()-> Ops.(p_p2
      + Line.extended_vec ~vec:vec_p (p_length *. 0.625)
      + Line.extended_vec ~vec:vec_p_clock90 (p_length *. 0.12)
      )) in
    let _up_ctrl2= ctrl2 in
    let r_ctrl1= ctrl1 in
    let r_ctrl2= ctrl2 in
    let p2= {
      x= p_end.x +. width*.0.5;
      y= p_end.y -. width;
    } in
    let lerp_p_up= Bezier.lerp3 p_p1 r_ctrl1 r_ctrl2 p2 in
    let end'= get_adjust pj.end_ ~f:(fun()->
      let j_base= lerp_p_up (1. -. width*.(0.8+.0.1) /. p_length) in
      { x= j_base.x;
        y= p2.y -. width *. 2.0;
      }) in
    let pre_end= {end' with x= end'.x -. width*.0.1}
    and post_end= {end' with x= end'.x +. width*.0.1} in
    let j_base_left= lerp_p_up (1. -. width*.(0.8+.0.2+.0.4) /. p_length) in
    let j_start= lerp_p_up (1. -. width*.(0.8+.0.2+.1.0) /. p_length) in
    let p_j_left_c=
      let line1= Line.of_points r_ctrl2 j_start
      and line2= Line.of_points pre_end j_base_left in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point) in
    let vec_end_p2= Ops.(p2 - post_end) in
    let p_j_right_c=
      let open Ops in
      let vec= Matrix.(apply clockwise_90 vec_end_p2) in
      let base= post_end *< 0.3 + p2 *< 0.7 in
      base + Line.extended_vec ~vec (width*.0.6)
    in
    let l_ctrl1= Ops.(ctrl2 +
      Line.extended_vec ~vec:vec_p_clock90 (width *. 1.50)) in
    let l_ctrl2= Ops.(ctrl1 +
      Line.extended_vec ~vec:vec_p_clock90 (width *. 1.0)) in
    let lerp_p_down= Bezier.lerp3 p_start l_ctrl2 l_ctrl1 p_end in
    let p_end_post=lerp_p_down 0.9 in
    let c2= let open Ops in
      let vec= p_end_post - l_ctrl1 in
      p_end_post + Line.extended_vec ~vec (width*.1.5)
    in
    let segments= [
      Line p_p1;
      Line p_p2;
      Ccurve {ctrl1= r_ctrl1; ctrl2= r_ctrl2; end'= j_start};

      Qcurve {ctrl= p_j_left_c; end'= pre_end};
      Line post_end;
      Qcurve {ctrl= p_j_right_c; end'= p2};

      Qcurve {ctrl= c2; end'= p_end_post};
      Ccurve {ctrl1= l_ctrl1; ctrl2= l_ctrl2; end'= p_start};
      ] in
    {
      start;
      segments;
    }

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

  let from_thtaj ?(width=Width.width) (thtaj:thtaj)=
    let _h= 0.5
    and _a= 0.5 in
    let _sa= Float.(pow ((pow _a 2.)*.2.) 0.5) in
    let _sah= _sa in
    let open Path in
    let open Point in
    let start= thtaj.thtaj_start in
    let a_radius= get_adjust thtaj.a_radius ~f:(fun ()->
      let avg= (thtaj.h1_length +. thtaj.h2_length) /. 2. in
      avg /. 6.)
    in

    let t1_end= thtaj.t_end in

    let vec_t1= Ops.(t1_end - start) in
    let t1_angle= angle vec_t1 in
    let t1_p1_angle= t1_angle -. pi/.1.8 in
    let t1_p2_angle= t1_p1_angle +. pi*.0.65 in
    let t1_p1= Ops.(+) start @@
      Line.extended_angle ~angle:t1_p1_angle (width*. 1.3)
    in
    let t1_p2= Ops.(+) t1_p1 @@
      Line.extended_angle ~angle:t1_p2_angle (width*. 0.4)
    in
    let t1_p3= {t1_end with x= t1_end.x +. width *.0.4} in
    let t1_down=
      let calc_ctrl= template_curve ~start:t1_p2 ~end':t1_p3 in
      let ctrl1= match thtaj.ctrl1 with
        | Auto-> calc_ctrl ~ratio:{x= 0.07; y= 0.3}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(t1_angle-. (pi*.0.5)) (width*. 0.8)
      and ctrl2= match thtaj.ctrl2 with
        | Auto-> calc_ctrl ~ratio:{x= 0.07; y= 0.7}
        | Specify p-> Ops.(+) p @@
            Line.extended_angle ~angle:(t1_angle-. (pi*.0.5)) (width*. 0.6)
      in
      Ccurve {ctrl1; ctrl2; end'= t1_p3 }
    in
    let t1_up=
      let calc_ctrl= template_curve ~start ~end':t1_end in
      let ctrl1= get_adjust thtaj.ctrl1
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.06; y= 0.3})
      and ctrl2= get_adjust thtaj.ctrl2
        ~f:(fun ()-> calc_ctrl ~ratio:{x= 0.08; y= 0.7})
      in
      Ccurve {ctrl1= ctrl2; ctrl2= ctrl1; end'= start}
    in

    let h1_start= thtaj.t_end in
    let h1_end= { h1_start with x= h1_start.x +. thtaj.h1_length } in
    let a_end= thtaj.a_end in
    let h2_end= { a_end with y= a_end.y -. width} in
    let h2_start= { h2_end with
      x= h2_end.x -. thtaj.h2_length;
    } in
    let vec_h2= Ops.(h2_end - h2_start) in
    let h2_length= distance vec_h2 in
    let end'= get_adjust thtaj.end_ ~f:(fun()->
      { x= h2_end.x -. width; y= h2_end.y -. width *. 3.5}) in
    let vec_h2= Ops.(thtaj.a_end - h2_start) in
    let vec_h2_anti90= Matrix.(apply anticlock_90 vec_h2) in
    let a_center= Ops.(h2_start +
      Line.extended_vec ~vec:vec_h2_anti90 a_radius) in
    let vec_center_start= Ops.(h1_end - a_center) in
    let t2_end=
      let vec= Matrix.(apply anticlock_90 vec_center_start) in
      Ops.(a_center + Line.extended_vec ~vec a_radius)
    in
    let t2_vec= Ops.(t2_end - h1_end) in
    let t2_length= distance t2_vec in
    let t2_c= Ops.(
      (t2_end+h1_end) /< 2. +
      Line.extended_vec
        ~vec:Matrix.(apply clockwise_90 t2_vec)
        (t2_length*.0.05)) in
    let h2_c= Ops.(
      (h2_start+h2_end) /< 2. +
      Line.extended_vec
        ~vec:Matrix.(apply clockwise_90 vec_h2)
        (h2_length*.0.05)) in
    let a_c=
      let line1= Line.of_points t2_c t2_end
      and line2= Line.of_points h2_start h2_c in
      Line.(intersection_of_lines line1 line2 |> get_intersection_point)in

    let h0= h1_start in
    let h1= {h1_end with x= h1_end.x -. width} in
    let h2= {
      x= h1.x +. width*._a;
      y= h1.y -. width*._a; } in
    let h3= {
      x= h2.x +. width*.0.8;
      y= h1_start.y +. width*._h*.0.8 } in
    (* insert v *)
    let h4= {
      x= min (h0.x +. width*.2.) h1.x;
      y= h0.y +. width *. _h;
      } in
    let h5= {
      x= h0.x +. width*._sah;
      y= h0.y +. width*._sah;
      } in
    let h5_c= { h4 with
      x= (h5.x +. h4.x) *. 0.5;
      } in

    let projection=
      let vec= Ops.(t2_c - h1_end) in
      let d= distance vec in
      let r= Float.abs(d /. vec.y) in
      width *. r
    in
    let t2_top_left= { x= h1.x -. projection +. width ; y= h1.y +. width *. _h} in
    let t2_top_right= { t2_top_left with x= t2_top_left.x +. projection } in
    let t2_r_c= t2_c in
    let a_r_c= a_c in
    let up_h2_c= h2_c in
    let l_t2_c= Ops.(t2_c +
      Line.extended_vec ~vec:Matrix.(apply clockwise_90 t2_vec) width) in
    let l_a_c= Ops.(a_c +
      Line.extended_vec ~vec:(a_c - a_center) (width*.1.5)) in
    let down_h2_c= {h2_c with y= h2_c.y+.width} in

    let h2_start_down= let open Ops in
      let vec= Matrix.(apply clockwise_90 (up_h2_c - h2_start)) in
      h2_start + Line.extended_vec ~vec (width) in
    let t2_end_l= let open Ops in
      let vec= Matrix.(apply clockwise_90 (t2_end - t2_r_c)) in
      Ops.(t2_end + Line.extended_vec ~vec width) in

    let j_start= {h2_end with x= h2_end.x -. width*.1.2} in
    let j_right= {x= h2_end.x; y= j_start.y -. width *. 0.1 } in
    let a_end_post= { x= j_start.x-.width*.0.6; y= h2_end.y +. width } in
    let vec_j= Ops.(end' - j_start) in
    let vec_j_clock90= Matrix.(apply clockwise_90 vec_j) in
    let j_length= distance vec_j in
    let end_post= Ops.(end' +
      Line.extended_vec ~vec:vec_j_clock90 (width*.0.2)) in

    let h2_up_right=
      { x= j_start.x -. width*.0.8; y= j_start.y } in

    let vec_h2_up_right= Ops.(h2_up_right - up_h2_c) in
    let h2_j_c1= Ops.( h2_up_right+
      Line.extended_vec ~vec:vec_h2_up_right (j_length*.0.25)) in
    let h2_j_c2= Ops.(end' -
      Line.extended_vec ~vec:vec_j (j_length*.0.133)) in
    let j_h2_right_c= {
      x= end_post.x*.0.95+.j_right.x*.0.05;
      y= j_right.y+.width*.0.1} in
    let end_down_c= Ops.(a_end_post +
      Line.extended_vec ~vec:(a_end_post - down_h2_c) (width *. 1.6)
      ) in
    let segments= [
      Line t1_p1;
      Line t1_p2;
      t1_down;

      Line h1;
      Line h2;
      Line h3;
      Line t2_top_right;
      Qcurve {ctrl= t2_r_c; end'= t2_end};
      Qcurve {ctrl= a_r_c; end'= h2_start};

      Qcurve { ctrl= up_h2_c; end'= h2_up_right };

      Ccurve { ctrl1= h2_j_c1; ctrl2= h2_j_c2; end'; };
      Line end_post;
      Qcurve { ctrl= j_h2_right_c; end'= j_right };

      Qcurve { ctrl= end_down_c; end'=a_end_post; };

      Qcurve {ctrl= down_h2_c; end'= h2_start_down};
      Qcurve {ctrl= l_a_c; end'= t2_end_l};
      Qcurve {ctrl= l_t2_c; end'= t2_top_left};

      Line h4;
      Qcurve { ctrl= h5_c; end'= h5 };
      Line h0;

      t1_up;
    ] in
    {
      start;
      segments;
    }

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

  let from_tod ?(width=Width.width) tod=
    let open Path in
    let open Point in
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
    (*
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
    *)

    let l1_vec= Ops.(bottom - start) in
    let l1_p1_vec= Matrix.(apply (anticlock ~radian:(pi*.0.45)) l1_vec) in
    let l1_p2_vec= Matrix.(apply (clockwise ~radian:(pi*.0.65)) l1_p1_vec) in
    let l1_p1= Ops.(+) start @@
      Line.extended_vec ~vec:l1_p1_vec (width*. 0.5)
    in
    let l1_p2= Ops.(+) l1_p1 @@
      Line.extended_vec ~vec:l1_p2_vec (width*. 0.45)
    in

    let l1o_ctrl1= get_adjust tod.ctrl1 ~f:(fun ()->
      Point.{
        x= start.x -. right_width /. 6.;
        y= start.y +. right_height;
      })
    and l1o_ctrl2= { bottom with x= bottom.x +. c_left_h }
    and l1o_end= bottom in
    let l1o= Ccurve {ctrl1= l1o_ctrl1; ctrl2= l1o_ctrl2; end'= l1o_end} in

    let l2o_ctrl1= { bottom with x= bottom.x -. c_right_h }
    and l2o_ctrl2= { left with y= left.y +. c_right_v }
    and l2o_end= left in
    let l2o= Ccurve {ctrl1= l2o_ctrl1; ctrl2= l2o_ctrl2; end'= l2o_end} in

    let l3o_ctrl1= { left with y= left.y -. c_right_v }
    and l3o_ctrl2= { up with x= up.x -. c_right_h }
    and l3o_end= up in
    let l3o= Ccurve {ctrl1=l3o_ctrl1; ctrl2=l3o_ctrl2; end'=l3o_end} in

    let l4o_ctrl1= { up with x= up.x +. c_left_h }
    and l4o_ctrl2= Point.{
      x= end_.x -. right_width /. 6.;
      y= end_.y -. right_height;
      }
    and l4o_end= end_ in
    let l4o= Ccurve {ctrl1=l4o_ctrl1; ctrl2=l4o_ctrl2; end'=l4o_end} in

    let l4o_vec= Ops.(l4o_end - l3o_end) in
    let l4o_vec_clock90= Matrix.(apply clockwise_90) l4o_vec in

    let l4i_end= {up with y= up.y +. width*.0.5} in
    let l4i_start= Ops.(l4o_end +
      Line.extended_vec ~vec:l4o_vec_clock90 width)
    and l4i_ctrl1= Ops.(l4o_ctrl2 +
      Line.extended_vec ~vec:l4o_vec_clock90 width)
    and l4i_ctrl2= {x= l4o_ctrl1.x -. width*.0.5; y= l4i_end.y} in
    let l4i= Ccurve {ctrl1=l4i_ctrl1; ctrl2=l4i_ctrl2; end'=l4i_end} in

    let oi_ctrl1=
      let open Ops in
      let vec= l4o_end - l4o_ctrl2 in
      l4o_end + Line.extended_vec ~vec width
    and oi_ctrl2=
      let open Ops in
      let vec= l4i_start - l4i_ctrl1 in
      l4i_start + Line.extended_vec ~vec width in
    let oi= Ccurve {ctrl1=oi_ctrl1; ctrl2=oi_ctrl2; end'=l4i_start} in

    let l3i_end= {left with x= left.x +. width*.0.9} in
    let l3i_ctrl1= {x= l3o_ctrl2.x +. width*.0.5; y= l4i_end.y}
    and l3i_ctrl2= {x= l3i_end.x; y= l3o_ctrl1.y-. width*.0.5} in
    let l3i= Ccurve {ctrl1=l3i_ctrl1; ctrl2=l3i_ctrl2; end'=l3i_end} in

    let l2i_end= {bottom with y= bottom.y -. width*.0.5} in
    let l2i_ctrl1= {x= l3i_end.x; y= l2o_ctrl2.y+.width*.0.5}
    and l2i_ctrl2= {x= l2o_ctrl1.x+.width*.0.5; y= l2i_end.y} in
    let l2i= Ccurve {ctrl1=l2i_ctrl1; ctrl2=l2i_ctrl2; end'=l2i_end} in

    let l1o_vec= Ops.(l1o_end - start) in
    let l1o_vec_clock90= Matrix.(apply clockwise_90) l1o_vec in

    let l1i_end= Ops.(start +
      Line.extended_vec ~vec:l1o_vec_clock90 width)
    and l1i_ctrl1= {x= l1o_ctrl2.x -. width*.0.5; y= l2i_end.y}
    and l1i_ctrl2= Ops.(l1o_ctrl1 +
      Line.extended_vec ~vec:l1o_vec_clock90 width) in
    let l1i= Ccurve {ctrl1=l1i_ctrl1; ctrl2=l1i_ctrl2; end'=l1i_end} in

    let segments= [
      Line l1_p1;
      Line l1_p2;
      l1o;
      l2o;
      l3o;
      l4o;
      oi;
      l4i;
      l3i;
      l2i;
      l1i;
      Line start;
      ] in
    { start= l1i_end; segments }


  let to_path_raw ?width= function
    | H h-> [from_h ?width h]
    | Sh sh-> [from_sh ?width sh]
    | U u-> [from_u ?width u]
    | Du du-> [from_du ?width du]
    | V v-> [from_v ?width v]
    | Sv sv-> [from_sv ?width sv]
    | Rsv rsv-> [from_rsv ?width rsv]
    | T t_-> [from_t' ?width t_]
    | Ft ft-> [from_ft ?width ft]
    | Wt wt-> [from_wt ?width wt]
    | D d-> [from_d ?width d]
    | Ed ed-> [from_ed ?width ed]
    | Ld ld-> [from_ld ?width ld]
    | Wd wd-> [from_wd ?width wd]
    | P p-> [from_p ?width p]
    | Up up-> [from_up ?width up]
    | Hp hp-> [from_hp ?width hp]
    | Fp fp-> [from_fp ?width fp]
    | Ufp ufp-> [from_ufp ?width ufp]
    | C c-> [from_c ?width c]
    | A a-> [from_a ?width a]
    | O o-> from_o ?width o
    | Hj hj-> [from_hj ?width hj]
    | Uj uj-> [from_uj ?width uj]
    | Ht ht-> [from_ht ?width ht]
    | Hsv hsv-> [from_hsv ?width hsv]
    | Hv hv-> [from_hv ?width hv]
    | Hvj hvj-> [from_hvj ?width hvj]
    | Htj htj-> [from_htj ?width htj]
    | Utj utj-> [from_utj ?width utj]
    | Hvh hvh-> [from_hvh ?width hvh]
    | Hvu hvu-> [from_hvu ?width hvu]
    | Ha ha-> [from_ha ?width ha]
    | Haj haj-> [from_haj ?width haj]
    | Hpj hpj-> [from_hpj ?width hpj]
    | Htaj htaj-> [from_htaj ?width htaj]
    | Htc htc-> [from_htc ?width htc]
    | Htht htht-> [from_htht ?width htht]
    | Htcj htcj-> [from_htcj ?width htcj]
    | Hvhv hvhv-> [from_hvhv ?width hvhv]
    | Hthtj hthtj-> [from_hthtj ?width hthtj]
    | Vu vu-> [from_vu ?width vu]
    | Vh vh-> [from_vh ?width vh]
    | Va va-> [from_va ?width va]
    | Vaj vaj-> [from_vaj ?width vaj]
    | Vhv vhv-> [from_vhv ?width vhv]
    | Vht vht-> [from_vht ?width vht]
    | Vhtj vhtj-> [from_vhtj ?width vhtj]
    | Vj vj-> [from_vj ?width vj]
    | Vc vc-> [from_vc ?width vc]
    | Vcj vcj-> [from_vcj ?width vcj]
    | Tu tu-> [from_tu ?width tu]
    | Th th-> [from_th ?width th]
    | Td td-> [from_td ?width td]
    | Wtd wtd-> [from_wtd ?width wtd]
    | Tht tht-> [from_tht ?width tht]
    | Thtj thtj-> [from_thtj ?width thtj]
    | Tj tj-> [from_tj ?width tj]
    | Cj cj-> [from_cj ?width cj]
    | Fpj fpj-> [from_fpj ?width fpj]
    | Pj pj-> [from_pj ?width pj]
    | Thtaj thtaj-> [from_thtaj ?width thtaj]
    | Tod tod-> [from_tod ?width tod]

  let to_path= Outline_path to_path_raw
end

(*
let to_path= To_path.to_path
let to_frame t=
  let frame, _last=t |> to_path |> Smaji_glyph_path.Path.frame in
  frame
*)
