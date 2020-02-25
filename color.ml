(*
                              CS51 Lab 7
                          Modules & Functors

                 A module for colors and color names

The representation for colors in this implementation is really obscure
and arguably unnecessarily so. By the way, it also has some bugs so it
doesn't pass all the unit tests. No need to debug it though. You'll be
replacing it wholesale with a simpler implementation. *)

(* 8-bit RGB channel colors *)
type color = (int*int*int) ;;

(* Some standard color names *)
type color_name =
  | Red
  | Green
  | Blue
  | Orange
  | Yellow
  | Indigo
  | Violet ;;

(* to_color r g b -- Returns the color corresponding to the RGB
   values given by r, g, and b *)
(* let to_color (r : int) (g : int) (b : int) : color =
  r lsl 0b10000 + g lsl 0b1000 + b ;; *)

(* red c -- Returns the red channel value for the color c *)
(* let red (c : color) : int =
  c lsr 0b10000  ;;

(* green c -- Returns the green channel value for the color c *)
let green (c : color) : int =
  (c lsr 0b1000) land 0b11111111 ;;

(* blue c -- Returns the blue channel value for the color c *)
let blue (c : color) : int =
   c land 0b11111111 ;; *)

(* color_named name -- Returns the color (as RGB representation)
   corresponding to the color name *)

let color_named (name : color_name) : color =
  match name with
  | Red ->    (255, 0, 0)
  | Green ->  (0, 255, 0)
  | Blue ->   (0, 0, 255)
  | Orange -> (255, 165, 0)
  | Yellow -> (255, 255, 0)
  | Indigo -> (75, 0, 130)
  | Violet -> (240, 130, 240) ;;

let rgb_named (name : color_name) : color =
  match name with
  | (255, 0, 0) -> Red
  | (0, 255, 0) -> Green
  | (0, 0, 255) -> Blue
  | (255, 165, 0) -> Orange
  | (255, 255, 0) -> Yellow
  | (75, 0, 130) -> Indigo
  | (240, 130, 240) -> Violet ;;
