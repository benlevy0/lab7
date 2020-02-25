
type color
type color_name =
  | Red
  | Green
  | Blue
  | Orange
  | Yellow
  | Indigo
  | Violet
val color_named : color_name -> color
val rgb_named : color -> color_name
