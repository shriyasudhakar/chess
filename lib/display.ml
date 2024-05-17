open Position
(** @author Kiran Mitra (km936), Rayhan Khanna (rk696), Shriya Sudhakar (ss3576) *)

type user_input =
  | Click of position
  | Key of char

let scale = 80
let size = scale * 8
let dark = 0x779556
let light = 0xEBECD0
let highlight_light = 0xF5F568
let highlight_dark = 0xB5B548

let initialize () =
  Graphics.open_graph (Printf.sprintf " %dx%d" size size);
  Graphics.set_window_title "Chess";
  Graphics.remember_mode true

let fill_rect ?(highlight = false) x y =
  Graphics.set_color
    (if (x + y) mod 2 == 1 then if highlight then highlight_light else light
     else if highlight then highlight_dark
     else dark);
  Graphics.fill_rect (x * scale) (y * scale) scale scale

let resize_window () =
  if Graphics.size_x () != size || Graphics.size_y () != size then
    Graphics.resize_window size size
  else ()

let set_title str = Graphics.set_window_title str

let draw_image_centered (image : Graphics.image) =
  let img = Graphics.dump_image image in
  let w = Array.length img.(0) in
  let h = Array.length img in
  Graphics.draw_image image ((size - w) / 2) ((size - h) / 2)

let auto_synchronize sync =
  Graphics.synchronize ();
  Graphics.auto_synchronize sync

let draw_tile ?(highlight = false) image position =
  fill_rect ~highlight position.x position.y;
  Graphics.draw_image image (position.x * scale) (position.y * scale)

let rec next_input () =
  try
    let status = Graphics.wait_next_event [ Button_down; Key_pressed ] in
    if status.button then
      Click
        {
          Position.x = status.mouse_x / scale;
          Position.y = status.mouse_y / scale;
        }
    else if status.keypressed then Key status.key
    else next_input ()
  with Graphics.Graphic_failure _ ->
    failwith "Graphics window closed by player."
