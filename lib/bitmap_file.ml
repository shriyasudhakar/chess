open Color

(* Using source of [Graphic_image] to modify it to support transparency.
   Transparency is set on all pixels that match the first pixel. Original code
   from camlimages/graphics/graphic_image.ml for [array_of_image] which you can
   navigate by clicking on F12 on [Graphic_image] if you have camlimages. *)
let array_of_image img =
  match img with
  | Images.Rgb24 bitmap ->
      let w = bitmap.Rgb24.width in
      let h = bitmap.Rgb24.height in
      let pixel j i =
        let { r; g; b } = Rgb24.get bitmap j i in
        Graphics.rgb r g b
      in
      let corner = pixel 0 0 in
      Array.init h (fun i ->
          Array.init w (fun j ->
              let p = pixel j i in
              if p = corner then Graphics.transp else p))
  | _ -> failwith "unhandled bitmap format"

(* StackOverflow provided this solution to load images. Png files require
   additional libraries but bitmaps do not need any extra libraries to be
   installed.
   https://stackoverflow.com/questions/50108152/load-image-from-file-in-ocaml-graphics *)
let load filename =
  let img = Bmp.load filename [] in
  Graphics.make_image (array_of_image img)
