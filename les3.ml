open Tsdl.Sdl
open Printf

let (>>=) a f = match a with `Error -> `Error
                           | `Ok x -> f x
let (>>) a f  = match a with `Error -> `Error
                           | `Ok x -> `Ok (f x)

module List = struct
  include List
  let init f n =
    let ans = ref [] in
    for i=n-1 downto 0 do ans:=(f i):: !ans done;
    !ans
end

let screen_width = 640
let screen_height = 480
let tile_size = 160

let render_texture ~texture ~renderer ~rect =
  render_copy renderer texture ~dst:rect

let render_texture_in ~texture ~renderer ~x ~y = 
  query_texture texture >>= fun (_,_,(w,h)) ->
  render_texture ~texture ~renderer ~rect:(Rect.create ~x ~y ~w ~h)

let main () = 
  create_window "Lesson 3" ~x:100 ~y:100 ~w:screen_width ~h:screen_height Window.opengl >>= fun w ->
  create_renderer ~flags:Renderer.(accelerated + presentvsync) w >>= fun renderer -> 
  img_load_texture renderer "res/lesson3/background.png" >>= fun background ->
  img_load_texture renderer "res/lesson3/image.png" >>= fun image ->
  let xtiles = screen_width / tile_size in
  let ytiles = screen_height / tile_size in
  List.init (fun i -> (i / ytiles, i mod  ytiles)) (xtiles*ytiles)
  |> List.fold_left (fun acc (x,y) -> 
    acc >>= fun _ ->  render_texture  ~texture:background ~renderer 
             ~rect:(Rect.create ~x:(x*tile_size) ~y:(y*tile_size) ~w:tile_size ~h:tile_size)) (`Ok ())
  >>= fun _ -> query_texture image >>= fun (_,_,(iW,iH)) -> 
  let x = (screen_width -  iW) / 2 in
  let y = (screen_height - iH) / 2 in
  render_texture_in ~texture:image ~renderer ~x ~y >>= fun _ -> 
  render_present renderer;
  delay 2000l;
  destroy_texture background;
  destroy_texture image;
  destroy_renderer renderer;
  destroy_window w;
  `Ok (quit ())
  
let (_ : _ result) =
  match init Init.video with
  | `Error -> exit 1
  | `Ok () -> `Ok (main ())

