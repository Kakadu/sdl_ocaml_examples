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
  create_window "Lesson 4" ~x:100 ~y:100 ~w:screen_width ~h:screen_height Window.opengl >>= fun w ->
  create_renderer ~flags:Renderer.(accelerated + presentvsync) w >>= fun renderer ->
  img_load_texture renderer "res/lesson3/image.png" >>= fun image ->
  query_texture image >>= fun (_,_,(iW,iH)) ->
  let x = (screen_width -  iW) / 2 in
  let y = (screen_height - iH) / 2 in

  let e = Event.create () in
  let rec loop last =
    match last with
    | `Error -> `Error
    | `Ok _ -> begin
        let quit = ref false in
        while poll_event (Some e) do
          let x = Event.get e Event.typ in
          if x=Event.quit || x = Event.mouse_button_down || x = Event.key_down then quit := true
        done;
        render_clear renderer >>= fun _ ->
        render_texture_in ~texture:image ~renderer ~x ~y >>= fun _ ->
        render_present renderer;
        if not !quit then loop (`Ok ()) else `Error
     end
  in
  loop (`Ok ()) >>= fun _ ->
  destroy_texture image;
  destroy_renderer renderer;
  destroy_window w;
  `Ok ()

let (_ : _ result) =
  match init Init.video with
  | `Error -> exit 1
  | `Ok () -> `Ok (main ())
