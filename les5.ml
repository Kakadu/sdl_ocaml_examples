open Tsdl.Sdl
open Printf

let (>>=) a f = match a with `Error -> `Error
                           | `Ok x -> f x
let (>>) a f  = match a with `Error -> `Error
                           | `Ok x -> `Ok (f x)


let screen_width = 640
let screen_height = 480

let render_texture_in ?clip ~texture ~renderer ~x ~y =
  (match clip with
    | Some r ->
      let (w,h) = Rect.(w r, h r) in
      render_copy ~src:r ~dst:(Rect.create ~w ~h ~x ~y) renderer texture
    | None   ->
      query_texture texture >>= fun (_,_,(w,h)) ->
      render_copy ~dst:(Rect.create ~x ~y ~w ~h) renderer texture
  )

let main () =
  create_window "Lesson 5 (try keys 1,2,3,4)"
    ~x:100 ~y:100 ~w:screen_width ~h:screen_height Window.opengl >>= fun window ->
  create_renderer ~flags:Renderer.(accelerated + presentvsync) window >>= fun renderer ->
  img_load_texture renderer "res/lesson5/image.png" >>= fun image ->
  query_texture image >>= fun (_,_,(iW,iH)) ->
  let w = iW / 2 in
  let h = iH / 2 in
  let clips =
    [ (Rect.create ~w ~h ~x:0 ~y:0, (0, 0))
    ; (Rect.create ~w ~h ~x:w ~y:0, (screen_width-w, 0))
    ; (Rect.create ~w ~h ~x:0 ~y:h, (0, screen_height-h))
    ; (Rect.create ~w ~h ~x:w ~y:h, (screen_width-w, screen_height-h))
    ]
  in
  let e = Event.create () in
  let n = ref 1 in
  let rec loop last =
    match last with
    | `Error -> `Error
    | `Ok _ -> begin
        let quit = ref false in
        while poll_event (Some e) do
          let etyp = Event.get e Event.typ in
          if etyp = Event.quit ||
             etyp = Event.mouse_button_down then quit := true
          else
            let keycode = Event.get e Event.keyboard_keycode in
            if keycode = K.escape then quit := true else
            if keycode = K.k1 then n:=1 else
            if keycode = K.k2 then n:=2 else
            if keycode = K.k3 then n:=3 else
            if keycode = K.k4 then n:=4

        done;
        let init = render_clear renderer in
        List.fold_left (fun acc (_,(x,y)) -> acc >>= fun _ ->
          let clip = List.nth clips (!n-1) |> fst in
          render_texture_in ~clip ~texture:image ~renderer ~x ~y) init clips
        >>= fun _ ->
        render_present renderer;
        if not !quit then loop (`Ok ()) else `Error
     end
  in
  loop (`Ok ()) >>= fun _ ->
  destroy_texture image;
  destroy_renderer renderer;
  destroy_window window;
  `Ok ()

let (_ : _ result) =
  match init Init.video with
  | `Error -> exit 1
  | `Ok () -> `Ok (main ())
