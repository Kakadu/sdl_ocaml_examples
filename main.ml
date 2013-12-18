open Tsdl.Sdl

let (>>=) a f = match a with `Error -> `Error
                           | `Ok x -> f x
let (>>) a f  = match a with `Error -> `Error
                           | `Ok x -> `Ok (f x)

let main () = 
  create_window "Lesson 1" ~x:100 ~y:100 ~w:640 ~h:480 Window.opengl >>= fun w ->
  create_renderer ~flags:Renderer.(accelerated + presentvsync) w >>= fun renderer -> 
  load_bmp "hello.bmp" >>= fun bmp ->
  create_texture_from_surface renderer bmp >>= fun tex ->
  free_surface bmp;
  render_clear renderer >>= fun () ->
  render_copy renderer tex >>= fun () ->
  render_present renderer;
  delay 2000l;
  destroy_texture tex;
  destroy_renderer renderer;
  destroy_window w;
  `Ok (quit ())
  
let (_ : _ result) =
  match init Init.video with
  | `Error -> exit 1
  | `Ok () -> `Ok (main ())

