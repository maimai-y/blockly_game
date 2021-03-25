open UniverseJs
open Color
open Image
open World
open TransformToInt

let width = 1000
let height = 600
let image_size = 150
let half_image_size = image_size / 2
let acceleration = 1
let cat_image = read_image "https://1.bp.blogspot.com/-APqJJHLSDqo/XOPXToXOZ1I/AAAAAAABS7c/CQQUiQq04jkZ8h14x6_HEydL6EqqUs5qgCLcBGAs/s800/yumekawa_animal_neko.png" image_size image_size
let panda_image = read_image "https://1.bp.blogspot.com/-tJSdNxtkb9s/XkZdUlN_4hI/AAAAAAABXWY/Nrs4P6Q9lGsowouXErIjOe6iKDEHd3JpgCNcBGAsYHQ/s1600/yumekawa_animal_panda.png" image_size image_size
let taiyaki_image = read_image "https://1.bp.blogspot.com/-3kpSsqCU_6Y/YEGQIAMUCCI/AAAAAAABdfI/LyM6y3Rk-jkCAsSFl5_hUq4VL1vTClpfACNcBGAsYHQ/s840/sweets_shiroi_taiyaki_white.png" image_size image_size
let background = place_image (read_image "https://3.bp.blogspot.com/-zqmSqt_BOQE/V8jqWFasBOI/AAAAAAAA9dM/G4gaQ9NMsXgjXXwJuazoWnknp8mQm_VBgCLcB/s2000/bg_heaven_tengoku.jpg" width height) (width / 2, height / 2) (empty_scene width height)
type character_t = {
  posn : int * int;
  vec : int * int;
}
type world_t = {
  cat : character_t;
  panda : character_t;
  list_of_taiyakis : character_t list;
}
let to_taiyaki n =
  {posn = (width * n, half_image_size); vec = (-10, 0)}
let initial_world = {cat = {posn = (half_image_size, half_image_size); vec = (10, 0)}; panda = {posn = (width - half_image_size, half_image_size); vec = (-10, 0)}; list_of_taiyakis = List.map to_taiyaki [2; 3; 4; 5; 7]}
let get_posn {posn = posn_v; vec = vec_v} =
  posn_v
let to_taiyaki_image x =
  taiyaki_image
let f v =
  1
let size lst =
  sum (List.map f lst)
let draw {cat = cat_v; panda = panda_v; list_of_taiyakis = list_of_taiyakis_v} =
  place_image cat_image (get_posn cat_v) (place_image panda_image (get_posn panda_v) (place_images (List.map to_taiyaki_image list_of_taiyakis_v) (List.map get_posn list_of_taiyakis_v) (place_image (text (string_of_int (5 - size list_of_taiyakis_v) ^ " / 5") 20 Color.white) (40, 30) (place_image (text "▲: jump" 20 Color.white) (60, 60) background))))
let outside_range_x posn_x vec_x =
  if posn_x >= width - half_image_size && vec_x > 0 || posn_x <= half_image_size && vec_x < 0 then true
  else false
let outside_range_y posn_y vec_y =
  if posn_y >= height - half_image_size && vec_y > 0 then true
  else false
let compute_new_posn (posn_x, posn_y) (vec_x, vec_y) =
  (posn_x + vec_x, posn_y + vec_y)
let compute_new_vec (posn_x, posn_y) (vec_x, vec_y) =
  ((if outside_range_x posn_x vec_x then vec_x * (-1)
  else vec_x), (if outside_range_y posn_y vec_y then vec_y * (-1)
  else vec_y + acceleration))
let move_taiyaki {posn = posn_v; vec = vec_v} =
  {posn = compute_new_posn posn_v vec_v; vec = compute_new_vec posn_v vec_v}
let overlaying (posn1_x, posn1_y) (posn2_x, posn2_y) =
  let dist = (posn1_x - posn2_x) * (posn1_x - posn2_x) + (posn1_y - posn2_y) * (posn1_y - posn2_y)
  in (if dist <= half_image_size * half_image_size then true
  else false)
let on_tick {cat = {posn = cat_posn_v; vec = cat_vec_v}; panda = {posn = panda_posn_v; vec = panda_vec_v}; list_of_taiyakis = list_of_taiyakis_v} =
  {cat = {posn = compute_new_posn cat_posn_v cat_vec_v; vec = compute_new_vec cat_posn_v cat_vec_v}; panda = {posn = compute_new_posn panda_posn_v panda_vec_v; vec = compute_new_vec panda_posn_v panda_vec_v}; list_of_taiyakis = (let cat_didnt_get {posn = posn_v; vec = vec_v} =
  not (overlaying posn_v cat_posn_v)
  in List.map move_taiyaki (List.filter cat_didnt_get list_of_taiyakis_v))}
let on_key {cat = {posn = cat_posn_v; vec = (cat_vec_x, cat_vec_y)}; panda = panda_v; list_of_taiyakis = list_of_taiyakis_v} key =
  {cat = {posn = cat_posn_v; vec = (cat_vec_x, (if key = "up" then (if cat_vec_y > 0 then -20
  else cat_vec_y - 5)
  else cat_vec_y))}; panda = panda_v; list_of_taiyakis = list_of_taiyakis_v}
let game_over {cat = cat_v; panda = panda_v; list_of_taiyakis = list_of_taiyakis_v} =
  overlaying (get_posn cat_v) (get_posn panda_v) || list_of_taiyakis_v = []
let draw_last {cat = cat_v; panda = panda_v; list_of_taiyakis = list_of_taiyakis_v} =
  if list_of_taiyakis_v = [] then place_image (text "game clear!!" 60 Color.pink) (200, 400) (place_image (read_image "https://1.bp.blogspot.com/-3kpSsqCU_6Y/YEGQIAMUCCI/AAAAAAABdfI/LyM6y3Rk-jkCAsSFl5_hUq4VL1vTClpfACNcBGAsYHQ/s840/sweets_shiroi_taiyaki_white.png" 200 200) (700, 350) (place_image (read_image "https://1.bp.blogspot.com/-APqJJHLSDqo/XOPXToXOZ1I/AAAAAAABS7c/CQQUiQq04jkZ8h14x6_HEydL6EqqUs5qgCLcBGAs/s800/yumekawa_animal_neko.png" 700 700) (700, 300) background))
  else place_image (text "game over..." 60 (make_color 100 149 237)) (width / 2, height / 2) (draw {cat = cat_v; panda = panda_v; list_of_taiyakis = list_of_taiyakis_v})
;; big_bang initial_world
  ~width:width
  ~height:height
  ~to_draw:draw
  ~on_tick:on_tick
  ~on_key_press:on_key
  ~rate:50
  ~stop_when:game_over
  ~to_draw_last:draw_last