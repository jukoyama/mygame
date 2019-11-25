open UniverseJs
open World
open Image
open Color

(* world_t :    りんごの座標,   鳥の座標,      鳥の加速度,    スコア の組の型 *)
type world_t = (int * int) * (int * int) * (int * int) * int

;;Random.self_init ()

(* ----------画像関係---------- *)

let width  = 560	(* 画面の幅 *)
let height = 560	(* 画面の高さ *)

let fish_width  = 50  (* 魚の画像の横幅 *)
let fish_height = 35  (* 魚の画像の縦幅 *)
let bird_width  = 70  (* 鳥の横幅 *)
let bird_height = 79  (* 鳥の縦幅 *)

let rec_width   = 400 (* ゲーム終了時画面の幅 *)
let rec_height  = 400 (* ゲーム終了時画面の高さ *)

let background	= read_image "images/ocean.jpeg"  (* 背景画像 *)
let fish	= read_image "images/fish.png"    (* 魚 *)
let bird	= read_image "images/penguin.png" (* 鳥 *)

(* ----------画像関係ここまで---------- *)

(* -------------初期値関係------------ *)

(* worldの初期値 *)
let initial_world =
  ((300, 300), (* 魚の位置 *)
   (50,  500), (* 鳥の位置 *)
   (20, 0),    (* 鳥の加速度 *)
   0)			            (* スコア *)

(* -----初期値関係ここまで----- *)




(* -----スコアの判定関係-----*)

(* 魚の点数 *)
let fish_score  = 10

(* 果物と鳥の座標をもらう *)
(* 果物がキャッチされていたら score、されていなかったら 0 を返す *)
let check (ax, ay) (x, y) score =
  (* if x < ax + fish_width && ax < x + bird_width  && ay > y - bird_height && y > ay - fish_height *)
	if x <= ax && ax + fish_width <= x + bird_width && ay <= y && y - bird_height <= ay - fish_height
  then score
  else 0

(* 果物をキャッチしているか判定してスコアを出す *)
let check_score ((ax, ay), (x, y)) = check (ax, ay) (x, y) fish_score
(* -----スコアの判定関係ここまで-----*)



(* -----on_tick関係----- *)
(* 魚がキャッチされたら、ランダムな座標を返す *)
let rand_vector score (x, y)=
	if score = 0 then (x, y)
	else
		let new_x = Random.int 460 in
		let new_y = 460 + (Random.int 100) in
	(new_x, new_y)

(* 果物の y 座標が一番下についていたら一番上にもってくる *)
(* up_bird : int -> int *)
let up_bird y =
  if y < bird_width then height
	else if y > height then bird_width
	else y

(* 鳥をマウスの方向によって動かす *)
(* move_bird : int * int -> int -> int -> int * int *)
let move_bird (x, y) (nx, ny) = (x + nx, y + ny)


(* 魚がキャッチされたら、魚の位置が移動し、scoreを加算 *)
(* move_on_tick : world_t -> int * int -> (world_t, 'a) World.t *)
let move_on_tick ((ax, ay), (x, y), (acx, acy), score) =
	(rand_vector (check_score ((ax, ay), (x, y))) (ax, ay),
	 move_bird (x, y) (acx, acy),
	 (acx, acy),
	 score + check_score ((ax, ay), (x, y))
	 )

(* -----on_tick関係ここまで----- *)




(* -----キー操作関係-----*)

(* キーによって鳥の動きが決まる *)
(* key_draw : world_t -> (world_t, 'a) World.t *)
let key_draw ((ax, ay), (x, y), (acx, acy), score) key =
 (if key = "left" then
		move_on_tick ((ax, ay), (x, y), (-20, 0), score)
  else if key = "right" then
		move_on_tick ((ax, ay), (x, y), (20, 0), score)
	else if key = "up" then
		move_on_tick ((ax, ay), (x, y), (0, 20), score)
	else if key = "down" then
		move_on_tick ((ax, ay), (x, y), (0, -20), score)
  else
		move_on_tick ((ax, ay), (x, y), (20, 0), score))

(* -----キー操作関係ここまで-----*)




(* -----描画関係----- *)

(* y座標を表示用に変換 *)
(* change_y : (int * int) -> int *)
let change_y y = height - y

(* 各座標から画像を作成 *)
(* draw : world_t -> Image.t *)
let draw ((ax, ay), (x, y), (acx, acy), score) =
  (place_image (text (string_of_int score) ~size:50 bisque4) (float_of_int (x + bird_width / 2 - 15), float_of_int (change_y (y - bird_width / 2 )))
  (place_image bird   (float_of_int x,  float_of_int (change_y y))
  (place_image fish  (float_of_int ax, float_of_int (change_y ay))
	 background)))

(* -----描画関係ここまで----- *)


(* -----ゲーム終了関係----- *)

(* 壁にぶつかったら終了 *)
(* game_finished : world_t -> bool *)
let game_finished ((ax, ay), (x, y), (acx, acy), score) =
	if y < bird_width then true
	else if y > height then true
	else if x < 0 then true
	else if x > width - bird_width then true
	else false

(* 終了画面の位置 *)
let (rec_x, rec_y)   = (float_of_int ((width - rec_width) / 2), float_of_int ((height - rec_height) / 2))
(* Game Over のテキストの位置 *)
let (texg_x, texg_y) = ((float_of_int (width / 2)) -. rec_x, rec_y +. float_of_int (rec_height / 4) -. 15.)
(* Score という文字の位置 *)
let (texs_x, texs_y) = (texg_x +. (rec_x /. 2.), texg_y +. rec_y)
(* 獲得した点数の表示いち *)
let (texn_x, texn_y) = (texg_x +. rec_x, texs_y +. rec_y)


(* 終了画面の描画 *)
(* draw_game_over : world_t -> Image.t *)
let draw_game_over ((ax, ay), (x, y), (acx, acy), score) =
  place_image (text "Game Over" ~size:30 white) (texg_x, texg_y)
							(place_image (text "Score" ~size:30 white) (texs_x, texs_y)
							(place_image (text (string_of_int score) ~size:30 white) (texn_x, texn_y)
							(place_image (rectangle (float_of_int rec_width) (float_of_int rec_height) (make_color 102 102 102)) (rec_x, rec_y)
              (draw ((ax, ay), (x, y), (acx, acy), score)))))

(* -----ゲーム終了条件はここまで----- *)


(* ゲーム開始 *)
let _ =
  big_bang initial_world
	   ~name:"catch_fish"
     ~width:width
	   ~height:height
	   ~to_draw:draw
	   ~on_key_press:key_draw
	   ~on_tick:move_on_tick
	   ~rate:0.1			               (* ゲームの動く速さ *)
		 ~stop_when:game_finished
		 ~to_draw_last:draw_game_over
