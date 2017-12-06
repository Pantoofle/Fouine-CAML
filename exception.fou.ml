let t_int = fun k kE -> k _e1 in t_int ;;
let t_op  = fun k kE -> _e2 (fun v2 -> _e1 (fun v1 -> k (v1 _+ v2)) kE) kE in t_op ;;
let t_not = fun k kE -> _e1 (fun v -> k (not v)) kE in t_not ;;
let t_let = fun k kE -> _e1 (fun _x -> _e2 k kE) kE in t_let ;;
let t_rec = fun k kE -> _e1 (fun _x -> _e2 k kE) kE in t_rec ;;
let t_fun = fun k kE -> k (fun _x -> _e1) in t_fun ;;
let t_var = fun k kE -> k _e1 in t_var ;;
let t_app = fun k kE -> _e2 (fun v2 -> _e1 (fun f -> f v2 k kE) kE) kE in t_app ;;
let t_seq = fun k kE -> _e1 (fun v1 -> _e2 (fun v2 -> k v2) kE) kE in t_seq ;;
let t_ifte =
  fun k kE ->
    _e1 (
      fun v ->
	if v
	then _e2 (fun v1 -> k v1) kE
	else _e3 (fun v2 -> k v2) kE
    ) kE
in t_ifte ;;
let t_raise = fun k kE -> _e1 (fun v -> kE v) kE in t_raise ;;
let t_try = fun k kE -> _e1 k (fun _x -> _e2 k kE) in t_try ;;
let use = fun f -> let k x = x in feedback f; f k k in use;;
