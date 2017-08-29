open Core

module Make (Floatlike : Floatlike.For_autodiff) = struct
  module Univar = struct
    type t =
      { f : Floatlike.t -> Floatlike.t
      ; f' : t Lazy.t
      }

    let eval { f; f' = _ } y = f y

    let d { f = _; f' } = Lazy.force f'

    let rec c y =
      { f = Fn.const y
      ; f' = Lazy.from_fun (fun () -> c Floatlike.zero)
      }

    let zero = c Floatlike.zero

    let one = c Floatlike.one

    let two = c Floatlike.(one + one)

    let x =
      { f = Fn.id
      ; f' = Lazy.from_fun (fun () -> one)
      }

    let rec scale t k =
      { f = (fun y -> Floatlike.( * ) k (eval t y))
      ; f' = Lazy.from_fun (fun () -> scale (d t) k)
      }
    let rec (+) g h =
      { f = (fun y -> Floatlike.(+) (eval g y) (eval h y))
      ; f' = Lazy.from_fun (fun () -> d g + d h)
      }

    let rec (-) g h =
      { f = (fun y -> Floatlike.(-) (eval g y) (eval h y))
      ; f' = Lazy.from_fun (fun () -> d g - d h)
      }

    let rec ( * ) g h =
      { f = (fun y -> Floatlike.( * ) (eval g y) (eval h y))
      ; f' = Lazy.from_fun (fun () -> g * d h + d g * h)
      }

    let rec compose g h =
      { f = (fun y -> eval g (eval h y))
      ; f' = Lazy.from_fun (fun () -> compose (d g) h * d h)
      }

    module Uncomposed = struct
      let int_pow n =
        let rec int_pow_and_scale n k =
          { f = (fun y -> Floatlike.(k * int_pow y n))
          ; f' = Lazy.from_fun (fun () -> int_pow_and_scale (Int.pred n) Floatlike.(of_int n * k))
          }
        in
        int_pow_and_scale n Floatlike.one

      let (/) g h = g * (compose (int_pow (-1)) h)

      let pow p =
        let rec pow_and_scale ~power:p k =
          { f = (fun y -> Floatlike.(k * (y ** p)))
          ; f' = Lazy.from_fun (fun () -> pow_and_scale ~power:Floatlike.(p - one) Floatlike.(p * k))
          }
        in
        pow_and_scale ~power:p Floatlike.one

      let exp =
        let rec exp' () =
          { f = Floatlike.exp
          ; f' = Lazy.from_fun (fun () -> exp' ()) }
        in
        exp' ()

      let log =
        { f = Floatlike.log
        ; f' = Lazy.from_fun (fun () -> int_pow (-1))
        }

      let sin, cos =
        let rec sin' () =
          { f = Floatlike.sin
          ; f' = Lazy.from_fun (fun () -> cos' ()) }
        and cos' () =
          { f = Floatlike.cos
          ; f' = Lazy.from_fun (fun () -> scale (sin' ()) Floatlike.(zero - one)) }
        in
        sin' (), cos' ()

      let tan = sin / cos

      let abs =
        let rec abs' () =
          { f = Floatlike.abs
          ; f' = Lazy.from_fun (fun () -> abs' () * int_pow (-1))
          }
        in
        abs' ()

      let step = (abs + x) / (two * x)

      let relu = (abs + x) / two

      let softplus = compose log (one + exp)

      let sigmoid =
        let rec sigmoid' () =
          { f = (fun y -> Floatlike.(one * int_pow (one + exp ((zero - one) * y)) (-1)))
          ; f' = Lazy.from_fun (fun () -> sigmoid' () * (one - sigmoid' ()))
          }
        in
        sigmoid' ()
    end

    let int_pow t n = compose (Uncomposed.int_pow n) t

    let (/) = Uncomposed.(/)

    let pow t p = compose (Uncomposed.pow p) t

    let exp t = compose Uncomposed.exp t

    let log t = compose Uncomposed.log t

    let ( ** ) g h = exp (h * log g)

    let sin t = compose Uncomposed.sin t

    let cos t = compose Uncomposed.cos t

    let tan t = compose Uncomposed.tan t

    let abs t = compose Uncomposed.abs t

    let step t = compose Uncomposed.step t

    let relu t = compose Uncomposed.relu t

    let softplus t = compose Uncomposed.softplus t

    let sigmoid t = compose Uncomposed.sigmoid t
  end

  module Unidim = struct
    type t =
      { f : Floatlike.t Infinite_list.t -> Floatlike.t
      ; f' : t Infinite_list.t Lazy.t
      }

    let eval { f; f' = _ } y = f y

    let eval' t y = eval t (Infinite_list.of_list y ~default:Floatlike.zero)

    let grad { f = _; f' } = Lazy.force f'

    let rec c y =
      { f = Fn.const y
      ; f' = Lazy.from_fun (fun () -> Infinite_list.constant ~default:(c Floatlike.zero))
      }

    let zero = c Floatlike.zero

    let one = c Floatlike.one

    let two = c Floatlike.(one + one)

    let x_i i =
      { f = (fun ys -> Infinite_list.nth_exn ys i)
      ; f' = Lazy.from_fun (fun () -> Infinite_list.e_i i ~zero ~one)
      }

    let x_0 = x_i 0

    let x_1 = x_i 1

    let x_2 = x_i 2

    let rec scale t k =
      { f = (fun ys -> Floatlike.( * ) k (eval t ys))
      ; f' = Lazy.from_fun (fun () -> Infinite_list.map (grad t) ~f:(fun dt -> scale dt k))
      }

    let rec (+) g h =
      { f = (fun ys -> Floatlike.(+) (eval g ys) (eval h ys))
      ; f' = Lazy.from_fun (fun () -> Infinite_list.map2 (grad g) (grad h) ~f:(+))
      }

    let rec (-) g h =
      { f = (fun ys -> Floatlike.(-) (eval g ys) (eval h ys))
      ; f' = Lazy.from_fun (fun () -> Infinite_list.map2 (grad g) (grad h) ~f:(-))
      }

    let rec ( * ) g h =
      { f = (fun ys -> Floatlike.( * ) (eval g ys) (eval h ys))
      ; f' = Lazy.from_fun (fun () -> Infinite_list.map2 (grad g) (grad h) ~f:(fun dg dh -> g * dh + dg * h))
      }

    let rec compose_univar g h =
      { f = (fun ys -> (Univar.eval g (eval h ys)))
      ; f' = Lazy.from_fun (fun () ->
            let dg_of_h = compose_univar (Univar.d g) h in
            Infinite_list.map (grad h) ~f:(fun dh -> dg_of_h * dh))
      }

    let rec compose g hs =
      { f = (fun ys -> eval g (Infinite_list.map hs ~f:(fun h -> eval h ys)))
      ; f' = Lazy.from_fun (fun () ->
            let grad_g_of_hs =
              Infinite_list.map (grad g) ~f:(fun dg -> compose dg hs)
            in
            let grad_hs = Infinite_list.map hs ~f:grad in
            let grad_hs_transposed = Infinite_list.transpose grad_hs in
            Infinite_list.map grad_hs_transposed ~f:(fun dh ->
              let terms = Infinite_list.map2 grad_g_of_hs dh ~f:( * ) in
              Infinite_list.fold terms ~init:zero ~f:(+) ~f_default:(fun acc _ -> acc)))
      }

    let compose' g hs = compose g (Infinite_list.of_list hs ~default:zero)

    let int_pow t n = compose_univar (Univar.Uncomposed.int_pow n) t

    let (/) g h = g * (int_pow h (-1))

    let pow t p = compose_univar (Univar.Uncomposed.pow p) t

    let exp t = compose_univar Univar.Uncomposed.exp t

    let log t = compose_univar Univar.Uncomposed.log t

    let ( ** ) g h = exp (h * log g)

    let sin t = compose_univar Univar.Uncomposed.sin t

    let cos t = compose_univar Univar.Uncomposed.cos t

    let tan t = compose_univar Univar.Uncomposed.tan t

    let abs t = compose_univar Univar.Uncomposed.abs t

    let step t = compose_univar Univar.Uncomposed.step t

    let relu t = compose_univar Univar.Uncomposed.relu t

    let softplus t = compose_univar Univar.Uncomposed.softplus t

    let sigmoid t = compose_univar Univar.Uncomposed.sigmoid t
  end

  include Unidim

  module Multidim = struct
    type unidim = Unidim.t

    type t =
      { f : Floatlike.t Infinite_list.t -> Floatlike.t Infinite_list.t
      ; f' : Unidim.t Infinite_list.t Infinite_list.t Lazy.t (* the outer list corresponds to the output *)
      }

    let eval { f; f' = _ } y = f y

    let jacobian { f = _; f' } = Lazy.force f'

    let of_unidims us =
      { f = (fun ys -> Infinite_list.map us ~f:(fun u -> Unidim.eval u ys))
      ; f' = Lazy.from_fun (fun () -> Infinite_list.map us ~f:Unidim.grad)
      }

    let of_unidims' us = of_unidims (Infinite_list.of_list us ~default:Unidim.zero)

    let of_unidim ~dim u = of_unidims' (List.init dim ~f:(Fn.const u))

    let nth_unidim t i =
      { Unidim.f = (fun ys -> Infinite_list.nth_exn (eval t ys) i)
      ; f' = Lazy.from_fun (fun () -> Infinite_list.nth_exn (jacobian t) i)
      }

    let to_unidims ~dim t = List.init dim ~f:(fun i -> nth_unidim t i)

    let c ~dim y = of_unidim ~dim (Unidim.c y)

    let zero = of_unidims' []

    let x ~dim = of_unidims' (List.init dim ~f:(fun i -> Unidim.x_i i))

    let scale t k =
      { f = (fun ys -> Infinite_list.map (eval t ys) ~f:(fun z -> Floatlike.( * ) k z))
      ; f' = Lazy.from_fun (fun () -> Infinite_list.map (jacobian t) ~f:(Infinite_list.map ~f:(fun dt -> scale dt k)))
      }

    let (+) g h =
      { f = (fun ys -> Infinite_list.map2 (eval g ys) (eval h ys) ~f:Floatlike.(+))
      ; f' = Lazy.from_fun (fun () -> Infinite_list.map2 (jacobian g) (jacobian h) ~f:(Infinite_list.map2 ~f:(+)))
      }

    let (-) g h =
      { f = (fun ys -> Infinite_list.map2 (eval g ys) (eval h ys) ~f:Floatlike.(-))
      ; f' = Lazy.from_fun (fun () -> Infinite_list.map2 (jacobian g) (jacobian h) ~f:(Infinite_list.map2 ~f:(-)))
      }

    let map ~dim t ~f =
      of_unidims' (List.map (to_unidims ~dim t) ~f)

    let map2 ~dim g h ~f =
      of_unidims' (List.map2_exn (to_unidims ~dim g) (to_unidims ~dim h) ~f)

    let compose_univar ~dim g h =
      map ~dim h ~f:(fun h_i -> Unidim.compose_univar g h_i)
  end
end

module Float = Make(Floatlike.Float)
