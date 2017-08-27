open Core

module type S = sig
  type t
  type floatlike

  val c : floatlike -> t
  val zero : t
  val one : t
  val two : t
  val scale : t -> floatlike -> t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val int_pow : t -> int -> t
  val (/) : t -> t -> t
  val pow : t -> floatlike -> t
  val exp : t -> t
  val log : t -> t
  val ( ** ) : t -> t -> t
  val sin : t -> t
  val cos : t -> t
  val tan : t -> t
  val abs : t -> t
  val step : t -> t
  val relu : t -> t
  val softplus : t -> t
  val sigmoid : t -> t
end

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

      let sigmoid = one / (one + compose exp (scale x Floatlike.(zero - one)))
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
    { f = (fun l -> Infinite_list.nth_exn l i)
    ; f' = Lazy.from_fun (fun () -> Infinite_list.e_i i ~zero ~one)
    }

  let x_0 = x_i 0

  let x_1 = x_i 1

  let x_2 = x_i 2

  let rec scale t k =
    { f = (fun y -> Floatlike.( * ) k (eval t y))
    ; f' = Lazy.from_fun (fun () -> Infinite_list.map (grad t) ~f:(fun dt -> scale dt k))
    }
  let rec (+) g h =
    { f = (fun y -> Floatlike.(+) (eval g y) (eval h y))
    ; f' = Lazy.from_fun (fun () -> Infinite_list.map2 (grad g) (grad h) ~f:(+))
    }

  let rec (-) g h =
    { f = (fun y -> Floatlike.(-) (eval g y) (eval h y))
    ; f' = Lazy.from_fun (fun () -> Infinite_list.map2 (grad g) (grad h) ~f:(-))
    }

  let rec ( * ) g h =
    { f = (fun y -> Floatlike.( * ) (eval g y) (eval h y))
    ; f' = Lazy.from_fun (fun () -> Infinite_list.map2 (grad g) (grad h) ~f:(fun dg dh -> g * dh + dg * h))
    }

  let rec compose g h =
    { f = (fun y -> (Univar.eval g (eval h y)))
    ; f' = Lazy.from_fun (fun () -> Infinite_list.map (grad h) ~f:(fun dh -> compose (Univar.d g) h * dh))
    }

  let int_pow t n = compose (Univar.Uncomposed.int_pow n) t

  let (/) g h = g * (int_pow h (-1))

  let pow t p = compose (Univar.Uncomposed.pow p) t

  let exp t = compose Univar.Uncomposed.exp t
 
  let log t = compose Univar.Uncomposed.log t

  let ( ** ) g h = exp (h * log g)

  let sin t = compose Univar.Uncomposed.sin t

  let cos t = compose Univar.Uncomposed.cos t

  let tan t = compose Univar.Uncomposed.tan t

  let abs t = compose Univar.Uncomposed.abs t

  let step t = compose Univar.Uncomposed.step t

  let relu t = compose Univar.Uncomposed.relu t

  let softplus t = compose Univar.Uncomposed.softplus t

  let sigmoid t = compose Univar.Uncomposed.sigmoid t
end
