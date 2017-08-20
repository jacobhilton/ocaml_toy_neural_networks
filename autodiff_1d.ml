open Core

module Make (Floatlike : sig
    type t
    val zero : t
    val one : t
    val (+) : t -> t -> t
    val (-) : t -> t -> t
    val ( * ) : t -> t -> t
    val scale : t -> float -> t
    val int_pow : t -> int -> t
    val exp : t -> t
    val log : t -> t
    val sin : t -> t
    val cos : t -> t
    val abs : t -> t
  end) = struct
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
    let scale k =
      { f = (fun y -> Floatlike.scale y k)
      ; f' = Lazy.from_fun (fun () -> (c Floatlike.(scale one k)))
      }

    let rec int_pow n =
      { f = (fun y -> Floatlike.int_pow y n)
      ; f' = Lazy.from_fun (fun () -> compose (scale (Float.of_int n)) (int_pow (Int.pred n)))
      }

    let rec exp () =
      { f = Floatlike.exp
      ; f' = Lazy.from_fun (fun () -> exp ()) }

    let log =
      { f = Floatlike.log
      ; f' = Lazy.from_fun (fun () -> int_pow (-1))
      }
    let rec sin () =
      { f = Floatlike.sin
      ; f' = Lazy.from_fun (fun () -> cos ()) }

    and cos () =
      { f = Floatlike.cos
      ; f' = Lazy.from_fun (fun () -> zero - sin ()) }

    let rec abs () =
      { f = Floatlike.abs
      ; f' = Lazy.from_fun (fun () -> abs () * int_pow (-1))
      }
  end

  let scale t k = compose (Uncomposed.scale k) t

  let int_pow t n = compose (Uncomposed.int_pow n) t

  let (/) g h = g * (int_pow h (-1))

  let exp t = compose (Uncomposed.exp ()) t

  let log t = compose Uncomposed.log t

  let ( ** ) g h = exp (h * log g)

  let sin t = compose (Uncomposed.sin ()) t

  let cos t = compose (Uncomposed.cos ()) t

  let tan t = sin t / cos t

  let abs t = compose (Uncomposed.abs ()) t

  let step t = ((abs t) + t) / (two * t)

  let relu t = ((abs t) + t) / two

  let softplus t = log (one + exp t)

  let sigmoid t = one / (one + exp (zero - t))
end
