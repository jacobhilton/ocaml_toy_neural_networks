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
  end) = struct
  type t =
    { f : Floatlike.t -> Floatlike.t
    ; f' : t Lazy.t
    }

  let eval { f; f' = _ } x = f x

  let diff { f = _; f' } = Lazy.force f'

  let rec const x =
    { f = Fn.const x
    ; f' = Lazy.from_fun (fun () -> const Floatlike.zero)
    }

  let id =
    { f = Fn.id
    ; f' = Lazy.from_fun (fun () -> const Floatlike.one)
    }

  let rec (+) g h =
    { f = (fun x -> Floatlike.(+) (eval g x) (eval h x))
    ; f' = Lazy.from_fun (fun () -> diff g + diff h)
    }

  let rec (-) g h =
    { f = (fun x -> Floatlike.(-) (eval g x) (eval h x))
    ; f' = Lazy.from_fun (fun () -> diff g - diff h)
    }

  let rec ( * ) g h =
    { f = (fun x -> Floatlike.( * ) (eval g x) (eval h x))
    ; f' = Lazy.from_fun (fun () -> g * diff h + diff g * h)
    }

  let rec compose g h =
    { f = (fun x -> eval g (eval h x))
    ; f' = Lazy.from_fun (fun () -> compose (diff g) h * diff h)
    }

  module Uncomposed = struct
    let scale c =
      { f = (fun x -> Floatlike.scale x c)
      ; f' = Lazy.from_fun (fun () -> (const Floatlike.(scale one c)))
      }

    let rec int_pow i =
      { f = (fun x -> Floatlike.int_pow x i)
      ; f' = Lazy.from_fun (fun () -> compose (scale (Float.of_int i)) (int_pow (Int.pred i)))
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
      ; f' = Lazy.from_fun (fun () -> (const Floatlike.zero) - sin ()) }
  end

  let scale t c = compose (Uncomposed.scale c) t

  let int_pow t i = compose (Uncomposed.int_pow i) t

  let (/) g h = g * (int_pow h (-1))

  let exp t = compose (Uncomposed.exp ()) t

  let log t = compose Uncomposed.log t

  let ( ** ) g h = exp (h * log g)

  let sin t = compose (Uncomposed.sin ()) t

  let cos t = compose (Uncomposed.cos ()) t
end
