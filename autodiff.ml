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
    val sin : t -> t
    val cos : t -> t
    val exp : t -> t
  end) = struct
  type t =
    { f : Floatlike.t -> Floatlike.t
    ; f' : t Lazy.t
    }

  let diff { f; f' } = Lazy.force f'

  let eval { f; f' } x = f x

  let rec const x =
    { f = Fn.const x
    ; f' = Lazy.from_fun (fun () -> const Floatlike.zero)
    }

  let id =
    { f = Fn.id
    ; f' = Lazy.from_fun (fun () -> const Floatlike.one)
    }

  let rec (+) { f = g; f' = g' } { f = h; f' = h' } =
    { f = (fun x -> Floatlike.(+) (g x) (h x))
    ; f' = Lazy.from_fun (fun () -> Lazy.force g' + Lazy.force h')
    }

  let rec (-) { f = g; f' = g' } { f = h; f' = h' } =
    { f = (fun x -> Floatlike.(-) (g x) (h x))
    ; f' = Lazy.from_fun (fun () -> Lazy.force g' - Lazy.force h')
    }

  let rec ( * ) tg th =
    let { f = g; f' = g' } = tg in
    let { f = h; f' = h' } = th in
    { f = (fun x -> Floatlike.( * ) (g x) (h x))
    ; f' = Lazy.from_fun (fun () -> tg * Lazy.force h' + Lazy.force g' * th)
    }

  let rec compose tg th =
    let { f = g; f' = g' } = tg in
    let { f = h; f' = h' } = th in
    { f = (fun x -> g (h x))
    ; f' = Lazy.from_fun (fun () -> compose (diff tg) th * diff th)
    }

  module Uncomposed = struct
    let rec scale c =
      { f = (fun x -> Floatlike.scale x c)
      ; f' = Lazy.from_fun (fun () -> compose (scale c) (const Floatlike.one))
      }

    let rec int_pow i =
      { f = (fun x -> Floatlike.int_pow x i)
      ; f' = Lazy.from_fun (fun () -> compose (scale (Float.of_int i)) (int_pow (Int.pred i)))
      }

    let rec sin () =
      { f = Floatlike.sin
      ; f' = Lazy.from_fun (fun () -> cos ()) }

    and cos () =
      { f = Floatlike.cos
      ; f' = Lazy.from_fun (fun () -> (const Floatlike.zero) - sin ()) }

    let rec exp () =
      { f = Floatlike.exp
      ; f' = Lazy.from_fun (fun () -> exp ()) }
  end

  let sin t = compose (Uncomposed.sin ()) t
  let cos t = compose (Uncomposed.cos ()) t
  let exp t = compose (Uncomposed.exp ()) t
end
