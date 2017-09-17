module Status : sig
  type t =
    | Converged
    | Failed
    | Iterating
  [@@ deriving sexp]

  val to_string : t -> string
end

(* Newton-Raphson *)
val find_root
  :  ?robust:bool
  -> ?step_size:float
  -> ?iterations:int
  -> ?init:float
  -> Autodiff.Float.Univar.t
  -> float * Status.t

(* Newton's method *)
val find_stationary
  :  ?robust:bool
  -> ?step_size:float
  -> ?iterations:int
  -> ?init:float Infinite_list.t
  -> dim:int
  -> Autodiff.Float.t
  -> float list * Status.t

(* Gradient descent *)
val find_minimum
  :  ?robust:bool
  -> step_size:float
  -> ?iterations:int
  -> ?init:float Infinite_list.t
  -> dim:int
  -> Autodiff.Float.t
  -> float list * Status.t
