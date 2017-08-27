module Status : sig
  type t =
    | Converged
    | Failed
    | Iterating

  val to_string : t -> string
end

val find_root
  :  ?robust:bool
  -> ?step_size:float
  -> ?iterations:int
  -> ?init:float
  -> Autodiff.Float.Univar.t
  -> float * Status.t

val find_stationary
  :  ?robust:bool
  -> ?step_size:float
  -> ?iterations:int
  -> ?init:float Infinite_list.t
  -> dim:int
  -> Autodiff.Float.t
  -> float list * Status.t
