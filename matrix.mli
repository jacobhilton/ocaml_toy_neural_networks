module Make(Floatlike : Floatlike.For_matrix) : sig
  type t

  val constant : dimx:int -> dimy:int -> Floatlike.t -> t

  val id : dim:int -> t

  val of_infinite_matrix
    :  dim:int
    -> Floatlike.t Infinite_list.t Infinite_list.t
    -> t

  val to_matrix : t -> Floatlike.t list list

  val transpose : t -> t

  val pointwise : t -> t -> f:(Floatlike.t -> Floatlike.t -> Floatlike.t) -> t option

  val (+) : t -> t -> t option

  val (-) : t -> t -> t option

  (** Pointwise multiplication. *)
  val ( *. ) : t -> t -> t option

  (** Matrix multiplication *)
  val ( * ) : t -> t -> t option

  (** LU decomposition. *)
  val lu : t -> (t * t) option
end
