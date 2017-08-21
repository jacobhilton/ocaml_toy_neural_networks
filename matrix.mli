module Make(Floatlike : Floatlike.For_matrix) : sig
  type t

  val constant : dimx:int -> dimy:int -> Floatlike.t -> t

  val id : dim:int -> t

  val of_infinite_matrix
    :  dimx:int
    -> dimy:int
    -> Floatlike.t Infinite_list.t Infinite_list.t
    -> t

  val row_vector_of_list : Floatlike.t list -> t

  val column_vector_of_list : Floatlike.t list -> t

  val dimx : t -> int

  val dimy : t -> int

  val to_matrix : t -> Floatlike.t list list

  val get : t -> x:int -> y:int -> Floatlike.t option

  val set : t -> x:int -> y:int -> Floatlike.t -> t option

  val get_row : t -> x:int -> Floatlike.t list option

  val get_column : t -> y:int -> Floatlike.t list option

  val set_row : t -> x:int -> Floatlike.t list -> t option

  val set_column : t -> y:int -> Floatlike.t list -> t option

  val transpose : t -> t

  val append_vertical : t -> t -> t option

  val append_horizontal : t -> t -> t option

  val pointwise : t -> t -> f:(Floatlike.t -> Floatlike.t -> Floatlike.t) -> t option

  val (+) : t -> t -> t option

  val (-) : t -> t -> t option

  (** Pointwise multiplication. *)
  val ( *. ) : t -> t -> t option

  (** Matrix multiplication *)
  val ( * ) : t -> t -> t option

  (** LU decomposition with pivoting. If three matrices are returned, then their
      product should be the original matrix. *)
  val plu : t -> (t * t * t) option

  (** Solve the linear equation Ax = b for x given a matrix A and a vector b. *)
  val solve : matrix:t -> vector:t -> t option

  val solve_from_plu : t * t * t -> vector:t -> t option

  val inverse : t -> t option

  module Exn : sig
    val pointwise : t -> t -> f:(Floatlike.t -> Floatlike.t -> Floatlike.t) -> t

    val append_vertical : t -> t -> t

    val append_horizontal : t -> t -> t

    val (+) : t -> t -> t

    val (-) : t -> t -> t

    val ( *. ) : t -> t -> t

    val ( * ) : t -> t -> t
  end
end
