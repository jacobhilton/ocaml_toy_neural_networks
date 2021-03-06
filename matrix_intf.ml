module type S = sig
  type 'a t

  val constant : dimx:int -> dimy:int -> 'a -> 'a t

  val of_infinite_matrix
    :  dimx:int
    -> dimy:int
    -> 'a Infinite_list.t Infinite_list.t
    -> 'a t

  val row_vector_of_list : 'a list -> 'a t

  val column_vector_of_list : 'a list -> 'a t

  val dimx : _ t -> int

  val dimy : _ t -> int

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val to_matrix : 'a t -> 'a list list

  val get_entry : 'a t -> x:int -> y:int -> 'a option

  val set_entry : 'a t -> x:int -> y:int -> 'a -> 'a t option

  val get_row : 'a t -> x:int -> 'a list option

  val get_column : 'a t -> y:int -> 'a list option

  val set_row : 'a t -> x:int -> 'a list -> 'a t option

  val set_column : 'a t -> y:int -> 'a list -> 'a t option

  val transpose : 'a t -> 'a t

  val append_vertical : 'a t -> 'a t -> 'a t option

  val append_horizontal : 'a t -> 'a t -> 'a t option

  val pointwise : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t option

  module Exn : sig
    val append_vertical : 'a t -> 'a t -> 'a t

    val append_horizontal : 'a t -> 'a t -> 'a t

    val pointwise : 'a t -> 'a t -> f:('a -> 'a -> 'a) -> 'a t
  end
end

module type Numeric = sig
  type t

  val id : dim:int -> t

  val (+) : t -> t -> t option

  val (-) : t -> t -> t option

  (** Pointwise multiplication. *)
  val ( *. ) : t -> t -> t option

  (** Matrix multiplication *)
  val ( * ) : t -> t -> t option

  val equal : ?robust:bool -> t -> t -> bool

  (** LU decomposition with pivoting to find a permutation matrix p, a lower
      triangular matrix l and an upper triangular matrix u such that p * l * u
      is equal to the original matrix *)
  val plu : ?robust:bool -> t -> (t * t * t) option

  (** Solve the linear equation Ax = b for x given a matrix A and a vector b. *)
  val solve : ?robust:bool -> t -> (vector:t -> t option) option

  val solve' : ?robust:bool -> t -> vector:t -> t option

  (** [inverse t] returns [None] if t is singlular.
      [inverse ~robust:true t] returns [None] if t is singular up to robust
      comparison tolerance. *)
  val inverse : ?robust:bool -> t -> t option

  module Exn : sig
    val (+) : t -> t -> t

    val (-) : t -> t -> t

    val ( *. ) : t -> t -> t

    val ( * ) : t -> t -> t
  end
end
