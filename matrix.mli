include Matrix_intf.S

module Numeric(Floatlike : Floatlike.For_matrix) : sig
  type nonrec t = Floatlike.t t

  include Matrix_intf.Numeric with type t := t
end

module Float : sig
  type nonrec t = float t

  include Matrix_intf.Numeric with type t:= t
end
