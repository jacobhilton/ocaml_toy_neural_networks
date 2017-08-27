module Make (Floatlike : Floatlike.For_autodiff) : sig
  include Autodiff_intf.S with type floatlike := Floatlike.t
end

module Float : sig
  include Autodiff_intf.S with type floatlike := float
end
