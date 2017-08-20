open Core

module Make(Floatlike : Floatlike.Basic) = struct
  type t =
    { dimx : int (* length of outer array *)
    ; dimy : int (* length of each inner array *)
    ; matrix : Floatlike.t Array.t Array.t
    }

  let constant ~dimx ~dimy x =
    { dimx
    ; dimy
    ; matrix = Array.make_matrix ~dimx ~dimy x
    }

  let id ~dim =
    { dimx = dim
    ; dimy = dim
    ; matrix =
        Array.init dim ~f:(fun i ->
          Array.init dim ~f:(fun j ->
            if Int.equal i j then Floatlike.one else Floatlike.zero
    ))}

  let of_infinite_matrix ~dim infinite_matrix =
    { dimx = dim
    ; dimy = dim
    ; matrix =
        Infinite_list.split_n infinite_matrix dim
        |> fst
        |> List.map ~f:(fun l -> Infinite_list.split_n l dim |> fst)
        |> List.map ~f:Array.of_list
        |> Array.of_list
    }

  let to_matrix { dimx = _; dimy = _; matrix } =
    Array.to_list matrix
    |> List.map ~f:Array.to_list

  let copy { dimx; dimy; matrix } =
    { dimx
    ; dimy
    ; matrix = Array.map matrix ~f:Array.copy
    }

  let transpose { dimx; dimy; matrix } =
    { dimx
    ; dimy
    ; matrix = Array.transpose_exn matrix
    }

  let pointwise
      { dimx = dimx1; dimy = dimy1; matrix = m1 }
      { dimx = dimx2; dimy = dimy2; matrix = m2 }
      ~f =
    if Int.equal dimx1 dimx2 && Int.equal dimy1 dimy2 then
      let m = constant ~dimx:dimx1 ~dimy:dimy1 Floatlike.zero in
      for i = 0 to Int.(dimx1 - 1) do
        for j = 0 to Int.(dimy1 - 1) do
          m.matrix.(i).(j) <- f m1.(i).(j) m2.(i).(j)
        done
      done;
      Some m
    else
      None

  let (+) = pointwise ~f:Floatlike.(+)

  let (-) = pointwise ~f:Floatlike.(-)

  let ( *. ) = pointwise ~f:Floatlike.( * )

  let ( * )
      { dimx = dimx1; dimy = dimy1; matrix = m1 }
      { dimx = dimx2; dimy = dimy2; matrix = m2 } =
    if Int.equal dimy1 dimx2 then
      let m =
        Array.init dimx1 ~f:(fun i ->
          Array.init dimx2 ~f:(fun k ->
            Array.foldi m1.(i) ~init:Floatlike.zero
              ~f:(fun j acc x -> Floatlike.(acc + x * m2.(j).(k)))
        ))
      in
      Some
        { dimx = dimx1
        ; dimy = dimy2
        ; matrix = m
        }
    else
      None

  let lu t =
    let { dimx; dimy; matrix = _ } = t in
    if Int.equal dimx dimy then
      begin
        let t' = copy t in
        let l = constant ~dimx ~dimy Floatlike.zero in
        let u = constant ~dimx ~dimy Floatlike.zero in
        let divided_by_zero = ref false in
        for k = 0 to Int.(dimx - 1) do
          for j = k to Int.(dimx - 1) do
            u.matrix.(k).(j) <- t'.matrix.(k).(j)
          done;
          for i = k to Int.(dimx - 1) do
            if Floatlike.(equal t'.matrix.(k).(k) zero) then
              divided_by_zero := true;
            l.matrix.(i).(k) <-
              Floatlike.(t'.matrix.(i).(k) / t'.matrix.(k).(k))
          done;
          for i = k to Int.(dimx - 1) do
            for j = k to Int.(dimx - 1) do
              t'.matrix.(i).(j) <-
                Floatlike.(t'.matrix.(i).(j) - l.matrix.(i).(k) * u.matrix.(k).(j))
            done
          done
        done;
        if !divided_by_zero then None else Some (l, u)
      end
    else
      None
end
