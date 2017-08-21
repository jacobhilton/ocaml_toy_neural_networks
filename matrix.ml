open Core

module Make(Floatlike : Floatlike.For_matrix) = struct
  type t =
    { dimx : int (* length of outer array *)
    ; dimy : int (* length of each inner array *)
    ; matrix : Floatlike.t Array.t Array.t
    }

 let constant ~dimx ~dimy value =
    { dimx
    ; dimy
    ; matrix = Array.make_matrix ~dimx ~dimy value
    }

  let id ~dim =
    { dimx = dim
    ; dimy = dim
    ; matrix =
        Array.init dim ~f:(fun i ->
          Array.init dim ~f:(fun j ->
            if Int.equal i j then Floatlike.one else Floatlike.zero
    ))}

  let of_infinite_matrix ~dimx ~dimy infinite_matrix =
    { dimx = dimx
    ; dimy = dimy
    ; matrix =
        Infinite_list.split_n infinite_matrix dimx |> fst
        |> List.map ~f:(fun l -> Infinite_list.split_n l dimy |> fst)
        |> List.map ~f:Array.of_list
        |> Array.of_list
    }

  let transpose { dimx; dimy; matrix } =
    { dimx = dimy
    ; dimy = dimx
    ; matrix = Array.transpose_exn matrix
    }

  let row_vector_of_list l =
    { dimx = 1
    ; dimy = List.length l
    ; matrix = [| Array.of_list l |]
    }

  let column_vector_of_list l = transpose (row_vector_of_list l)

  let dimx t = t.dimx

  let dimy t = t.dimy

  let to_matrix { dimx = _; dimy = _; matrix } =
    Array.to_list matrix
    |> List.map ~f:Array.to_list

  let copy { dimx; dimy; matrix } =
    { dimx
    ; dimy
    ; matrix = Array.map matrix ~f:Array.copy
    }

  let get { dimx; dimy; matrix } ~x:i ~y:j =
    if Int.(i >= 0 && i < dimx && j >= 0 && j < dimy) then
      Some matrix.(i).(j)
    else
      None

  let set t ~x:i ~y:j value =
    if Int.(i >= 0 && i < t.dimx && j >= 0 && j < t.dimy) then
      let t' = copy t in
      t'.matrix.(i).(j) <- value;
      Some t'
    else
      None

  let get_row { dimx; dimy = _; matrix } ~x:i =
    if Int.(i >= 0 && i < dimx) then
      Some (Array.to_list matrix.(i))
    else
      None

  let get_column t ~y:j = get_row (transpose t) ~x:j

  let set_row t ~x:i l =
    if Int.(i>= 0 && i < t.dimx && equal (List.length l) t.dimy) then
      let t' = copy t in
      t'.matrix.(i) <- Array.of_list l;
      Some t'
    else
      None

  let set_column t ~y:j l = Option.map (set_row (transpose t) ~x:j l) ~f:transpose

  let append_vertical t1 t2 =
    if Int.equal t1.dimy t2.dimy then
      Some
        { dimx = t1.dimx + t2.dimx
        ; dimy = t1.dimy
        ; matrix = Array.append t1.matrix t2.matrix
        }
    else
      None

  let append_horizontal t1 t2 =
    Option.map (append_vertical (transpose t1) (transpose t2)) ~f:transpose

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
          Array.init dimy2 ~f:(fun k ->
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

  let plu t =
    let swap_rows { dimx = _; dimy = _; matrix } i j =
      let row = matrix.(i) in
      matrix.(i) <- matrix.(j);
      matrix.(j) <- row;
      ()
    in
    let { dimx; dimy; matrix = _ } = t in
    let p' = id ~dim:dimx in
    if Int.equal dimx dimy then
      begin
        let t' = copy t in
        let divided_by_zero = ref false in
        for k = 0 to Int.(dimx - 1) do
          let index_of_max =
            let init = (k, Floatlike.abs t'.matrix.(k).(k)) in
            Array.foldi t'.matrix ~init ~f:(fun i (index, max) row ->
              let x = Floatlike.abs row.(k) in
              if Int.(i > k) && Floatlike.(x > max) then
                (i, x)
              else
                (index, max)
            )
            |> fst
          in
          swap_rows t' k index_of_max;
          swap_rows p' k index_of_max;
          if Floatlike.(equal t'.matrix.(k).(k) zero) then
            divided_by_zero := true;
          for i = Int.(k + 1) to Int.(dimx - 1) do
            t'.matrix.(i).(k) <-
              Floatlike.(t'.matrix.(i).(k) / t'.matrix.(k).(k));
            for j = Int.(k + 1) to Int.(dimx - 1) do
              t'.matrix.(i).(j) <-
                Floatlike.(t'.matrix.(i).(j) - t'.matrix.(i).(k) * t'.matrix.(k).(j))
            done
          done
        done;
        let l = constant ~dimx ~dimy Floatlike.zero in
        let u = constant ~dimx ~dimy Floatlike.zero in
        for i = 0 to Int.(dimx - 1) do
          for j = 0 to Int.(dimy - 1) do
            match Int.(sign (i - j)) with
            | Zero ->
              l.matrix.(i).(j) <- Floatlike.one;
              u.matrix.(i).(j) <- t'.matrix.(i).(j);
              ()
            | Pos ->
              l.matrix.(i).(j) <- t'.matrix.(i).(j);
              ()
            | Neg ->
              u.matrix.(i).(j) <- t'.matrix.(i).(j);
              ()
          done
        done; 
        if !divided_by_zero then None else Some (transpose p', l, u)
      end
    else
      None

  module Exn = struct
    let value_exn s = function
      | Some t -> t
      | None -> failwithf "Matrix operation %s failed" s ()

    let append_vertical t1 t2 =
      value_exn "append_vertical" (append_vertical t1 t2)

    let append_horizontal t1 t2 =
      value_exn "append_horizontal" (append_horizontal t1 t2)

    let pointwise t1 t2 ~f = value_exn "pointwise" (pointwise t1 t2 ~f)

    let (+) t1 t2 = value_exn "(+)"((+) t1 t2)

    let (-) t1 t2 = value_exn "(-)" ((-) t1 t2)

    let ( *. ) t1 t2 = value_exn "( *. )" (( *. ) t1 t2)

    let ( * ) t1 t2 = value_exn "( * )" (( * ) t1 t2)

    let plu t = value_exn "plu" (plu t)
  end
end
