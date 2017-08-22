open Core

type 'a t =
  { dimx : int (* length of outer array *)
  ; dimy : int (* length of each inner array *)
  ; matrix : 'a Array.t Array.t
  }

let constant ~dimx ~dimy value =
  { dimx
  ; dimy
  ; matrix = Array.make_matrix ~dimx ~dimy value
  }

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

let to_matrix t = Array.to_list t.matrix |> List.map ~f:Array.to_list

let copy { dimx; dimy; matrix } =
  { dimx
  ; dimy
  ; matrix = Array.map matrix ~f:Array.copy
  }

let map { dimx; dimy; matrix } ~f =
  { dimx
  ; dimy
  ; matrix = Array.map matrix ~f:(Array.map ~f)
  }

let get_entry { dimx; dimy; matrix } ~x:i ~y:j =
  if Int.(i >= 0 && i < dimx && j >= 0 && j < dimy) then
    Some matrix.(i).(j)
  else
    None

let set_entry t ~x:i ~y:j value =
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
    let t =
      { dimx = t1.dimx + t2.dimx
      ; dimy = t1.dimy
      ; matrix = Array.append t1.matrix t2.matrix
      }
    in
    Some (copy t)
  else
    None

let append_horizontal t1 t2 =
  Option.map (append_vertical (transpose t1) (transpose t2)) ~f:transpose

let pointwise t1 t2 ~f =
  if Int.equal t1.dimx t2.dimx && Int.equal t1.dimy t2.dimy then
    let m = constant ~dimx:t1.dimx ~dimy:t1.dimy None in
    for i = 0 to Int.(t1.dimx - 1) do
      for j = 0 to Int.(t1.dimy - 1) do
        m.matrix.(i).(j) <- Some (f t1.matrix.(i).(j) t2.matrix.(i).(j))
      done
    done;
    Some (map m ~f:(fun o -> Option.value_exn o))
  else
    None

module Exn = struct
  let value_exn s = function
    | Some t -> t
    | None -> failwithf "Matrix operation %s failed due to mismatched dimensions" s ()

  let append_vertical t1 t2 =
    value_exn "append_vertical" (append_vertical t1 t2)

  let append_horizontal t1 t2 =
    value_exn "append_horizontal" (append_horizontal t1 t2)

  let pointwise t1 t2 ~f = value_exn "pointwise" (pointwise t1 t2 ~f)
end

module Numeric(Floatlike : Floatlike.For_matrix) = struct
  type nonrec t = Floatlike.t t

  let id ~dim =
    { dimx = dim
    ; dimy = dim
    ; matrix =
        Array.init dim ~f:(fun i ->
          Array.init dim ~f:(fun j ->
            if Int.equal i j then Floatlike.one else Floatlike.zero
          ))}

  let (+) = pointwise ~f:Floatlike.(+)

  let (-) = pointwise ~f:Floatlike.(-)

  let ( *. ) = pointwise ~f:Floatlike.( * )

  let ( * ) t1 t2 =
    if Int.equal t1.dimy t2.dimx then
      let matrix =
        Array.init t1.dimx ~f:(fun i ->
          Array.init t2.dimy ~f:(fun k ->
            Array.foldi t1.matrix.(i) ~init:Floatlike.zero
              ~f:(fun j acc x -> Floatlike.(acc + x * t2.matrix.(j).(k)))
          ))
      in
      Some
        { dimx = t1.dimx
        ; dimy = t2.dimy
        ; matrix
        }
    else
      None

  module Exn = struct
    let (+) t1 t2 = Exn.value_exn "(+)"((+) t1 t2)

    let (-) t1 t2 = Exn.value_exn "(-)" ((-) t1 t2)

    let ( *. ) t1 t2 = Exn.value_exn "( *. )" (( *. ) t1 t2)

    let ( * ) t1 t2 = Exn.value_exn "( * )" (( * ) t1 t2)
  end

  module Internal = struct
    let equal_entry ~robust x1 x2 =
      if robust then
        Int.equal (Floatlike.robustly_compare x1 x2) 0
      else
        Floatlike.equal x1 x2

    let equal ~robust t1 t2 =
      let are_equal = ref true in
      match
        pointwise t1 t2 ~f:(fun x1 x2 ->
          if equal_entry ~robust x1 x2 then () else are_equal := false
        )
      with
      | None -> false
      | Some _unit_matrix -> !are_equal

    let plu ~robust t =
      let swap_rows t i j =
        let row = t.matrix.(i) in
        t.matrix.(i) <- t.matrix.(j);
        t.matrix.(j) <- row;
        ()
      in
      if Int.equal t.dimx t.dimy then
        begin
          let p' = id ~dim:t.dimx in
          let t' = copy t in
          let divided_by_zero = ref false in
          for k = 0 to Int.(t.dimx - 1) do
            let index_of_max_abs =
              let init = (k, Floatlike.abs t'.matrix.(k).(k)) in
              Array.foldi t'.matrix ~init ~f:(fun i (index, max_abs) row ->
                let x = Floatlike.abs row.(k) in
                if Int.(i > k) && Floatlike.(x > max_abs) then
                  (i, x)
                else
                  (index, max_abs)
                )
              |> fst
            in
            swap_rows t' k index_of_max_abs;
            swap_rows p' k index_of_max_abs;
            if equal_entry ~robust t'.matrix.(k).(k) Floatlike.zero then
              divided_by_zero := true;
            for i = Int.(k + 1) to Int.(t.dimx - 1) do
              t'.matrix.(i).(k) <-
                Floatlike.(t'.matrix.(i).(k) / t'.matrix.(k).(k));
              for j = Int.(k + 1) to Int.(t.dimx - 1) do
                t'.matrix.(i).(j) <-
                  Floatlike.(t'.matrix.(i).(j) - t'.matrix.(i).(k) * t'.matrix.(k).(j))
              done
            done
          done;
          let l = constant ~dimx:t.dimx ~dimy:t.dimx Floatlike.zero in
          let u = constant ~dimx:t.dimx ~dimy:t.dimx Floatlike.zero in
          for i = 0 to Int.(t.dimx - 1) do
            for j = 0 to Int.(t.dimx - 1) do
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

    let solve_from_plu ~robust (p, l, u) ~vector =
      let divided_by_zero = ref false in
      let solve_l ~l ~b =
        let y = copy b in
        for i = 0 to Int.(b.dimx - 1) do
          for j = 0 to Int.(i - 1) do
            y.matrix.(i).(0) <-
              Floatlike.(y.matrix.(i).(0) - l.matrix.(i).(j) * y.matrix.(j).(0))
          done;
          if equal_entry ~robust l.matrix.(i).(i) Floatlike.zero then
            divided_by_zero := true;
          y.matrix.(i).(0) <- Floatlike.(y.matrix.(i).(0) / l.matrix.(i).(i))
        done;
        y
      in
      match (transpose p) * vector with
      | None -> None
      | Some b ->
        if Int.equal b.dimy 1 then
          let y = solve_l ~l ~b in
          let flip_across = Array.iter ~f:Array.rev_inplace in
          let flip_up = Array.rev_inplace in
          let u' = copy u in
          flip_across u'.matrix;
          flip_up u'.matrix;
          flip_up y.matrix;
          let x = solve_l ~l:u' ~b:y in
          flip_up x.matrix;
          if !divided_by_zero then None else Some x
        else
          None

    let solve ~robust t =
      match plu ~robust t with
      | None -> None
      | Some (p, l, u) -> Some (solve_from_plu ~robust (p, l, u))

    let inverse ~robust t =
      match plu ~robust t with
      | None -> None
      | Some (p, l, u) ->
        let inv' = id ~dim:t.dimx in
        let failed = ref false in
        for i = 0 to Int.(t.dimx - 1) do
          let vector =
            { dimx = t.dimx
            ; dimy = 1
            ; matrix = Array.transpose_exn [| inv'.matrix.(i) |]
            }
          in
          match solve_from_plu ~robust (p, l, u) ~vector with
          | None -> failed := true
          | Some x -> inv'.matrix.(i) <- (transpose x).matrix.(0)
        done;
        if !failed then None else Some (transpose inv')
  end

  let equal ?(robust=false) = Internal.equal ~robust

  let plu ?(robust=false) = Internal.plu ~robust

  let solve ?(robust=false) = Internal.solve ~robust

  let solve' ?(robust=false) t =
    match Internal.solve ~robust t with
    | None -> (fun ~vector:_ -> None)
    | Some f -> f

  let inverse ?(robust=false) = Internal.inverse ~robust
end
