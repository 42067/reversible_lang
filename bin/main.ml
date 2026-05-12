let read_program path = open_in path |> In_channel.input_all |> Lexer.parse

let () =
  let alpha =
    let gen = Util.create () in
    Alpha.create gen
  in
  match read_program Sys.argv.(1) with
  | Error e -> Format.printf "%s\n" e
  | Ok p ->
      let ts, t = Surface.cvt_program alpha p in
      let gen, map = Alpha.destruct alpha in
      let ts = Surface.alpha_typedefs ~map:(Alpha.convert map) gen ts in
      let phi, delta = Inference.init_ctx ts in
      let t = Surface.expand_term gen t in
      let alpha = Alpha.create_u gen in
      let t = Terms.alpha_term alpha t in
      let gen, map' = Alpha.destruct_u alpha in
      Alpha.compose_map map map';
      let map = Alpha.convert map in
      let a = Inference.auto ~map gen phi delta t in
      let v = Eval.eval_term t in
      Format.printf "\n\x1b[1m%a\x1b[0m\n- : \x1b[35m%a\x1b[0m\n"
        (Terms.pp_value map) v
        (Types.pp_base
           (Util.union ~weak:map ~strong:(Types.create_map_base [ a ])))
        a
