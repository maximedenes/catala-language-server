module Position = struct
  
  include Lsp.Types.Position

  let compare pos1 pos2 =
    match Int.compare pos1.line pos2.line with
    | 0 -> Int.compare pos1.character pos2.character
    | x -> x

  let to_string pos = Format.sprintf "(%i,%i)" pos.line pos.character

end

module Range = struct

  include Lsp.Types.Range

  let of_lexpos startp endp =
    let open Lexing in
    let start = Position.{ line = startp.pos_lnum-1; character = startp.pos_cnum - startp.pos_bol; } in
    let end_ = Position.{ line = endp.pos_lnum-1; character = endp.pos_cnum - endp.pos_bol; } in
    { start; end_}

  let of_catala_pos pos =
    let open Catala_utils.Pos in
    let start = Position.{ line = get_start_line pos - 1; character = get_start_column pos - 1; } in
    let end_ = Position.{ line = get_end_line pos - 1; character = get_end_column pos - 1; } in
    { start; end_}

end 

module Location = struct

  include Lsp.Types.Location

  let of_catala_pos pos =
    let open Catala_utils.Pos in
    let uri = Lsp.Uri.of_path (get_file pos) in
    let range = Range.of_catala_pos pos in
    { uri; range }

end