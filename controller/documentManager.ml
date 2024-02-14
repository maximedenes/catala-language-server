type state = {
  text : string;
}

let init ~text =
  { text }

let get_text st = st.text