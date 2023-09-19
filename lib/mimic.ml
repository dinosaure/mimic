module Mirage_protocol = Mirage_protocol

module Functor = struct
  type +'a t = 'a Lwt.t

  let bind = Lwt.bind
  let return = Lwt.return
  let protect ~finally fn = Lwt.catch fn finally
  let raise = Lwt.fail
end

include Core.Make (Functor)

let read flow =
  let open Lwt_result.Infix in
  read flow >|= function `Data _ as v -> v | `End_of_input -> `Eof

let register :
    type edn flow.
    ?priority:int ->
    name:string ->
    (module Mirage_protocol.S with type flow = flow and type endpoint = edn) ->
    edn value * (edn, flow) protocol =
 fun ?priority ~name (module Protocol) ->
  let module P = struct
    type +'a io = 'a Lwt.t

    include Protocol

    let read flow =
      let open Lwt_result.Infix in
      read flow >|= function `Data _ as v -> v | `Eof -> `End_of_input
  end in
  register ?priority ~name (module P)
