module Flow = Flow
module Protocol = Protocol

module Functor = struct
  type +'a t = 'a

  let bind x f = f x
  let return = Fun.id
  let protect ~finally fn = try fn () with exn -> finally exn
  let raise = raise
end

include Core.Make (Functor)

let register :
    type edn flow.
    ?priority:int ->
    name:string ->
    (module Protocol.S with type flow = flow and type endpoint = edn) ->
    edn value * (edn, flow) protocol =
 fun ?priority ~name (module Protocol) ->
  let module P = struct
    type +'a io = 'a

    include Protocol
  end in
  register ?priority ~name (module P)
