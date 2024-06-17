module type S = sig
  type t
  type flow

  val happy_eyeballs : t Mimic.value

  val resolve :
    t ->
    ?aaaa_timeout:int64 ->
    ?connect_delay:int64 ->
    ?connect_timeout:int64 ->
    ?resolve_timeout:int64 ->
    ?resolve_retries:int ->
    string ->
    int list ->
    ((Ipaddr.t * int) * flow, [> `Msg of string ]) result Lwt.t
end

module Make
    (Stack : Tcpip.Stack.V4V6)
    (Happy_eyeballs : Happy_eyeballs_mirage.S
                        with type flow = Stack.TCP.flow
                         and type stack = Stack.t)
    (_ : Dns_client_mirage.S
           with type happy_eyeballs = Happy_eyeballs.t
            and type Transport.stack = Stack.t * Happy_eyeballs.t) : sig
  include S with type t = Happy_eyeballs.t and type flow = Stack.TCP.flow

  val connect : Happy_eyeballs.t -> Mimic.ctx Lwt.t
end = struct
  type t = Happy_eyeballs.t
  type flow = Stack.TCP.flow

  let happy_eyeballs = Mimic.make ~name:"mimic-happy-eyeballs"
  let resolve = Happy_eyeballs.connect
  let connect he = Lwt.return (Mimic.add happy_eyeballs he Mimic.empty)
end
