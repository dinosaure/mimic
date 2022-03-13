module type S = sig
  type t
  type flow

  val happy_eyeballs : t Mimic.value

  val resolve :
    t ->
    string ->
    int list ->
    ((Ipaddr.t * int) * flow, [> `Msg of string ]) result Lwt.t
end

module Make
    (Stack : Tcpip.Stack.V4V6)
    (DNS : Dns_client_mirage.S with type Transport.stack = Stack.t)
    (Happy_eyeballs : Happy_eyeballs_mirage.S
                        with module Transport = DNS.Transport
                         and type dns = DNS.t
                         and type flow = Stack.TCP.flow) : sig
  include S with type t = Happy_eyeballs.t and type flow = Stack.TCP.flow

  val connect : t -> Mimic.ctx Lwt.t
  (** [connect_device ts dns stack] returns a {!Mimic.ctx} which contains an
      {!happy_eyeballs} value which can be used and re-used by some others
      devices which wants to resolve domain-name with the given [dns] entity.
    *)
end
