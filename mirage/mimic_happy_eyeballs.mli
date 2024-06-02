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
  (** [resolve happy_eyeballs domain_name ports] tries to connect the user with
      the given endpoint. The {i domain-name} can be an IP address or a
      {i real} domain-name. [resolve] tries to resolve {i via} a DNS request
      the given [domain_name] if it's a real domain-name and it tries to
      initiate a TCP/IP connection with the destination.

      If it's a success, it returns the resource and the user is able to
      [read] or [write] {i via} this resource. *)
end

(** The functor used by the MirageOS to prepare the {!Mimic.ctx} from an
    already allocated [happy_eyeballs] resource which is able to allocate a
    TCP/IP connection from a destination regardless the target chosen by the
    user. *)
module Make
    (Stack : Tcpip.Stack.V4V6)
    (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Stack.TCP.flow) : sig
  include S with type t = Happy_eyeballs.t and type flow = Stack.TCP.flow

  val connect : t -> Mimic.ctx Lwt.t
  (** [connect happy_eyeballs] returns a {!Mimic.ctx} which contains an
      {!happy_eyeballs} value which can be used and re-used by some others
      devices which want to resolve a domain-name.

      More concretely, the user is able to describe a sub-process to allocate
      a {!type:flow} from some Mimic's values:
      {[
        (* main.ml, generated by the mirage tool *)

        include Make (Stack) (Dns) (Happy_eyeballs)
        let domain_name : string Mimic.value =
          Mimic.make ~name:"domain-name"

        let ctx happy_eyeballs_v =
          let open Lwt.Infix in
          let k0 happy_eyeballs domain_name =
            resolve happy_eyeballs domain_name [ 80 ] >>= function
            | Ok (_, flow) -> Lwt.return_some flow
            | Error _ -> Lwt.return_none in
          connect happy_eyeballs_v >|=
          Mimic.fold edn Mimic.Fun.[ req happy_eyeballs; req domain_name ]
            ~k:k0

        (* unikernel.ml *)

        let run ~ctx = Mimic.resolve ctx >>= function
          | Ok flow -> (* ... *)
          | Error (`Msg err) -> failwith err
      ]}
    *)
end