module Mirage_protocol = Mirage_protocol

type flow = private ..
(** The type for flows. A flow represents the state of a single reliable stream
    stream that is connected to an {i endpoint}. *)

include
  Mirage_flow.S
    with type flow := flow
     and type error = [ `Msg of string | `Not_found | `Cycle ]

type ctx
(** The type for contexts. It's a {i heterogeneous map} of values to help mimic
    to instantiate a new {!type:flow} {i via} {!val:resolve}. *)

type 'edn value
(** The type for {i witnesses} whose lookup value is of type ['edn]. *)

module Fun : sig
  type ('k, 'res) args =
    | [] : ('res, 'res) args
    | ( :: ) : 'a arg * ('k, 'res) args -> ('a -> 'k, 'res) args

  and 'v arg

  val req : 'a value -> 'a arg
  val opt : 'a value -> 'a option arg
  val dft : 'a value -> 'a -> 'a arg
  val map : ('k, 'a) args -> 'k -> 'a arg
end

val make : name:string -> 'edn value
(** [make ~name] is a new witness. *)

val add : 'edn value -> 'edn -> ctx -> ctx
(** [add w v ctx] is [ctx] with [w] bound to [v]. *)

val get : 'edn value -> ctx -> 'edn option
(** [get w ctx] is the value of [w]'s binding in [ctx], if any. *)

val fold : 'edn value -> ('k, 'edn option Lwt.t) Fun.args -> k:'k -> ctx -> ctx
val merge : ctx -> ctx -> ctx

val empty : ctx
(** [empty] is the empty context. *)

type ('edn, 'flow) protocol

val register :
  ?priority:int ->
  name:string ->
  (module Mirage_protocol.S with type flow = 'flow and type endpoint = 'edn) ->
  'edn value * ('edn, 'flow) protocol
(** [register ?priority ~name (module Protocol)] registers the given [Protocol]
    into the internal global Mimic's state as a possible transmission protocol
    available {i via} {!val:resolve}.

    [?priority] is used to help mimic to choose between multiple solutions
    according to the given context. Mimic will choose the lower-priority
    solution.

    [name] helps the end-user to know which solution mimic will dynamically
    {i via} log outputs.

    [register] returns 2 values:
    - a {i witness} as the required value to initiate a transmission {i via}
      the given [Protocol] implementation
    - a {!type:protocol} which can help the end-user to destruct a {!type:flow}
      to its structural type {i via} {!val:repr}. *)

module type REPR = sig
  type t type flow += (* XXX(dinosaure): private? *) T of t
end

val repr : ('edn, 'flow) protocol -> (module REPR with type t = 'flow)
(** [repr protocol] gives a module definition with an OCaml constructor to help
    the end-user to destruct the structural type of a given {!type:flow}:

    {[
      module Protocol
        : Mirage_protocol.S with type flow = Lwt_unix.file_descr

      let edn, protocol = Mimic.register ~name:"protocol" (module Protocol)
      module R = (val (Mimic.repr protocol))

      let () = Mimic.resolve ~ctx >>= function
        | Ok (R.T lwt_unix_file_descr) -> ...
        | ...
    ]} *)

val resolve : ctx -> (flow, [> error ]) result Lwt.t
(** [resolve ctx] tries to instantiate a {!type:flow} from the given [ctx]. *)

type edn =
  | Edn : 'edn value * 'edn -> edn  (** The type of a value and its witness. *)

type (_, _) refl = Refl : ('a, 'a) refl

val equal : 'a value -> 'b value -> ('a, 'b) refl option
(** [equal a b] returns a proof that [a] and [b] are
    {i structurally} equal. *)

val unfold : ctx -> (edn list, [> `Cycle ]) result Lwt.t
(** [unfold ctx] applies any functions available into the given [ctx] and
    and possible to compute according to available values and return a list
    of what these functions return.

    It's useful to do an introspection of what [mimic] does when it
    {!val:resolve}s the given [ctx]. From that and {!val:equal}, the user is
    able to introspect what [mimic] generated and which protocol it is able
    to instantiate then.

    {val:resolve} is:
    {[
      let resolve ctx =
        unfold ctx >>= function
        | Ok lst -> connect lst
        | Error _ as err -> Lwt.return err
    ]} *)

val connect : edn list -> (flow, [> error ]) result Lwt.t
(** [connect values] tries to instantiate a {!type:flow} from given [values]
    and registered protocols (see {!val:register}). *)

module Merge (A : sig
  val ctx : ctx
end) (B : sig
  val ctx : ctx
end) : sig
  val ctx : ctx
end
