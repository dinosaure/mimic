type 'a info = { name : string; root : root }
and root = Root of int option | Value

let pp_info ppf { name; root } =
  match root with
  | Root (Some p) -> Fmt.pf ppf "<%s:%d>" name p
  | Root None -> Fmt.pf ppf "<%s>" name
  | Value -> Fmt.pf ppf "%s" name

module Mirage_protocol = Mirage_protocol
module Info = struct type 'a t = 'a info end
module Hmap0 = Hmap.Make (Info)

let pp_value ppf value = Fmt.pf ppf "%a" pp_info (Hmap0.Key.info value)
let src = Logs.Src.create "mimic" ~doc:"logs mimic's event"

module Log = (val Logs.src_log src : Logs.LOG)

module rec Fun : sig
  type ('k, 'res) args =
    | [] : ('res, 'res) args
    | ( :: ) : 'a arg * ('k, 'res) args -> ('a -> 'k, 'res) args

  and 'v arg =
    | Map : ('f, 'a) args * 'f -> 'a arg
    | Req : 'a Hmap0.key -> 'a arg
    | Opt : 'a Hmap0.key -> 'a option arg
    | Dft : 'a * 'a Hmap0.key -> 'a arg

  val req : 'a Hmap0.key -> 'a arg
  val opt : 'a Hmap0.key -> 'a option arg
  val dft : 'a Hmap0.key -> 'a -> 'a arg
  val map : ('k, 'a) args -> 'k -> 'a arg
end = struct
  type ('k, 'res) args =
    | [] : ('res, 'res) args
    | ( :: ) : 'a arg * ('k, 'res) args -> ('a -> 'k, 'res) args

  and 'v arg =
    | Map : ('f, 'a) args * 'f -> 'a arg
    | Req : 'a Hmap0.key -> 'a arg
    | Opt : 'a Hmap0.key -> 'a option arg
    | Dft : 'a * 'a Hmap0.key -> 'a arg

  let req value = Req value
  let opt value = Opt value
  let dft value v = Dft (v, value)
  let map args k = Map (args, k)
end

and Value : sig
  type 'a elt =
    | Val : 'a -> 'a elt
    | Fun : ('k, 'a option Lwt.t) Fun.args * 'k -> 'a elt

  type 'a t = 'a elt list
end = struct
  type 'a elt =
    | Val : 'a -> 'a elt
    | Fun : ('k, 'a option Lwt.t) Fun.args * 'k -> 'a elt

  type 'a t = 'a elt list
end

module Hmap = Hmap0.Make (Value)

type ctx = Hmap.t
type 'edn value = 'edn Hmap0.key

let merge ctx0 ctx1 =
  let f :
      type a.
      a value -> a Value.t option -> a Value.t option -> a Value.t option =
   fun _k lst0 lst1 ->
    match lst0, lst1 with
    | Some lst0, Some lst1 -> Some (lst0 @ lst1)
    | Some x, None | None, Some x -> Some x
    | None, None -> None
  in
  Hmap.merge { f } ctx0 ctx1

module Merge (A : sig
  val ctx : ctx
end) (B : sig
  val ctx : ctx
end) =
struct
  let ctx = merge A.ctx B.ctx
end

let add value v ctx =
  match Hmap.find value ctx with
  | Some lst -> Hmap.add value (lst @ [ Val v ]) ctx
  | None -> Hmap.add value [ Val v ] ctx

let fold value args ~k ctx =
  match Hmap.find value ctx with
  | Some lst -> Hmap.add value (lst @ [ Fun (args, k) ]) ctx
  | None -> Hmap.add value [ Fun (args, k) ] ctx

let replace value v ctx =
  match Hmap.find value ctx with
  | None -> Hmap.add value [ Val v ] ctx
  | Some lst ->
      let lst =
        List.fold_left
          (fun acc -> function
            | Value.Fun _ as v -> v :: acc
            | Value.Val _ -> acc)
          [] lst
      in
      let lst = List.rev lst in
      (* XXX(dinosaure): keep the order! *)
      Hmap.add value (Val v :: lst) ctx

(***** Mirage_flow.S part *****)

module Implicit0 = Implicit.Make (struct
  type 'flow t = (module Mirage_flow.S with type flow = 'flow)
end)

type flow = Implicit0.t = private ..
type error = [ `Msg of string | `Not_found | `Cycle ]
type write_error = [ `Msg of string | `Closed ]

let pp_error ppf = function
  | `Msg err -> Fmt.string ppf err
  | `Not_found -> Fmt.string ppf "No connection found"
  | `Cycle -> Fmt.string ppf "Context contains a cycle"

let pp_write_error ppf = function
  | `Msg err -> Fmt.string ppf err
  | `Closed -> Fmt.string ppf "Connection closed by peer"

let read flow =
  let (Implicit0.Value (flow, (module Flow))) = Implicit0.prj flow in
  let open Lwt.Infix in
  Flow.read flow
  >|= Result.map_error (fun fe -> `Msg (Fmt.to_to_string Flow.pp_error fe))

let write flow cs =
  let (Implicit0.Value (flow, (module Flow))) = Implicit0.prj flow in
  let open Lwt.Infix in
  Flow.write flow cs >|= function
  | Error `Closed -> Error `Closed
  | Error e -> Error (`Msg (Fmt.to_to_string Flow.pp_write_error e))
  | Ok _ as v -> v

let writev flow css =
  let (Implicit0.Value (flow, (module Flow))) = Implicit0.prj flow in
  let open Lwt.Infix in
  Flow.writev flow css
  >|= Result.map_error (fun fe ->
          `Msg (Fmt.to_to_string Flow.pp_write_error fe))

let close flow =
  let (Implicit0.Value (flow, (module Flow))) = Implicit0.prj flow in
  Flow.close flow

(***** Protocol (Mirage_flow.S + connect) part *****)

type ('edn, 'flow) snd = Snd : 'flow -> ('edn, 'flow) snd [@@warning "-37"]

type _ pack =
  | Protocol :
      'edn Hmap0.key
      * 'flow Implicit0.witness
      * (module Mirage_protocol.S
           with type flow = 'flow
            and type endpoint = 'edn)
      -> ('edn, 'flow) snd pack

module Implicit1 = Implicit.Make (struct type 'v t = 'v pack end)

type ('edn, 'flow) protocol = {
  flow : 'flow Implicit0.witness;
  protocol : ('edn, 'flow) snd Implicit1.witness;
}

let register :
    type edn flow.
    ?priority:int ->
    name:string ->
    (module Mirage_protocol.S with type flow = flow and type endpoint = edn) ->
    edn value * (edn, flow) protocol =
 fun ?priority ~name (module Protocol) ->
  let value = Hmap0.Key.create { name; root = Root priority } in
  let flow = Implicit0.inj (module Protocol) in
  let protocol = Implicit1.inj (Protocol (value, flow, (module Protocol))) in
  value, { flow; protocol }

module type REPR = sig
  type t type flow += (* XXX(dinosaure): private? *) T of t
end

let repr :
    type edn flow. (edn, flow) protocol -> (module REPR with type t = flow) =
 fun { flow; _ } ->
  let (module Witness) = flow in
  let module M = struct
    include Witness

    type t = a
  end in
  (module M)

let rec apply :
    type k res. ctx -> (k, res option Lwt.t) Fun.args -> k -> res option Lwt.t =
 fun ctx args f ->
  let open Lwt.Infix in
  let rec go : type k res. ctx -> (k, res) Fun.args -> k -> res Lwt.t =
   fun ctx -> function
    | [] -> fun x -> Lwt.return x
    | Map (args', f') :: tl ->
        fun f -> go ctx args' f' >>= fun v -> go ctx tl (f v)
    | Opt value :: tl -> fun f -> find value ctx >>= fun v -> go ctx tl (f v)
    | Dft (v, value) :: tl -> (
        fun f ->
          find value ctx >>= function
          | Some v' ->
              Log.debug (fun m ->
                  m "Found a value for the default argument: %a." pp_value value);
              go ctx tl (f v')
          | None -> go ctx tl (f v))
    | Req value :: tl -> (
        fun f ->
          find value ctx >>= function
          | Some v -> go ctx tl (f v)
          | None -> Lwt.fail Not_found)
  in
  Lwt.catch (fun () -> go ctx args f >>= fun fiber -> fiber) @@ function
  | Not_found -> Lwt.return_none
  | exn -> Lwt.fail exn

and find : type a. a value -> ctx -> a option Lwt.t =
 fun value ctx ->
  match Hmap.find value ctx with
  | None | Some [] -> Lwt.return_none
  | Some lst ->
      (* XXX(dinosaure): priority on values, then we apply the first [Fun] *)
      let rec go fold lst =
        match fold, lst with
        | None, [] -> Lwt.return_none
        | Some (Value.Fun (args, f)), [] -> apply ctx args f
        | Some (Value.Val _), [] -> assert false
        | None, (Value.Fun _ as x) :: r -> go (Some x) r
        | _, Val v :: _ -> Lwt.return_some v
        | Some _, Fun _ :: r -> go fold r
      in
      go None (List.rev lst)
(* XXX(dinosaure): the most recent value. *)

type edn = Edn : 'edn value * 'edn -> edn
type fnu = Fun : 'edn value * ('k, 'edn option Lwt.t) Fun.args * 'k -> fnu
type dep = Dep : 'edn value -> dep

let pp_fnu ppf (Fun (dep, _, _)) = Fmt.pf ppf "%a" pp_info (Hmap0.Key.info dep)

module Sort = struct
  type t =
    | Val : 'edn value * 'edn -> t
    | Fun : 'edn value * ('k, 'edn option Lwt.t) Fun.args * 'k -> t

  let pp ppf = function
    | Val (k, _) -> pp_info ppf (Hmap0.Key.info k)
    | Fun (k, _, _) -> pp_info ppf (Hmap0.Key.info k)
end

let partition bindings =
  let rec go leafs nodes = function
    | [] -> List.rev leafs, List.rev nodes
    | Hmap.B (_, []) :: r -> go leafs nodes r
    | Hmap.B (k, Val v :: tl) :: r ->
        go (Sort.Val (k, v) :: leafs) nodes (Hmap.B (k, tl) :: r)
    | Hmap.B (k, Fun (args, f) :: tl) :: r ->
        go leafs (Fun (k, args, f) :: nodes) (Hmap.B (k, tl) :: r)
  in
  go [] [] bindings

let exists k bindings =
  let rec go k = function
    | [] -> false
    | Hmap.B (k', _) :: r -> (
        match Hmap0.Key.proof k k' with Some _ -> true | None -> go k r)
  in
  go k bindings

let dependencies (Fun (_, args, _)) bindings =
  let rec go : type k r. _ -> (k, r) Fun.args -> _ =
   fun acc -> function
    | Fun.Req dep :: r -> go (Dep dep :: acc) r
    | Fun.Opt dep :: r when exists dep bindings -> go (Dep dep :: acc) r
    | Fun.Dft (_, dep) :: r when exists dep bindings -> go (Dep dep :: acc) r
    | _ :: r -> go acc r
    | [] -> List.rev acc
  in
  go [] args

let exists leafs (Dep k) =
  let rec go = function
    | [] -> false
    | Sort.Val (k', _) :: r -> (
        match Hmap0.Key.proof k k' with Some _ -> true | None -> go r)
    | Sort.Fun (k', _, _) :: r -> (
        match Hmap0.Key.proof k k' with Some _ -> true | None -> go r)
  in
  go leafs

let sort bindings =
  let rec go acc later todo progress =
    match todo, later with
    | [], [] -> List.rev acc
    | [], _ when progress -> go acc [] later false
    | [], later ->
        (* TODO(dinosaure): check, at least, one root in [acc]. *)
        Log.debug (fun m ->
            m "Found a solution only for: @[<hov>%a@]."
              Fmt.(Dump.list Sort.pp)
              acc);
        Log.debug (fun m ->
            m "Unsolvable values: @[<hov>%a@]." Fmt.(Dump.list pp_fnu) later);
        List.rev acc
    | (Fun (k, args, f) as x) :: xs, _ ->
        let deps = dependencies x bindings in
        let available = List.for_all (exists acc) deps in
        if available then go (Sort.Fun (k, args, f) :: acc) later xs true
        else go acc (x :: later) xs progress
  in
  let leafs, nodes = partition bindings in
  Log.debug (fun m -> m "Partition done.");
  Log.debug (fun m -> m "Nodes: @[<hov>%a@]." Fmt.(Dump.list pp_fnu) nodes);
  go leafs [] nodes false

let inf = -1 and sup = 1

let priority_compare (Edn (k0, _)) (Edn (k1, _)) =
  match (Hmap0.Key.info k0).root, (Hmap0.Key.info k1).root with
  | Root (Some p0), Root (Some p1) -> p0 - p1
  | (Root None | Value), Root (Some _) -> sup
  | Root (Some _), (Root None | Value) -> inf
  | Value, Value -> 0
  | Root None, Root None -> 0
  | Value, Root None -> sup
  | Root None, Value -> inf

let unfold : ctx -> (edn list, [> `Cycle ]) result Lwt.t =
 fun ctx ->
  let open Lwt.Infix in
  let rec go ctx acc : Sort.t list -> _ = function
    | [] ->
        (* XXX(dinosaure): here, we use a stable sort, [List.rev]
         * is needed to keep a certain topological order - see [sort].
         * [stable_sort] keeps this order too. *)
        let acc = List.stable_sort priority_compare (List.rev acc) in
        Lwt.return_ok acc
    | Sort.Val (k, v) :: r ->
        Log.debug (fun m -> m "Return a value %a." pp_value k);
        go ctx (Edn (k, v) :: acc) r
    | Sort.Fun (k, args, f) :: r -> (
        Log.debug (fun m -> m "Apply a function %a." pp_value k);
        apply ctx args f >>= function
        | Some v -> go (add k v ctx) (Edn (k, v) :: acc) r
        | None -> go ctx acc r)
  in
  let ordered_bindings = sort (Hmap.bindings ctx) in
  go ctx [] ordered_bindings

let flow_of_value :
    type edn. edn value -> edn -> (flow, [> error ]) result Lwt.t =
 fun k v ->
  let open Lwt.Infix in
  let rec go : Implicit1.pack list -> _ = function
    | [] -> Lwt.return_error `Not_found
    | Implicit1.Key (Protocol (k', (module Witness), (module Protocol))) :: r
      -> (
        match Hmap0.Key.proof k k' with
        | None -> go r
        | Some Teq -> (
            Protocol.connect v >>= function
            | Ok flow -> Lwt.return_ok (Witness.T flow)
            | Error _err -> go r))
  in
  go (Implicit1.bindings ())

type ('a, 'b) refl = Refl : ('a, 'a) refl

let equal : type a b. a value -> b value -> (a, b) refl option =
 fun a b ->
  match Hmap0.Key.proof a b with Some Teq -> Some Refl | None -> None

let rec connect : edn list -> (flow, [> error ]) result Lwt.t = function
  | [] -> Lwt.return_error `Not_found
  | Edn (k, v) :: r -> (
      let open Lwt.Infix in
      Log.debug (fun m -> m "Try to instantiate %a." pp_value k);
      flow_of_value k v >>= function
      | Ok _ as v -> Lwt.return v
      | Error _err -> connect r)

let resolve : ctx -> (flow, [> error ]) result Lwt.t =
 fun ctx ->
  let open Lwt.Infix in
  unfold ctx >>= function
  | Ok lst ->
      Log.debug (fun m ->
          m "List of endpoints: @[<hov>%a@]"
            Fmt.(Dump.list (fun ppf (Edn (k, _)) -> pp_value ppf k))
            lst);
      connect lst
  | Error _ as err -> Lwt.return err

let make ~name = Hmap0.Key.create { name; root = Value }
let empty = Hmap.empty

let get value ctx =
  match Hmap.find value ctx with
  | Some lst ->
      let rec first = function
        | [] -> None
        | Value.Val v :: _ -> Some v
        | _ :: r -> first r
      in
      first lst
  | None -> None
