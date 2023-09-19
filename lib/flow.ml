type 'a or_eoi = [ `Data of 'a | `End_of_input ]

module type S = sig
  type flow
  type error

  val pp_error : error Fmt.t

  type write_error = private [> `Closed ]

  val pp_write_error : write_error Fmt.t
  val read : flow -> (Cstruct.t or_eoi, error) result
  val write : flow -> Cstruct.t -> (unit, write_error) result
  val writev : flow -> Cstruct.t list -> (unit, write_error) result
  val close : flow -> unit
end
