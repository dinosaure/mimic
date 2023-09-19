module type S = sig
  include Flow.S

  type endpoint

  val connect : endpoint -> (flow, write_error) result
end
