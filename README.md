# Mimic, a full-abstract way to instantiate a transmission protocol

`mimic` is a small project which gives you the opportunity to instantiate a
transmission protocol - such as a TCP/IP connection - from dynamic values. A
simple tutorial is available [here][tutorial]. It explains to implement a
ping-pong protocol and upgrade it to TLS.

## Some examples

[git][git] or [paf][paf] are examples where they use `mimic` as the only
transmission protocol implementation available. It permits to be compatible
with MirageOS without the complexity of _functors_ (commonly used with
[functoria][functoria] to unlock the possibility to abstract anything).

## Design

`mimic` is pretty-small (~ 700 lines) and the API wants to fit into several
different contexts (HTTP, [TLS][tls] or [SSH][ssh]). It's possible to make
helpers from it such as some derivations for `unix` or `mirage` - as we
commonly designed for [conduit][conduit]. However, with a big retro-spective,
such piece of code should **not** include these derivations.

Indeed, they give an opportunity to the user to assert a non-compaibility with
MirageOS if you use the `unix` derivation for example.

`mimic` wants to be abstract and "simple". Then, the user is able to construct
something more complex and easy to use at his level - and it's what [paf][paf]
does for example or [git-unix][git-unix].

## The goal of `mimic`

In the context of MirageOS which has a first stage which decides
implementation of protocols according to arguments let us to provide a client
which can work on many contexts:
- as a simple executable which can use the host TCP/IP stack
- as a full operating system which integrate its own TCP/IP stack
- as something else which wants to use something else than the TCP/IP stack

That mostly means that, _de facto_, we can not assert a certain implementation
of the underlying transmission protocol used by a protocol such as HTTP or
SMTP. This required abstraction becomes more complexe when we start to think
about composition of protocols (such as TCP/IP and TLS for instance).

This abstraction, in the context of a client, is not only determined by a
static application of _functors_ with our implementations. It depends on an
user's input value which will choose the right transmission protocol. For
instance:
- `git@github.com:repo/name` expects TCP/IP + SSH
- `http://github.com/repo/name` expects TCP/IP + HTTP
- `git://github.com/repo/name` expects TCP/IP
- `https://github.com/repo/name` expects TCP/IP + TLS + HTTP

`mimic` gives the opportunity to provide a full implementation of the
[`Mirage_flow.S`][mirage-flow] interface and require a function to instantiate
the given transmission protocol (which respects our interface). By this way and
according to user's input values, `mimic` is able to choose an try to
instantiate a _certain_ transmission protocol and hide it into an _not-fully_
abstracted type `Mimic.flow`.

It unlock the ability to implement a protocol such as the Git protocol - or
something else such as the HTTP protocol. By this way, this implementation is,
_de facto_ compatible with MirageOS in any contexts. In the case of MirageOS,
a simple registration of available transmission protocols _via_
[functoria][functoria] is enough. For a more concrete usage such as the Unix
usage, a derivation of your protocol with `unix` and a registration by 
default of some transmission protocols is enough too. The main difference is:
- one is leaded by arguments of the user (and `functoria`)
- the second is established by the developer

### The result of the mimic's usage

More practically, in the MirageOS world, a _device_ **can not** provide _via_
its interface the `connect` function but it must implement it let write the
`functoria` glue to to let it to call the `connect` function with available
arguments (from the command-line).

For instance, a device can be described with this interface:
```ocaml
module type S = sig
  type t

  val read : t -> buffer
  val write : t -> buffer -> unit
end
```

And its implementation can be described with:
```ocaml
module TCP : sig
  include S

  val connect : ipv4 -> t
end
```

That mostly mean that, inside the `unikernel.ml` which is your application, you
don't have an access to the `connect` function:
```ocaml
module Make (My_device : Device.S) = struct
  let start (t : My_device.t) =
    ...
end
```

A _hot-connect_ can not be available into the interface for a specific reason:
the abstraction. Arguments required to `connect`/allocate a resource which
represents our device depend on the implementation. As we said earlier,
`ocaml-tls` expects a `Tls.Config.client` where `Lwt_ssl` expects an
`Ssl.context`. It can be difficult to shape these values into an ultimate type
(which is, of course, non-exhaustive from possible TLS implementations).

Mimic wants to provide this _hot-connect_ function into your application
(inside the `unikernel.ml`) without a static dependency to `ocaml-tls` or
`lwt_ssl` _à priori_. Then, the `functoria`/`mirage` tool will choose right
dependency according to the command-line invokation and produce the glue needed
to be able to _hot-connect_ a TLS connection over TCP/IP.

## Reverse dependencies

`mimic` must be thought according to who use it. The API is not designed to be
canonic and usable as is. It has been thought to unlock the full abstraction
and the compatibility with MirageOS for others projects.

If you think that you can have an usage of `mimic` and something is missing,
you should implement what you want **outside** `mimic`.

## The `Mirage_flow.S` interface

Finally, the only assumption about design of protocols, transmission protocols,
etc. is `Mirage_flow.S`. Several issues exist about this interface but the cost
to upgrade the interface (to be unix-friendly for example) is huge when
several MirageOS projects trust on this specific interface.

## Documentation

`mimic` can be hard to explain when we don't know all details about the
MirageOS eco-system. The existence of this project can be critized when we
don't really understand all details and how this project fits in.

The documentation is not very clear and does not explain the big-picture of
`mimic`. So it's a real issue and the [tutorial][tutorial] wants to fix it but
my lack of English does not help me.

[tutorial]: https://dinosaure.github.io/mimic/
[git]: https://github.com/mirage/ocaml-git
[paf]: https://github.com/mirage/paf-le-chien
[functoria]: https://github.com/mirage/mirage
[tls]: https://github.com/mirleft/ocaml-tls
[ssh]: https://github.com/mirage/awa-ssh
[git-unix]: https://github.com/mirage/ocaml-git
[mirage-flow]: https://github.com/mirage/mirage-flow
