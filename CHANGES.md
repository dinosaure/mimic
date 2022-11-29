### 0.0.6 (2022-11-29) Paris - France

* Fix typo on documentation (2fac4cc, @dinosaure)
* Add `replace` function (#14, @dinosaure)
* Update to `ocamlformat.0.23.0` (#15, @dinosaure)

### 0.0.5 (2022-03-24) Paris - France

* Add support of OCaml 5.00.0 (#10, @dinosaure)
* Add `happy-eyeballs` device for MirageOS 4 (#11, @dinosaure)
* Add `{= version}` constraint on `mimic-happy-eyeballs` (@hannesm, #12)

### 0.0.4 (2021-08-12) Paris - France

- Use `Cstruct.length` instead of `Cstruct.len` (@dinosaure, #2)
- Remove unnucessary `bigarray-compat` dependency (@hannesm, #3)
- Remove `rresult` (@hannesm, #4)
- Be able to introspect values produced by mimic (@dinosaure, #5)
- Improve documentation (@dinosaure, #6 & #7)

### 0.0.3 (2021-20-04) Paris - France

- Move the project to https://github.com/dinosaure/mimic (@dinosaure)
  Old distributions of `mimic` are still available on
  https://github.com/mirage/ocaml-git but `mimic` starts to be
  used by others projects than `ocaml-git`. We decided to make
  its own repository.
- Take the most recent value in the `ctx` instead of the older one
  **breaking changes**
  When `mimic` wants to instantiate a transmission protocol, if
  a value `'a Mimic.value` was inserted multiple times, `mimic`
  took the older one to instance the transmission protocol.

  Now, `mimic` takes the newer one. It useful when we want to
  implement the rediction in HTTP where we need to "replace" values
  by the new destination.

### 0.0.2 (2021-31-03) Paris - France

- Add documentation (#494, @dinosaure)
- Optimize projection of modules (#495, @dinosaure)
- Remove `hmap` dependency (which is vendored and tweaked)
  (fe55e14, @dinosaure)

### 0.0.1 (2021-08-01) Paris - France

- First release of `mimic`
