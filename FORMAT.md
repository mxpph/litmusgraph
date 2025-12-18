## Concurrent execution format

A concurrent execution consists of multiple threads. Each thread is headed by a
`[threadIdentifier]`, followed by events, until the next `[threadIdentifier]` is encountered or the
end of input.

Identifiers consist of alphanumeric characters and must begin with a lowercase letter.

Events can be of the following format:

- `a := x // 0`: read 0 from `x`, into `a`
- `x := 1`: write 1 to `x`
- `sfence`/`mfence`: fence instructions
- `a := CAS(x, 0, 1)`: atomic compare-and-swap on `x`, saving the result in `a`
- `a := FAA(x, 2)`: atomic fetch-and-add on `x`, saving the result in `a`
- `a := FAI(x)`, atomic fetch-and-increment on `x`, shorthand for `FAA(x, 1)`

Locations beginning with letters `a` through `m` are considered thread-local, and `n` through `z`
are considered shared memory.
