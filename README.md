# LitmusGraph

Generate execution graphs for concurrent executions described in the following format:

```
[thread1]
x := 1
sfence
a := y // 0

[thread2]
y := 1
b := CAS(x, 0, 2) // 0
```

for more information see [`FORMAT.md`](./FORMAT.md)

### Usage

Install [Scala](https://www.scala-lang.org/download/) and `sbt`.

You can compile code with `sbt compile` and run it with `sbt "run filename"`.
