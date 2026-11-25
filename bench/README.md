# Benchmarks

`main.roc` times `Base64.encode` and `Base64.decode` on SIZE bytes and prints
the fastest of ITERATIONS samples, in nanoseconds per call. Small sizes time
batches of calls covering 256 KiB per sample.

```sh
roc build --opt=speed --output=bench/bench bench/main.roc
./bench/bench 1048576 200
```
