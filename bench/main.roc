app [main!] {
	base64: "../package/main.roc",
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
}

import pf.Monotonic
import pf.OsStr
import pf.Stderr
import pf.Stdout
import base64.Base64

## Encodes and decodes SIZE bytes and prints the fastest of ITERATIONS
## samples of each, in nanoseconds per call. See README.md to run it.
main! : List(OsStr) => Try({}, _)
main! = |os_args|
	match os_args.map(OsStr.display).map(U64.from_str) {
		[Ok(size), Ok(iterations)] if size > 0 => {
			bytes = List.from_iter((0.U64).until(size)).map(|i| (i * 31 + 7).to_u8_wrap())
			text = Base64.encode(bytes)
			if Base64.decode(text) != Ok(bytes) {
				Stderr.line!("round trip failed")?
				return Err(Exit(1))
			}
			# Each sample times enough calls to cover 256 KiB, for timer resolution
			reps = (262144 + size - 1) / size
			encode_ns = best!(iterations, reps, |{}| Base64.encode(bytes).to_utf8().len())
			decode_ns = best!(
				iterations,
				reps,
				|{}| match Base64.decode(text) {
					Ok(decoded) => decoded.len()
					Err(_) => 0
				},
			)
			Stdout.line!("${encode_ns.to_str()} ${decode_ns.to_str()}")?
			Ok({})
		}
		_ => {
			Stderr.line!("Usage: ./bench/bench SIZE ITERATIONS, with SIZE at least 1")?
			Err(Exit(1))
		}
	}

## The fastest of `iterations` samples of `reps` runs of `f`, in nanoseconds
## per run. Each run's output length is checked, so the work cannot be skipped.
best! : U64, U64, ({} -> U64) => U64
best! = |iterations, reps, f| {
	_warm1 = f({})
	_warm2 = f({})
	var $min = 0xFFFF_FFFF_FFFF_FFFF.U64
	var $sample = 0
	while $sample < iterations {
		start = Monotonic.now!()
		var $len = 0
		var $rep = 0
		while $rep < reps {
			$len = $len + f({})
			$rep = $rep + 1
		}
		ns = start.elapsed!().to_nanos() / reps
		if $len == 0 {
			crash "a run produced no output"
		}
		if ns < $min {
			$min = ns
		}
		$sample = $sample + 1
	}
	$min
}
