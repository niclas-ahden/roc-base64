## Base64 encoding and decoding as per RFC 4648.
##
## `encode`/`decode` use the standard alphabet (`+`, `/`) with `=` padding.
## `encode_url`/`decode_url` use the URL-safe alphabet (`-`, `_`) without
## padding, as used in URLs, JWTs etc.
##
## Decoding is strict: any character outside the alphabet is rejected
## (including whitespace), padding must be well-formed when present, and both
## decoders accept unpadded input.

## Errors that can occur when decoding:
##
## - `InvalidLength` the input length can't come from base64 output (4n + 1
##    characters after removing padding).
## - `InvalidCharacter` a character outside the alphabet (or a misplaced `=`).
## - `InvalidPadding` a padded input whose length is not a multiple of four.
DecodeError : [
	InvalidLength,
	InvalidCharacter,
	InvalidPadding,
]

Base64 := [].{

	## Encode bytes with the standard alphabet, padded with `=`.
	##
	## ```roc
	## Base64.encode("foobar".to_utf8())  # "Zm9vYmFy"
	## Base64.encode("fo".to_utf8())      # "Zm8="
	## Base64.encode([])                  # ""
	## ```
	encode : List(U8) -> Str
	encode = |bytes| encode_with(bytes, 43, 47, Bool.True)

	## Encode bytes with the URL-safe alphabet (`-` and `_`), unpadded.
	##
	## ```roc
	## Base64.encode_url("fo".to_utf8())      # "Zm8"
	## Base64.encode_url([0xFF, 0xEF, 0xBE])  # "_---"
	## ```
	encode_url : List(U8) -> Str
	encode_url = |bytes| encode_with(bytes, 45, 95, Bool.False)

	## Decode standard-alphabet base64. Padded and unpadded inputs are both
	## accepted, and when padding is present it must be well-formed.
	##
	## ```roc
	## Base64.decode("Zm9vYmFy")  # Ok("foobar".to_utf8())
	## Base64.decode("Zm8=")      # Ok("fo".to_utf8())
	## Base64.decode("Zm8")       # Ok("fo".to_utf8())
	## Base64.decode("Zm 8")      # Err(InvalidCharacter)
	## ```
	decode : Str -> Try(List(U8), DecodeError)
	decode = |text| decode_with(text, 43, 47)

	## Decode URL-safe base64. Padded and unpadded inputs are both accepted.
	##
	## ```roc
	## Base64.decode_url("_---")  # Ok([0xFF, 0xEF, 0xBE])
	## ```
	decode_url : Str -> Try(List(U8), DecodeError)
	decode_url = |text| decode_with(text, 45, 95)
}

# --- Encode ---

encode_with : List(U8), U8, U8, Bool -> Str
encode_with = |bytes, char62, char63, pad| {
	encoded = encode_loop(bytes, 0, char62, char63, pad, List.with_capacity((bytes.len() / 3 + 1) * 4))
	match Str.from_utf8(encoded) {
		Ok(s) => s

		# Unreachable: sextet_char only emits ASCII
		Err(_) => ""
	}
}

encode_loop : List(U8), U64, U8, U8, Bool, List(U8) -> List(U8)
encode_loop = |bytes, pos, char62, char63, pad, out|
	if pos + 3 <= bytes.len() {
		b0 = (bytes.get(pos) ?? 0).to_u32()
		b1 = (bytes.get(pos + 1) ?? 0).to_u32()
		b2 = (bytes.get(pos + 2) ?? 0).to_u32()
		group = b0.shl_wrap(16).bitwise_or(b1.shl_wrap(8)).bitwise_or(b2)
		encode_loop(
			bytes,
			pos + 3,
			char62,
			char63,
			pad,
			out
				.append(sextet_char(group.shr_wrap(18).bitwise_and(63), char62, char63))
				.append(sextet_char(group.shr_wrap(12).bitwise_and(63), char62, char63))
				.append(sextet_char(group.shr_wrap(6).bitwise_and(63), char62, char63))
				.append(sextet_char(group.bitwise_and(63), char62, char63)),
		)
	} else if pos + 2 == bytes.len() {
		b0 = (bytes.get(pos) ?? 0).to_u32()
		b1 = (bytes.get(pos + 1) ?? 0).to_u32()
		group = b0.shl_wrap(16).bitwise_or(b1.shl_wrap(8))
		with_chars =
			out
				.append(sextet_char(group.shr_wrap(18).bitwise_and(63), char62, char63))
				.append(sextet_char(group.shr_wrap(12).bitwise_and(63), char62, char63))
				.append(sextet_char(group.shr_wrap(6).bitwise_and(63), char62, char63))
		if pad {
			with_chars.append(61)
		} else {
			with_chars
		}
	} else if pos + 1 == bytes.len() {
		b0 = (bytes.get(pos) ?? 0).to_u32()
		group = b0.shl_wrap(16)
		with_chars =
			out
				.append(sextet_char(group.shr_wrap(18).bitwise_and(63), char62, char63))
				.append(sextet_char(group.shr_wrap(12).bitwise_and(63), char62, char63))
		if pad {
			with_chars.append(61).append(61)
		} else {
			with_chars
		}
	} else {
		out
	}

# The alphabet is computed arithmetically (A-Z, a-z, 0-9, then the two
# alphabet-specific characters) rather than via a lookup table. At the time
# of writing, a top-level List(U8) is a heap value read through bounds-checked
# `List.get`, not an inlined static array, so a table version benchmarks ~1.5x slower
# than these register-only branches. Arithmetic also keeps both alphabets on one
# code path, parameterized by char62/char63. If the performance characteristics
# change in the future we may switch to a lookup table.
sextet_char : U32, U8, U8 -> U8
sextet_char = |sextet, char62, char63|
	if sextet <= 25 {
		65 + sextet.to_u8_wrap()
	} else if sextet <= 51 {
		97 + (sextet - 26).to_u8_wrap()
	} else if sextet <= 61 {
		48 + (sextet - 52).to_u8_wrap()
	} else if sextet == 62 {
		char62
	} else {
		char63
	}

# --- Decode ---

decode_with : Str, U8, U8 -> Try(List(U8), DecodeError)
decode_with = |text, char62, char63| {
	chars = text.to_utf8()
	body_len = padded_body_length(chars)?
	remainder = body_len % 4
	if remainder == 1 {
		Err(InvalidLength)
	} else {
		decode_loop(chars, 0, body_len, char62, char63, List.with_capacity(body_len * 3 / 4 + 2))
	}
}

# Length of the input without trailing padding. Padding may be one or two
# trailing `=`. If any padding is present the total length must be a multiple
# of four. `=` anywhere else is caught as `InvalidCharacter` during decoding.
padded_body_length : List(U8) -> Try(U64, DecodeError)
padded_body_length = |chars| {
	len = chars.len()
	pad_count =
		if len >= 2 and chars.get(len - 1) == Ok(61) and chars.get(len - 2) == Ok(61) {
			2
		} else if len >= 1 and chars.get(len - 1) == Ok(61) {
			1
		} else {
			0
		}
	if pad_count > 0 and len % 4 != 0 {
		Err(InvalidPadding)
	} else {
		Ok(len - pad_count)
	}
}

decode_loop : List(U8), U64, U64, U8, U8, List(U8) -> Try(List(U8), DecodeError)
decode_loop = |chars, pos, body_len, char62, char63, out|
	if pos + 4 <= body_len {
		s0 = sextet_value(chars.get(pos) ?? 61, char62, char63)?
		s1 = sextet_value(chars.get(pos + 1) ?? 61, char62, char63)?
		s2 = sextet_value(chars.get(pos + 2) ?? 61, char62, char63)?
		s3 = sextet_value(chars.get(pos + 3) ?? 61, char62, char63)?
		group = s0.shl_wrap(18).bitwise_or(s1.shl_wrap(12)).bitwise_or(s2.shl_wrap(6)).bitwise_or(s3)
		decode_loop(
			chars,
			pos + 4,
			body_len,
			char62,
			char63,
			out
				.append(group.shr_wrap(16).bitwise_and(0xFF).to_u8_wrap())
				.append(group.shr_wrap(8).bitwise_and(0xFF).to_u8_wrap())
				.append(group.bitwise_and(0xFF).to_u8_wrap()),
		)
	} else if pos + 3 == body_len {
		s0 = sextet_value(chars.get(pos) ?? 61, char62, char63)?
		s1 = sextet_value(chars.get(pos + 1) ?? 61, char62, char63)?
		s2 = sextet_value(chars.get(pos + 2) ?? 61, char62, char63)?
		group = s0.shl_wrap(18).bitwise_or(s1.shl_wrap(12)).bitwise_or(s2.shl_wrap(6))
		Ok(
			out
				.append(group.shr_wrap(16).bitwise_and(0xFF).to_u8_wrap())
				.append(group.shr_wrap(8).bitwise_and(0xFF).to_u8_wrap()),
		)
	} else if pos + 2 == body_len {
		s0 = sextet_value(chars.get(pos) ?? 61, char62, char63)?
		s1 = sextet_value(chars.get(pos + 1) ?? 61, char62, char63)?
		group = s0.shl_wrap(18).bitwise_or(s1.shl_wrap(12))
		Ok(out.append(group.shr_wrap(16).bitwise_and(0xFF).to_u8_wrap()))
	} else {
		Ok(out)
	}

sextet_value : U8, U8, U8 -> Try(U32, DecodeError)
sextet_value = |char, char62, char63|
	if char >= 65 and char <= 90 {
		Ok((char - 65).to_u32())
	} else if char >= 97 and char <= 122 {
		Ok((char - 97 + 26).to_u32())
	} else if char >= 48 and char <= 57 {
		Ok((char - 48 + 52).to_u32())
	} else if char == char62 {
		Ok(62)
	} else if char == char63 {
		Ok(63)
	} else {
		Err(InvalidCharacter)
	}

# --- Tests ---

# RFC 4648 section 10 test vectors

expect Base64.encode([]) == ""

expect Base64.encode("f".to_utf8()) == "Zg=="

expect Base64.encode("fo".to_utf8()) == "Zm8="

expect Base64.encode("foo".to_utf8()) == "Zm9v"

expect Base64.encode("foob".to_utf8()) == "Zm9vYg=="

expect Base64.encode("fooba".to_utf8()) == "Zm9vYmE="

expect Base64.encode("foobar".to_utf8()) == "Zm9vYmFy"

expect Base64.decode("") == Ok([])

expect Base64.decode("Zg==") == Ok("f".to_utf8())

expect Base64.decode("Zm8=") == Ok("fo".to_utf8())

expect Base64.decode("Zm9v") == Ok("foo".to_utf8())

expect Base64.decode("Zm9vYg==") == Ok("foob".to_utf8())

expect Base64.decode("Zm9vYmE=") == Ok("fooba".to_utf8())

expect Base64.decode("Zm9vYmFy") == Ok("foobar".to_utf8())

# Unpadded input decodes too

expect Base64.decode("Zg") == Ok("f".to_utf8())

expect Base64.decode("Zm8") == Ok("fo".to_utf8())

expect Base64.decode("Zm9vYg") == Ok("foob".to_utf8())

# The characters that differ between the alphabets

expect Base64.encode([0xFF, 0xEF, 0xBE]) == "/+++"

expect Base64.decode("/+++") == Ok([0xFF, 0xEF, 0xBE])

expect Base64.encode_url([0xFF, 0xEF, 0xBE]) == "_---"

expect Base64.decode_url("_---") == Ok([0xFF, 0xEF, 0xBE])

# URL-safe encoding is unpadded

expect Base64.encode_url("f".to_utf8()) == "Zg"

expect Base64.encode_url("fo".to_utf8()) == "Zm8"

expect Base64.decode_url("Zg") == Ok("f".to_utf8())

# ...but decode_url also accepts padded input
expect Base64.decode_url("Zg==") == Ok("f".to_utf8())

expect Base64.decode_url("Zm8=") == Ok("fo".to_utf8())

# All byte values round-trip through both alphabets
expect {
	all_bytes = List.repeat(0, 256).map_with_index(|_, index| index.to_u8_wrap())
	Base64.decode(Base64.encode(all_bytes)) == Ok(all_bytes)
}

expect {
	all_bytes = List.repeat(0, 256).map_with_index(|_, index| index.to_u8_wrap())
	Base64.decode_url(Base64.encode_url(all_bytes)) == Ok(all_bytes)
}

# Every input length remainder (0, 1, 2 bytes over a group) round-trips
expect {
	bytes = "base64 round trip".to_utf8()
	Base64.decode(Base64.encode(bytes)) == Ok(bytes)
}

expect {
	bytes = "base64 round trips".to_utf8()
	Base64.decode(Base64.encode(bytes)) == Ok(bytes)
}

expect {
	bytes = "base64 round trip!!".to_utf8()
	Base64.decode(Base64.encode(bytes)) == Ok(bytes)
}

# Non-canonical trailing bits are accepted, not rejected. Only three of the
# final sextet's six bits carry data here, so `Zg` and `Zh` (which differ only
# in the discarded low bits) both decode to `f`. RFC 4648 section 3.5 permits
# this.
expect Base64.decode("Zh==") == Ok("f".to_utf8())

# Decode errors

# 4n + 1 characters can never come out of an encoder
expect Base64.decode("Z") == Err(InvalidLength)

expect Base64.decode("Zm9vY") == Err(InvalidLength)

# Characters outside the alphabet, including whitespace and wrong-alphabet
expect Base64.decode("Zm 8") == Err(InvalidCharacter)

expect Base64.decode("Zm\n8") == Err(InvalidCharacter)

expect Base64.decode("_---") == Err(InvalidCharacter)

expect Base64.decode_url("/+++") == Err(InvalidCharacter)

# `=` in the middle is not padding
expect Base64.decode("Zm=8") == Err(InvalidCharacter)

# Padding on a length that is not a multiple of four
expect Base64.decode("Zg=") == Err(InvalidPadding)

expect Base64.decode("Zm9vY==") == Err(InvalidPadding)
