// Copyright 2020-2025 IOTA Stiftung
// SPDX-License-Identifier: Apache-2.0

/// A helper macro to emulate `try { ... }` blocks in stable Rust.
macro_rules! try_block {
  { $($expr:tt)* } => {{
    let __try_block = || { $($expr)* };
    __try_block()
  }};
}

/// A helper macro to emulate `try { ... }` blocks in stable Rust.
macro_rules! async_try_block {
  { $($expr:tt)* } => {{
    let __try_block = async || { $($expr)* };
    __try_block().await
  }};
}
