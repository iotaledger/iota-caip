// Copyright 2020-2025 IOTA Stiftung
// SPDX-License-Identifier: Apache-2.0

pub mod address;
pub mod network;
pub mod resource;

#[cfg(feature = "resolver")]
pub mod resolver;

pub use address::{IotaAccountId, IotaAddress};
pub use network::{IotaChainId, IotaNetwork};
pub use resource::IotaResourceLocator;
