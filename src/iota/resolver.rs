// Copyright 2020-2025 IOTA Stiftung
// SPDX-License-Identifier: Apache-2.0

use super::IotaResourceLocator;
use crate::iota::IotaNetwork;

use reqwest::Client;
use serde_json::Value;
use std::error::Error as StdError;
use std::fmt::Display;

const MAINNET_RPC_ENDPOINT: &str = "https://api.mainnet.iota.cafe";
const TESTNET_RPC_ENDPOINT: &str = "https://api.testnet.iota.cafe";
const DEVNET_RPC_ENDPOINT: &str = "https://api.devnet.iota.cafe";

/// A resolver for IOTA-based Chain Agnostic Resource Locators.
#[derive(Default)]
pub struct Resolver {
  custom_networks: Vec<(IotaNetwork, String)>,
}

impl Resolver {
  /// Returns a new [Resolver] capable of dereferencing [IotaResourceLocator]s
  /// that reference resources on any official IOTA networks (mainnet, testnet, devnet).
  pub fn new() -> Self {
    Self::default()
  }

  /// Returns a new [Resolver] capable of dereferencing [IotaResourceLocator]s
  /// that reference resources on any official IOTA networks and any other custom network
  /// specified through the argument `custom_networks`.
  ///
  /// `custom_networks` must be an iterable collection of `(IotaNetwork, String)`, where the first
  /// value must be a custom IOTA network (will be ignored otherwise), and the second value of the
  /// tuple must be the JSON RPC endpoint for that network.
  pub fn new_with_custom_networks(
    custom_networks: impl IntoIterator<Item = (IotaNetwork, String)>,
  ) -> Self {
    Self {
      custom_networks: custom_networks
        .into_iter()
        .filter(|(network, _)| network.is_custom())
        .collect(),
    }
  }

  /// Resolves the resource referenced by the given [IotaResourceLocator].
  pub async fn resolve(&self, resource: impl AsRef<str>) -> Result<Value, ResolutionError> {
    // Parse the resource locator.
    async_try_block! {
      let resource = resource
        .as_ref()
        .parse::<IotaResourceLocator>()?;

      // Get hold of an IOTA client for the network referenced in the queried resource.
      let endpoint = self
        .get_endpoint_for_network(&resource.network)
        .await?;

      // Query the object referenced in the resource.
      fetch_resource(endpoint, &resource).await
    }
    .map_err(|kind| ResolutionError {
      resource: resource.as_ref().to_string(),
      kind,
    })
  }

  /// Returns an IOTA client for the given network.
  async fn get_endpoint_for_network(
    &self,
    network: &IotaNetwork,
  ) -> Result<&str, ResolutionErrorKind> {
    match network {
      &IotaNetwork::Mainnet => Ok(MAINNET_RPC_ENDPOINT),
      &IotaNetwork::Testnet => Ok(TESTNET_RPC_ENDPOINT),
      &IotaNetwork::Devnet => Ok(DEVNET_RPC_ENDPOINT),
      custom => self
        .custom_networks
        .iter()
        .find_map(|(net, endpoint)| (custom == net).then_some(endpoint.as_str()))
        .ok_or_else(|| ResolutionErrorKind::UnknownNetwork(custom.clone())),
    }
  }
}

async fn fetch_resource(
  endpoint: &str,
  irl: &IotaResourceLocator,
) -> Result<Value, ResolutionErrorKind> {
  let req = make_rpc_request(&irl.object_id.to_string());
  let mut res = Client::new()
    .post(endpoint)
    .json(&req)
    .send()
    .await
    .map_err(|e| ResolutionErrorKind::ConnectionIssue {
      network: irl.network.clone(),
      source: e.into(),
    })?
    .json::<Value>()
    .await
    .map_err(|e| ResolutionErrorKind::QueryError(e.into()))?;

  let mut res = res
    .get_mut("result")
    .map(std::mem::take)
    .unwrap_or_default();

  if let Some(err) = res.get("error") {
    return Err(handle_response_error(err));
  }

  let pointer = format!("/data/content/fields/{}", irl.relative_url.path());
  let target = res
    .pointer_mut(pointer.as_str().trim_end_matches('/'))
    .ok_or(ResolutionErrorKind::NotFound)?;

  Ok(std::mem::take(target))
}

fn make_rpc_request(object_id: &str) -> Value {
  serde_json::json!({
    "id": 1,
    "jsonrpc": "2.0",
    "method": "iota_getObject",
    "params": [
      object_id,
      {
        "showContent": true
      }
    ]
  })
}

fn handle_response_error(err: &Value) -> ResolutionErrorKind {
  match err.get("code") {
    Some(Value::String(code)) if code == "notExists" || code == "deleted" => {
      ResolutionErrorKind::NotFound
    }
    _ => ResolutionErrorKind::QueryError("unknown error in response".into()),
  }
}

/// A failure in the resolution of a given IOTA resource.
#[derive(Debug)]
#[non_exhaustive]
pub struct ResolutionError {
  /// The resource's location that was being resolved.
  pub resource: String,
  /// The type of failure.
  pub kind: ResolutionErrorKind,
}

impl Display for ResolutionError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "failed to resolve `{}`", self.resource)
  }
}

impl StdError for ResolutionError {
  fn source(&self) -> Option<&(dyn StdError + 'static)> {
    Some(&self.kind)
  }
}

/// Types of resolution failures.
#[derive(Debug)]
#[non_exhaustive]
pub enum ResolutionErrorKind {
  /// The requested resource couldn't be found.
  NotFound,
  /// The requested resource is located on an unknown network.
  UnknownNetwork(IotaNetwork),
  /// Failed to establish a connection with the network the
  /// requested resource is located on.
  #[non_exhaustive]
  ConnectionIssue {
    network: IotaNetwork,
    source: Box<dyn StdError + Send + Sync>,
  },
  /// The underlying query failed.
  QueryError(Box<dyn StdError + Send + Sync>),
  /// Invalid IOTA Resource Locator.
  InvalidIrl(super::resource::IrlParsingError),
}

impl Display for ResolutionErrorKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::NotFound => f.write_str("not found"),
      Self::UnknownNetwork(network) => write!(f, "unknown network `iota:{network}`"),
      Self::ConnectionIssue { network, .. } => {
        write!(f, "failed to connect to `iota:{network}`")
      }
      Self::QueryError(e) => write!(f, "{e}"),
      Self::InvalidIrl(e) => write!(f, "{e}"),
    }
  }
}

impl StdError for ResolutionErrorKind {
  fn source(&self) -> Option<&(dyn StdError + 'static)> {
    match &self {
      Self::ConnectionIssue { source, .. } => Some(source.as_ref()),
      Self::QueryError(e) => e.source(),
      Self::InvalidIrl(e) => e.source(),
      _ => None,
    }
  }
}

impl From<super::resource::IrlParsingError> for ResolutionErrorKind {
  fn from(value: super::resource::IrlParsingError) -> Self {
    Self::InvalidIrl(value)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[tokio::test]
  async fn resolve_raw_did_bytes() -> Result<(), Box<dyn StdError>> {
    let resource = IotaResourceLocator::parse(
      "iota:mainnet/0xf4d6f08f5a1b80dd578da7dc1b49c886d580acd4cf7d48119dfeb82b538ad88a/state_metadata",
    )?;
    let resolver = Resolver::default();

    let json_result = resolver.resolve(&resource).await?;
    let did_raw_bytes: Vec<u8> = serde_json::from_value(json_result)?;

    assert_eq!(&did_raw_bytes[..3], b"DID");

    Ok(())
  }

  #[tokio::test]
  async fn empty_path_returns_entire_object() -> Result<(), Box<dyn StdError>> {
    let resolver = Resolver::default();
    let resource =
      "iota:mainnet/0x508974bc41f08e86ed3eb223c90081a9bc3f198be1e758c6ade9eff8db06a2dd"
        .parse::<IotaResourceLocator>()?;
    let resolved_coin = resolver.resolve(&resource).await?;

    let sdk_resolved_coin = fetch_resource(MAINNET_RPC_ENDPOINT, &resource).await?;
    assert_eq!(resolved_coin, sdk_resolved_coin);

    Ok(())
  }

  #[tokio::test]
  async fn resolving_a_non_existing_object_returns_not_found() {
    let resolver = Resolver::default();
    let e = resolver
      .resolve("iota:mainnet/0x4200000000000000000000000000000000000000000000000000000000000042")
      .await
      .unwrap_err();

    assert!(matches!(e.kind, ResolutionErrorKind::NotFound));
  }

  #[tokio::test]
  async fn resolving_on_an_unknown_network_returns_unknown_network() {
    let resolver = Resolver::default();
    let e = resolver
      .resolve("iota:aaaaaaaa/0x4200000000000000000000000000000000000000000000000000000000000042")
      .await
      .unwrap_err();

    assert!(matches!(e.kind, ResolutionErrorKind::UnknownNetwork(_)));
  }
}
