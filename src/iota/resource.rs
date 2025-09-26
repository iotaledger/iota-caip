// Copyright 2020-2025 IOTA Stiftung
// SPDX-License-Identifier: Apache-2.0

use std::fmt::Display;
use std::str::FromStr;

use crate::iota::address::iota_address_parser;
use crate::iota::address::IotaAddress;
use crate::iota::network::iota_chain_id_parser;
use crate::iota::network::IotaNetwork;
use crate::parser::all_consuming;
use crate::parser::char;
use crate::parser::opt;
use crate::parser::preceded;
use crate::parser::separated_pair;
use crate::parser::ParseError;
use crate::parser::Parser as _;
use crate::parser::ParserResult;
use crate::resource::relative_url_parser;
use crate::resource::ChainAgnosticResourceLocator;
use crate::resource::RelativeUrl;

/// A URL-like address used to locate arbitrary resources on an IOTA network.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub struct IotaResourceLocator {
  /// The IOTA network where the resource is located.
  pub network: IotaNetwork,
  /// The IOTA Object ID where the resource is located.
  pub object_id: IotaAddress,
  /// The relative URL within the IOTA Object where the resource is located.
  pub relative_url: RelativeUrl,
  /// The serialized form of the resource locator.
  serialized: Box<str>,
}

impl AsRef<str> for IotaResourceLocator {
  fn as_ref(&self) -> &str {
    &self.serialized
  }
}

impl IotaResourceLocator {
  /// Returns a new [IotaResourceLocator] from the given components.
  pub fn new(network: IotaNetwork, object_id: IotaAddress, relative_url: RelativeUrl) -> Self {
    let serialized = format!("iota:{}/{}{}", network, object_id, relative_url);
    Self {
      network,
      object_id,
      relative_url,
      serialized: serialized.into_boxed_str(),
    }
  }

  /// Returns the string representation of this [IotaResourceLocator].
  pub const fn as_str(&self) -> &str {
    &self.serialized
  }

  /// Parses an [IotaResourceLocator] from the given string.
  /// # Example
  /// [IotaResourceLocator] are mainly used to address IOTA Objects:
  /// ```rust
  /// # use iota_caip::iota::IotaResourceLocator;
  /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
  /// let iota_object = IotaResourceLocator::parse(
  ///   "iota:mainnet/0x1234567890123456789012345678901234567890123456789012345678901234",
  /// )?;
  /// # Ok(())
  /// # }
  /// ```
  /// But it can also be used to address part of an object. For instance, the content of
  /// an object's field:
  /// ```
  /// # use iota_caip::iota::IotaResourceLocator;
  /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
  /// let object_stored_data = IotaResourceLocator::parse(
  ///   "iota:mainnet/0x1234567890123456789012345678901234567890123456789012345678901234/data",
  /// )?;
  /// # assert_eq!(object_stored_data.relative_url.path(), "data");
  /// # Ok(())
  /// # }
  /// ```
  pub fn parse(input: &str) -> Result<Self, IrlParsingError> {
    all_consuming(iota_resource_locator_parser)
      .process(input)
      .map(|(_, out)| out)
      .map_err(|source| IrlParsingError {
        input: input.to_string(),
        source: source.into_owned(),
      })
  }
}

impl Display for IotaResourceLocator {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "iota:{}/{}/{}",
      self.network, self.object_id, self.relative_url
    )
  }
}

impl FromStr for IotaResourceLocator {
  type Err = IrlParsingError;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Self::parse(s)
  }
}

impl From<IotaResourceLocator> for ChainAgnosticResourceLocator {
  fn from(value: IotaResourceLocator) -> Self {
    let chain_id = value.network.into();
    let mut url = value.relative_url;
    url
      .set_path(&format!(
        "{}{}",
        value.object_id,
        url.path().trim_start_matches('/')
      ))
      .expect("valid_path");

    ChainAgnosticResourceLocator {
      chain_id,
      locator: url,
    }
  }
}

/// An error that may occur when parsing an [IotaResourceLocator].
#[derive(Debug)]
#[non_exhaustive]
pub struct IrlParsingError {
  /// The input that was being parsed.
  pub input: String,
  source: ParseError<'static>,
}

impl Display for IrlParsingError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "failed to parse an IOTA resource locator from `{}`",
      self.input
    )
  }
}

impl std::error::Error for IrlParsingError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    Some(&self.source)
  }
}

fn iota_resource_locator_parser(input: &str) -> ParserResult<'_, IotaResourceLocator> {
  let (rem, (chain_id, object_id)) =
    separated_pair(iota_chain_id_parser, char('/'), iota_address_parser)(input)?;
  let (rem, maybe_relative_url) = opt(preceded(opt(char('/')), relative_url_parser))(rem)?;

  let resource_locator = IotaResourceLocator::new(
    chain_id.network,
    object_id,
    maybe_relative_url.unwrap_or_default(),
  );

  Ok((rem, resource_locator))
}
