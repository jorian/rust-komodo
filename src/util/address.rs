use bitcoin::hashes::Hash;
use bitcoin::util::base58;
// use bitcoin::util::base58::from;
use bitcoin::{PubkeyHash, Script, ScriptHash};
// use serde::*;
use std::error;
use std::fmt::{self, Display, Formatter};
use std::str::FromStr;

/// Address error.
#[derive(Debug, PartialEq)]
pub enum Error {
    Base58(base58::Error),
    UncompressedPubkey,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Error::Base58(ref e) => write!(f, "base58: {}", e),
            Error::UncompressedPubkey => {
                write!(f, "an uncompressed pubkey was used where it is not allowed")
            }
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&dyn error::Error> {
        match *self {
            Error::Base58(ref e) => Some(e),
            _ => None,
        }
    }
}

impl From<base58::Error> for Error {
    fn from(e: base58::Error) -> Error {
        Error::Base58(e)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Address {
    pub addr_type: AddressType,
    // todo: verify payload
    // - whether address is valid
    // - compressed / uncompressed
    // - P2SH / P2PKH
    pub payload: Payload,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Payload {
    PubkeyHash(PubkeyHash),
    ScriptHash(ScriptHash),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AddressType {
    P2pkh,
    P2sh,
    Shielded,
}

macro_rules! serde_string_impl {
    ($name:ident, $expecting:expr) => {
        impl<'de> ::serde::Deserialize<'de> for $name {
            fn deserialize<D>(deserializer: D) -> Result<$name, D::Error>
            where
                D: ::serde::de::Deserializer<'de>,
            {
                use ::std::fmt::{self, Formatter};
                use ::std::str::FromStr;

                struct Visitor;
                impl<'de> ::serde::de::Visitor<'de> for Visitor {
                    type Value = $name;

                    fn expecting(&self, formatter: &mut Formatter) -> fmt::Result {
                        formatter.write_str($expecting)
                    }

                    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
                    where
                        E: ::serde::de::Error,
                    {
                        $name::from_str(v).map_err(E::custom)
                    }

                    fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
                    where
                        E: ::serde::de::Error,
                    {
                        self.visit_str(v)
                    }

                    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
                    where
                        E: ::serde::de::Error,
                    {
                        self.visit_str(&v)
                    }
                }

                deserializer.deserialize_str(Visitor)
            }
        }

        impl<'de> ::serde::Serialize for $name {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: ::serde::Serializer,
            {
                serializer.collect_str(&self)
            }
        }
    };
}
serde_string_impl!(Address, "a Komodo address");

impl Address {
    pub fn p2pkh(_pk: secp256k1::PublicKey) -> Address {
        unimplemented!()
    }

    pub fn p2sh(_script: &Script) -> Address {
        unimplemented!()
    }

    pub fn address_type(&self) -> Option<AddressType> {
        match self.payload {
            Payload::PubkeyHash(_) => Some(AddressType::P2pkh),
            Payload::ScriptHash(_) => Some(AddressType::P2sh),
        }
    }

    pub fn script_pubkey(&self) -> Script {
        // self.payload.script_pubkey()
        unimplemented!()
    }

    // /// for use in `z_shieldcoinbase` to merge all coinbases to a Shielded address
    // pub fn any() -> Address {
    //     Address {
    //         payload: "*".to_string(),
    //         addr_type: AddrType::Transparent
    //     }
    // }
}

impl FromStr for Address {
    type Err = Error;

    fn from_str(s: &str) -> Result<Address, Error> {
        if s.len() > 50 {
            return Err(Error::Base58(base58::Error::InvalidLength(
                s.len() * 11 / 15,
            )));
        }

        let data = base58::from_check(s)?;
        if data.len() != 21 {
            return Err(Error::Base58(base58::Error::InvalidLength(data.len())));
        }

        let (addr_type, payload) = match data[0] {
            60 => (
                AddressType::P2pkh,
                Payload::PubkeyHash(PubkeyHash::from_slice(&data[1..]).unwrap()),
            ),
            85 => (
                AddressType::P2sh,
                Payload::ScriptHash(ScriptHash::from_slice(&data[1..]).unwrap()),
            ),
            x => return Err(Error::Base58(base58::Error::InvalidVersion(vec![x]))),
        };

        Ok(Address { addr_type, payload })
    }
}

// impl<'de> serde::Deserialize<'de> for Address {
//     fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
//         where D: Deserializer<'de> {
//         let s: String = Deserialize::deserialize(deserializer)?;
//
//         // todo
//         Address::from_str(&s).map_err(D::Error::custom)
//     }
// }

// impl ::serde::Serialize for Address {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where
//         S: ::serde::Serializer {
//         serializer.collect_str(&self.payload)
//     }
// }

// impl Serialize for Address {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where
//         S: Serializer {
//         // todo
//         serializer.serialize_str(&*format!("{}", self.payload))
//     }
// }

impl Display for Address {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match self.payload {
            Payload::PubkeyHash(ref hash) => {
                let mut prefixed = [0; 21];
                prefixed[0] = 60;
                prefixed[1..].copy_from_slice(&hash[..]);
                base58::check_encode_slice_to_fmt(fmt, &prefixed[..])
            }
            Payload::ScriptHash(ref hash) => {
                let mut prefixed = [0; 21];
                prefixed[0] = 85;
                prefixed[1..].copy_from_slice(&hash[..]);
                base58::check_encode_slice_to_fmt(fmt, &prefixed[..])
            }
        }
    }
}

impl ::std::fmt::Debug for Address {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
