// use bitcoin::util::base58;
use secp256k1::{self, Secp256k1};
use serde::*;
use std::fmt::{self, Write};
// use serde::de::Visitor;
use bitcoin::hashes::Hash;
use bitcoin::util::base58;
// use bitcoin::util::key::Error;
use bitcoin::PubkeyHash;
use std::fmt::Formatter;

use std::str::FromStr;
use std::{error, io};

#[derive(Debug)]
pub enum Error {
    Base58(base58::Error),
    Secp256k1(secp256k1::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Base58(ref e) => write!(f, "base58 error: {}", e),
            Error::Secp256k1(ref e) => write!(f, "secp256k1 error: {}", e),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&dyn error::Error> {
        match *self {
            Error::Base58(ref e) => Some(e),
            Error::Secp256k1(ref e) => Some(e),
        }
    }
}

#[doc(hidden)]
impl From<base58::Error> for Error {
    fn from(e: base58::Error) -> Error {
        Error::Base58(e)
    }
}

#[doc(hidden)]
impl From<secp256k1::Error> for Error {
    fn from(e: secp256k1::Error) -> Error {
        Error::Secp256k1(e)
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct PrivateKey {
    pub compressed: bool,
    pub key: secp256k1::SecretKey,
}

impl PrivateKey {
    pub fn public_key<C: secp256k1::Signing>(&self, secp: &Secp256k1<C>) -> PublicKey {
        PublicKey {
            compressed: self.compressed,
            key: secp256k1::PublicKey::from_secret_key(secp, &self.key),
        }
    }

    pub fn fmt_wif(&self, fmt: &mut dyn fmt::Write) -> fmt::Result {
        let mut ret = [0; 34];
        ret[0] = 188;
        ret[1..33].copy_from_slice(&self.key[..]);
        let privkey = if self.compressed {
            ret[33] = 1;
            base58::check_encode_slice(&ret[..])
        } else {
            base58::check_encode_slice(&ret[..33])
        };
        fmt.write_str(&privkey)
    }

    pub fn from_wif(wif: &str) -> Result<PrivateKey, bitcoin::util::key::Error> {
        let data = base58::from_check(wif)?;

        let compressed = match data.len() {
            33 => false,
            34 => true,
            _ => {
                return Err(bitcoin::util::key::Error::Base58(
                    base58::Error::InvalidLength(data.len()),
                ));
            }
        };

        Ok(PrivateKey {
            compressed,
            key: secp256k1::SecretKey::from_slice(&data[1..33]).unwrap(),
        })
    }

    pub fn to_wif(&self) -> String {
        let mut buf = String::new();
        buf.write_fmt(format_args!("{}", self)).unwrap();
        buf.shrink_to_fit();

        buf
    }
}

impl fmt::Display for PrivateKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_wif(f)
    }
}

impl fmt::Debug for PrivateKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[private key data]")
    }
}

impl FromStr for PrivateKey {
    type Err = bitcoin::util::key::Error;
    fn from_str(s: &str) -> Result<PrivateKey, bitcoin::util::key::Error> {
        match s.len() {
            52 => PrivateKey::from_wif(s),
            64 => Ok(PrivateKey {
                compressed: false,
                key: secp256k1::SecretKey::from_str(s).unwrap(),
            }),
            _ => Err(bitcoin::util::key::Error::Base58(base58::Error::Other(
                String::from("invalid length trying to convert from str"),
            ))),
        }
    }
}

impl ::serde::Serialize for PrivateKey {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ::serde::Serializer,
    {
        serializer.collect_str(self)
    }
}

impl<'de> ::serde::Deserialize<'de> for PrivateKey {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error>
    where
        D: Deserializer<'de>,
    {
        struct WifVisitor;

        impl<'de> ::serde::de::Visitor<'de> for WifVisitor {
            type Value = PrivateKey;

            fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
                formatter.write_str("an ASCII WIF string")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: ::serde::de::Error,
            {
                dbg!(&v);
                PrivateKey::from_str(v).map_err(E::custom)
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: ::serde::de::Error,
            {
                if let Ok(s) = ::std::str::from_utf8(v) {
                    PrivateKey::from_str(s).map_err(E::custom)
                } else {
                    Err(E::invalid_value(::serde::de::Unexpected::Bytes(v), &self))
                }
            }
        }

        deserializer.deserialize_str(WifVisitor)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PublicKey {
    pub compressed: bool,
    pub key: secp256k1::PublicKey,
}

impl PublicKey {
    pub fn pubkey_hash(&self) -> PubkeyHash {
        if self.compressed {
            PubkeyHash::hash(&self.key.serialize())
        } else {
            PubkeyHash::hash(&self.key.serialize_uncompressed())
        }
    }

    pub fn write_into<W: io::Write>(&self, mut writer: W) {
        let _: io::Result<()> = if self.compressed {
            writer.write_all(&self.key.serialize())
        } else {
            writer.write_all(&self.key.serialize_uncompressed())
        };
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut buf = Vec::new();
        self.write_into(&mut buf);

        buf
    }

    pub fn from_slice(data: &[u8]) -> Result<PublicKey, Error> {
        let compressed: bool = match data.len() {
            33 => true,
            65 => false,
            len => {
                return Err(bitcoin::util::base58::Error::InvalidLength(len).into());
            }
        };

        Ok(PublicKey {
            compressed,
            key: secp256k1::PublicKey::from_slice(data)?,
        })
    }

    pub fn from_private_key<C: secp256k1::Signing>(
        secp: &Secp256k1<C>,
        sk: &PrivateKey,
    ) -> PublicKey {
        sk.public_key(secp)
    }
}

impl fmt::Display for PublicKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.compressed {
            for ch in &self.key.serialize()[..] {
                write!(f, "{:02x}", ch)?;
            }
        } else {
            for ch in &self.key.serialize_uncompressed()[..] {
                write!(f, "{:02x}", ch)?;
            }
        }
        Ok(())
    }
}

impl FromStr for PublicKey {
    type Err = Error;
    fn from_str(s: &str) -> Result<PublicKey, Error> {
        let key = secp256k1::PublicKey::from_str(s)?;
        Ok(PublicKey {
            key: key,
            compressed: s.len() == 66,
        })
    }
}

impl ::serde::Serialize for PublicKey {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        if serializer.is_human_readable() {
            serializer.collect_str(self)
        } else {
            if self.compressed {
                serializer.serialize_bytes(&self.key.serialize()[..])
            } else {
                serializer.serialize_bytes(&self.key.serialize_uncompressed()[..])
            }
        }
    }
}

impl<'de> ::serde::Deserialize<'de> for PublicKey {
    fn deserialize<D>(d: D) -> Result<Self, <D as Deserializer<'de>>::Error>
    where
        D: Deserializer<'de>,
    {
        if d.is_human_readable() {
            struct HexVisitor;

            impl<'de> ::serde::de::Visitor<'de> for HexVisitor {
                type Value = PublicKey;

                fn expecting(&self, formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                    formatter.write_str("an ASCII hex string")
                }

                fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
                where
                    E: ::serde::de::Error,
                {
                    PublicKey::from_str(v).map_err(E::custom)
                }

                fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
                where
                    E: ::serde::de::Error,
                {
                    if let Ok(hex) = ::std::str::from_utf8(v) {
                        PublicKey::from_str(hex).map_err(E::custom)
                    } else {
                        Err(E::invalid_value(::serde::de::Unexpected::Bytes(v), &self))
                    }
                }
            }
            d.deserialize_str(HexVisitor)
        } else {
            struct BytesVisitor;

            impl<'de> ::serde::de::Visitor<'de> for BytesVisitor {
                type Value = PublicKey;

                fn expecting(&self, formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                    formatter.write_str("a bytestring")
                }

                fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
                where
                    E: ::serde::de::Error,
                {
                    PublicKey::from_slice(v).map_err(E::custom)
                }
            }

            d.deserialize_bytes(BytesVisitor)
        }
    }
}
