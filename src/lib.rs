mod util {
    mod key {
        use secp256k1::{self, Secp256k1};

        pub struct PrivateKey {
            pub compressed: bool,
            pub key: secp256k1::SecretKey
        }

        impl PrivateKey {
            pub fn fmt_wif(&self) {
                // KMD uses 160, BTC 128.
                //
            }

            pub fn from_wif() {
                // deserialize from base58
                // check data.len()
                // if 33, uncompressed,
                // if 34, compressed
            }
        }

        pub struct PublicKey {
            pub compressed: bool,
            pub key: secp256k1::PublicKey
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
