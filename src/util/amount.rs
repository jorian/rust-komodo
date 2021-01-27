// To the extent possible under law, the author(s) have dedicated all
// copyright and related and neighboring rights to this software to
// the public domain worldwide. This software is distributed without
// any warranty.
//
// You should have received a copy of the CC0 Public Domain Dedication
// along with this software.
// If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.
//

//! Amounts
//!
//! This module mainly introduces the [Amount] and [SignedAmount] types.
//! We refer to the documentation on the types for more information.
//!

use std::default;
use std::error;
use std::fmt::{self, Write};
use std::ops;
use std::str::FromStr;
use std::cmp::Ordering;

/// A set of denominations in which amounts can be expressed.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Denomination {
    /// BTC
    Bitcoin,
    /// mBTC
    MilliBitcoin,
    /// uBTC
    MicroBitcoin,
    /// bits
    Bit,
    /// satoshi
    Satoshi,
    /// msat
    MilliSatoshi,
}

impl Denomination {
    /// The number of decimal places more than a satoshi.
    fn precision(self) -> i32 {
        match self {
            Denomination::Bitcoin => -8,
            Denomination::MilliBitcoin => -5,
            Denomination::MicroBitcoin => -2,
            Denomination::Bit => -2,
            Denomination::Satoshi => 0,
            Denomination::MilliSatoshi => 3,
        }
    }
}

impl fmt::Display for Denomination {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match *self {
            Denomination::Bitcoin => "BTC",
            Denomination::MilliBitcoin => "mBTC",
            Denomination::MicroBitcoin => "uBTC",
            Denomination::Bit => "bits",
            Denomination::Satoshi => "satoshi",
            Denomination::MilliSatoshi => "msat",
        })
    }
}

impl FromStr for Denomination {
    type Err = ParseAmountError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "BTC" => Ok(Denomination::Bitcoin),
            "mBTC" => Ok(Denomination::MilliBitcoin),
            "uBTC" => Ok(Denomination::MicroBitcoin),
            "bits" => Ok(Denomination::Bit),
            "satoshi" => Ok(Denomination::Satoshi),
            "sat" => Ok(Denomination::Satoshi),
            "msat" => Ok(Denomination::MilliSatoshi),
            d => Err(ParseAmountError::UnknownDenomination(d.to_owned())),
        }
    }
}

/// An error during amount parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseAmountError {
    /// Amount is negative.
    Negative,
    /// Amount is too big to fit inside the type.
    TooBig,
    /// Amount has higher precision than supported by the type.
    TooPrecise,
    /// Invalid number format.
    InvalidFormat,
    /// Input string was too large.
    InputTooLarge,
    /// Invalid character in input.
    InvalidCharacter(char),
    /// The denomination was unknown.
    UnknownDenomination(String),
}

impl fmt::Display for ParseAmountError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParseAmountError::Negative => f.write_str("amount is negative"),
            ParseAmountError::TooBig => f.write_str("amount is too big"),
            ParseAmountError::TooPrecise => f.write_str("amount has a too high precision"),
            ParseAmountError::InvalidFormat => f.write_str("invalid number format"),
            ParseAmountError::InputTooLarge => f.write_str("input string was too large"),
            ParseAmountError::InvalidCharacter(c) => write!(f, "invalid character in input: {}", c),
            ParseAmountError::UnknownDenomination(ref d) => write!(f, "unknown denomination: {}",d),
        }
    }
}

impl error::Error for ParseAmountError {}

fn is_too_precise(s: &str, precision: usize) -> bool {
    s.contains('.') || precision >= s.len() || s.chars().rev().take(precision).any(|d| d != '0')
}

/// Parse decimal string in the given denomination into a satoshi value and a
/// bool indicator for a negative amount.
fn parse_signed_to_satoshi(
    mut s: &str,
    denom: Denomination,
) -> Result<(bool, u64), ParseAmountError> {
    if s.is_empty() {
        return Err(ParseAmountError::InvalidFormat);
    }
    if s.len() > 50 {
        return Err(ParseAmountError::InputTooLarge);
    }

    let is_negative = s.starts_with('-');
    if is_negative {
        if s.len() == 1 {
            return Err(ParseAmountError::InvalidFormat);
        }
        s = &s[1..];
    }

    let max_decimals = {
        // The difference in precision between native (satoshi)
        // and desired denomination.
        let precision_diff = -denom.precision();
        if precision_diff < 0 {
            // If precision diff is negative, this means we are parsing
            // into a less precise amount. That is not allowed unless
            // there are no decimals and the last digits are zeroes as
            // many as the difference in precision.
            let last_n = precision_diff.abs() as usize;
            if is_too_precise(s, last_n) {
                return Err(ParseAmountError::TooPrecise);
            }
            s = &s[0..s.len() - last_n];
            0
        } else {
            precision_diff
        }
    };

    let mut decimals = None;
    let mut value: u64 = 0; // as satoshis
    for c in s.chars() {
        match c {
            '0'..='9' => {
                // Do `value = 10 * value + digit`, catching overflows.
                match 10_u64.checked_mul(value) {
                    None => return Err(ParseAmountError::TooBig),
                    Some(val) => match val.checked_add((c as u8 - b'0') as u64) {
                        None => return Err(ParseAmountError::TooBig),
                        Some(val) => value = val,
                    },
                }
                // Increment the decimal digit counter if past decimal.
                decimals = match decimals {
                    None => None,
                    Some(d) if d < max_decimals => Some(d + 1),
                    _ => return Err(ParseAmountError::TooPrecise),
                };
            }
            '.' => match decimals {
                None => decimals = Some(0),
                // Double decimal dot.
                _ => return Err(ParseAmountError::InvalidFormat),
            },
            c => return Err(ParseAmountError::InvalidCharacter(c)),
        }
    }

    // Decimally shift left by `max_decimals - decimals`.
    let scale_factor = max_decimals - decimals.unwrap_or(0);
    for _ in 0..scale_factor {
        value = match 10_u64.checked_mul(value) {
            Some(v) => v,
            None => return Err(ParseAmountError::TooBig),
        };
    }

    Ok((is_negative, value))
}

/// Format the given satoshi amount in the given denomination.
///
/// Does not include the denomination.
fn fmt_satoshi_in(
    satoshi: u64,
    negative: bool,
    f: &mut dyn fmt::Write,
    denom: Denomination,
) -> fmt::Result {
    if negative {
        f.write_str("-")?;
    }

    let precision = denom.precision();
    match precision.cmp(&0) {
        Ordering::Greater => {
            // add zeroes in the end
            let width = precision as usize;
            write!(f, "{}{:0width$}", satoshi, 0, width = width)?;
        }
        Ordering::Less => {
            // need to inject a comma in the number
            let nb_decimals = precision.abs() as usize;
            let real = format!("{:0width$}", satoshi, width = nb_decimals);
            if real.len() == nb_decimals {
                write!(f, "0.{}", &real[real.len() - nb_decimals..])?;
            } else {
                write!(
                    f,
                    "{}.{}",
                    &real[0..(real.len() - nb_decimals)],
                    &real[real.len() - nb_decimals..]
                )?;
            }
        }
        Ordering::Equal => write!(f, "{}", satoshi)?,
    }
    Ok(())
}

/// Amount
///
/// The [Amount] type can be used to express Bitcoin amounts that supports
/// arithmetic and conversion to various denominations.
///
///
/// Warning!
///
/// This type implements several arithmetic operations from [std::ops].
/// To prevent errors due to overflow or underflow when using these operations,
/// it is advised to instead use the checked arithmetic methods whose names
/// start with `checked_`.  The operations from [std::ops] that [Amount]
/// implements will panic when overflow or underflow occurs.  Also note that
/// since the internal representation of amounts is unsigned, subtracting below
/// zero is considered an underflow and will cause a panic if you're not using
/// the checked arithmetic methods.
///
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Amount(u64);

impl Amount {
    /// The zero amount.
    pub const ZERO: Amount = Amount(0);
    /// Exactly one satoshi.
    pub const ONE_SAT: Amount = Amount(1);
    /// Exactly one bitcoin.
    pub const ONE_BTC: Amount = Amount(100_000_000);

    /// Create an [Amount] with satoshi precision and the given number of satoshis.
    pub fn from_sat(satoshi: u64) -> Amount {
        Amount(satoshi)
    }

    /// Get the number of satoshis in this [Amount].
    pub fn as_sat(self) -> u64 {
        self.0
    }

    /// The maximum value of an [Amount].
    pub fn max_value() -> Amount {
        Amount(u64::max_value())
    }

    /// The minimum value of an [Amount].
    pub fn min_value() -> Amount {
        Amount(u64::min_value())
    }

    /// Convert from a value expressing bitcoins to an [Amount].
    pub fn from_btc(btc: f64) -> Result<Amount, ParseAmountError> {
        Amount::from_float_in(btc, Denomination::Bitcoin)
    }

    /// Parse a decimal string as a value in the given denomination.
    ///
    /// Note: This only parses the value string.  If you want to parse a value
    /// with denomination, use [FromStr].
    pub fn from_str_in(s: &str, denom: Denomination) -> Result<Amount, ParseAmountError> {
        let (negative, satoshi) = parse_signed_to_satoshi(s, denom)?;
        if negative {
            return Err(ParseAmountError::Negative);
        }
        if satoshi > i64::max_value() as u64 {
            return Err(ParseAmountError::TooBig);
        }
        Ok(Amount::from_sat(satoshi))
    }

    /// Parses amounts with denomination suffix like they are produced with
    /// [to_string_with_denomination] or with [fmt::Display].
    /// If you want to parse only the amount without the denomination,
    /// use [from_str_in].
    pub fn from_str_with_denomination(s: &str) -> Result<Amount, ParseAmountError> {
        let mut split = s.splitn(3, ' ');
        let amt_str = split.next().unwrap();
        let denom_str = split.next().ok_or(ParseAmountError::InvalidFormat)?;
        if split.next().is_some() {
            return Err(ParseAmountError::InvalidFormat);
        }

        Ok(Amount::from_str_in(amt_str, denom_str.parse()?)?)
    }

    /// Express this [Amount] as a floating-point value in the given denomination.
    ///
    /// Please be aware of the risk of using floating-point numbers.
    pub fn to_float_in(self, denom: Denomination) -> f64 {
        f64::from_str(&self.to_string_in(denom)).unwrap()
    }

    /// Express this [Amount] as a floating-point value in Bitcoin.
    ///
    /// Equivalent to `to_float_in(Denomination::Bitcoin)`.
    ///
    /// Please be aware of the risk of using floating-point numbers.
    pub fn as_btc(self) -> f64 {
        self.to_float_in(Denomination::Bitcoin)
    }

    /// Convert this [Amount] in floating-point notation with a given
    /// denomination.
    /// Can return error if the amount is too big, too precise or negative.
    ///
    /// Please be aware of the risk of using floating-point numbers.
    pub fn from_float_in(value: f64, denom: Denomination) -> Result<Amount, ParseAmountError> {
        if value < 0.0 {
            return Err(ParseAmountError::Negative);
        }
        // This is inefficient, but the safest way to deal with this. The parsing logic is safe.
        // Any performance-critical application should not be dealing with floats.
        Amount::from_str_in(&value.to_string(), denom)
    }

    /// Format the value of this [Amount] in the given denomination.
    ///
    /// Does not include the denomination.
    pub fn fmt_value_in(self, f: &mut dyn fmt::Write, denom: Denomination) -> fmt::Result {
        fmt_satoshi_in(self.as_sat(), false, f, denom)
    }

    /// Get a string number of this [Amount] in the given denomination.
    ///
    /// Does not include the denomination.
    pub fn to_string_in(self, denom: Denomination) -> String {
        let mut buf = String::new();
        self.fmt_value_in(&mut buf, denom).unwrap();
        buf
    }

    /// Get a formatted string of this [Amount] in the given denomination,
    /// suffixed with the abbreviation for the denomination.
    pub fn to_string_with_denomination(self, denom: Denomination) -> String {
        let mut buf = String::new();
        self.fmt_value_in(&mut buf, denom).unwrap();
        write!(buf, " {}", denom).unwrap();
        buf
    }

    // Some arithmetic that doesn't fit in `std::ops` traits.

    /// Checked addition.
    /// Returns [None] if overflow occurred.
    pub fn checked_add(self, rhs: Amount) -> Option<Amount> {
        self.0.checked_add(rhs.0).map(Amount)
    }

    /// Checked subtraction.
    /// Returns [None] if overflow occurred.
    pub fn checked_sub(self, rhs: Amount) -> Option<Amount> {
        self.0.checked_sub(rhs.0).map(Amount)
    }

    /// Checked multiplication.
    /// Returns [None] if overflow occurred.
    pub fn checked_mul(self, rhs: u64) -> Option<Amount> {
        self.0.checked_mul(rhs).map(Amount)
    }

    /// Checked integer division.
    /// Be aware that integer division loses the remainder if no exact division
    /// can be made.
    /// Returns [None] if overflow occurred.
    pub fn checked_div(self, rhs: u64) -> Option<Amount> {
        self.0.checked_div(rhs).map(Amount)
    }

    /// Checked remainder.
    /// Returns [None] if overflow occurred.
    pub fn checked_rem(self, rhs: u64) -> Option<Amount> {
        self.0.checked_rem(rhs).map(Amount)
    }

    /// Convert to a signed amount.
    pub fn to_signed(self) -> Result<SignedAmount, ParseAmountError> {
        if self.as_sat() > SignedAmount::max_value().as_sat() as u64 {
            Err(ParseAmountError::TooBig)
        } else {
            Ok(SignedAmount::from_sat(self.as_sat() as i64))
        }
    }
}

impl default::Default for Amount {
    fn default() -> Self {
        Amount::ZERO
    }
}

impl fmt::Debug for Amount {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Amount({} satoshi)", self.as_sat())
    }
}

// No one should depend on a binding contract for Display for this type.
// Just using Bitcoin denominated string.
impl fmt::Display for Amount {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_value_in(f, Denomination::Bitcoin)?;
        write!(f, " {}", Denomination::Bitcoin)
    }
}

impl ops::Add for Amount {
    type Output = Amount;

    fn add(self, rhs: Amount) -> Self::Output {
        self.checked_add(rhs).expect("Amount addition error")
    }
}

impl ops::AddAssign for Amount {
    fn add_assign(&mut self, other: Amount) {
        *self = *self + other
    }
}

impl ops::Sub for Amount {
    type Output = Amount;

    fn sub(self, rhs: Amount) -> Self::Output {
        self.checked_sub(rhs).expect("Amount subtraction error")
    }
}

impl ops::SubAssign for Amount {
    fn sub_assign(&mut self, other: Amount) {
        *self = *self - other
    }
}

impl ops::Rem<u64> for Amount {
    type Output = Amount;

    fn rem(self, modulus: u64) -> Self {
        self.checked_rem(modulus).expect("Amount remainder error")
    }
}

impl ops::RemAssign<u64> for Amount {
    fn rem_assign(&mut self, modulus: u64) {
        *self = *self % modulus
    }
}

impl ops::Mul<u64> for Amount {
    type Output = Amount;

    fn mul(self, rhs: u64) -> Self::Output {
        self.checked_mul(rhs).expect("Amount multiplication error")
    }
}

impl ops::MulAssign<u64> for Amount {
    fn mul_assign(&mut self, rhs: u64) {
        *self = *self * rhs
    }
}

impl ops::Div<u64> for Amount {
    type Output = Amount;

    fn div(self, rhs: u64) -> Self::Output {
        self.checked_div(rhs).expect("Amount division error")
    }
}

impl ops::DivAssign<u64> for Amount {
    fn div_assign(&mut self, rhs: u64) {
        *self = *self / rhs
    }
}

impl FromStr for Amount {
    type Err = ParseAmountError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Amount::from_str_with_denomination(s)
    }
}

/// SignedAmount
///
/// The [SignedAmount] type can be used to express Bitcoin amounts that supports
/// arithmetic and conversion to various denominations.
///
///
/// Warning!
///
/// This type implements several arithmetic operations from [std::ops].
/// To prevent errors due to overflow or underflow when using these operations,
/// it is advised to instead use the checked arithmetic methods whose names
/// start with `checked_`.  The operations from [std::ops] that [Amount]
/// implements will panic when overflow or underflow occurs.
///
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SignedAmount(i64);

impl SignedAmount {
    /// The zero amount.
    pub const ZERO: SignedAmount = SignedAmount(0);
    /// Exactly one satoshi.
    pub const ONE_SAT: SignedAmount = SignedAmount(1);
    /// Exactly one bitcoin.
    pub const ONE_BTC: SignedAmount = SignedAmount(100_000_000);

    /// Create an [SignedAmount] with satoshi precision and the given number of satoshis.
    pub fn from_sat(satoshi: i64) -> SignedAmount {
        SignedAmount(satoshi)
    }

    /// Get the number of satoshis in this [SignedAmount].
    pub fn as_sat(self) -> i64 {
        self.0
    }

    /// The maximum value of an [SignedAmount].
    pub fn max_value() -> SignedAmount {
        SignedAmount(i64::max_value())
    }

    /// The minimum value of an [SignedAmount].
    pub fn min_value() -> SignedAmount {
        SignedAmount(i64::min_value())
    }

    /// Convert from a value expressing bitcoins to an [SignedAmount].
    pub fn from_btc(btc: f64) -> Result<SignedAmount, ParseAmountError> {
        SignedAmount::from_float_in(btc, Denomination::Bitcoin)
    }

    /// Parse a decimal string as a value in the given denomination.
    ///
    /// Note: This only parses the value string.  If you want to parse a value
    /// with denomination, use [FromStr].
    pub fn from_str_in(s: &str, denom: Denomination) -> Result<SignedAmount, ParseAmountError> {
        let (negative, satoshi) = parse_signed_to_satoshi(s, denom)?;
        if satoshi > i64::max_value() as u64 {
            return Err(ParseAmountError::TooBig);
        }
        Ok(match negative {
            true => SignedAmount(-(satoshi as i64)),
            false => SignedAmount(satoshi as i64),
        })
    }

    /// Parses amounts with denomination suffix like they are produced with
    /// [to_string_with_denomination] or with [fmt::Display].
    /// If you want to parse only the amount without the denomination,
    /// use [from_str_in].
    pub fn from_str_with_denomination(s: &str) -> Result<SignedAmount, ParseAmountError> {
        let mut split = s.splitn(3, ' ');
        let amt_str = split.next().unwrap();
        let denom_str = split.next().ok_or(ParseAmountError::InvalidFormat)?;
        if split.next().is_some() {
            return Err(ParseAmountError::InvalidFormat);
        }

        Ok(SignedAmount::from_str_in(amt_str, denom_str.parse()?)?)
    }

    /// Express this [SignedAmount] as a floating-point value in the given denomination.
    ///
    /// Please be aware of the risk of using floating-point numbers.
    pub fn to_float_in(self, denom: Denomination) -> f64 {
        f64::from_str(&self.to_string_in(denom)).unwrap()
    }

    /// Express this [SignedAmount] as a floating-point value in Bitcoin.
    ///
    /// Equivalent to `to_float_in(Denomination::Bitcoin)`.
    ///
    /// Please be aware of the risk of using floating-point numbers.
    pub fn as_btc(self) -> f64 {
        self.to_float_in(Denomination::Bitcoin)
    }

    /// Convert this [SignedAmount] in floating-point notation with a given
    /// denomination.
    /// Can return error if the amount is too big, too precise or negative.
    ///
    /// Please be aware of the risk of using floating-point numbers.
    pub fn from_float_in(
        value: f64,
        denom: Denomination,
    ) -> Result<SignedAmount, ParseAmountError> {
        // This is inefficient, but the safest way to deal with this. The parsing logic is safe.
        // Any performance-critical application should not be dealing with floats.
        SignedAmount::from_str_in(&value.to_string(), denom)
    }

    /// Format the value of this [SignedAmount] in the given denomination.
    ///
    /// Does not include the denomination.
    pub fn fmt_value_in(self, f: &mut dyn fmt::Write, denom: Denomination) -> fmt::Result {
        let sats = self.as_sat().checked_abs().map(|a: i64| a as u64).unwrap_or_else(|| {
            // We could also hard code this into `9223372036854775808`
            u64::max_value() - self.as_sat() as u64 +1
        });
        fmt_satoshi_in(sats, self.is_negative(), f, denom)
    }

    /// Get a string number of this [SignedAmount] in the given denomination.
    ///
    /// Does not include the denomination.
    pub fn to_string_in(self, denom: Denomination) -> String {
        let mut buf = String::new();
        self.fmt_value_in(&mut buf, denom).unwrap();
        buf
    }

    /// Get a formatted string of this [SignedAmount] in the given denomination,
    /// suffixed with the abbreviation for the denomination.
    pub fn to_string_with_denomination(self, denom: Denomination) -> String {
        let mut buf = String::new();
        self.fmt_value_in(&mut buf, denom).unwrap();
        write!(buf, " {}", denom).unwrap();
        buf
    }

    // Some arithmetic that doesn't fit in `std::ops` traits.

    /// Get the absolute value of this [SignedAmount].
    pub fn abs(self) -> SignedAmount {
        SignedAmount(self.0.abs())
    }

    /// Returns a number representing sign of this [SignedAmount].
    ///
    /// - `0` if the amount is zero
    /// - `1` if the amount is positive
    /// - `-1` if the amount is negative
    pub fn signum(self) -> i64 {
        self.0.signum()
    }

    /// Returns `true` if this [SignedAmount] is positive and `false` if
    /// this [SignedAmount] is zero or negative.
    pub fn is_positive(self) -> bool {
        self.0.is_positive()
    }

    /// Returns `true` if this [SignedAmount] is negative and `false` if
    /// this [SignedAmount] is zero or positive.
    pub fn is_negative(self) -> bool {
        self.0.is_negative()
    }


    /// Get the absolute value of this [SignedAmount].
    /// Returns [None] if overflow occurred. (`self == min_value()`)
    pub fn checked_abs(self) -> Option<SignedAmount> {
        self.0.checked_abs().map(SignedAmount)
    }

    /// Checked addition.
    /// Returns [None] if overflow occurred.
    pub fn checked_add(self, rhs: SignedAmount) -> Option<SignedAmount> {
        self.0.checked_add(rhs.0).map(SignedAmount)
    }

    /// Checked subtraction.
    /// Returns [None] if overflow occurred.
    pub fn checked_sub(self, rhs: SignedAmount) -> Option<SignedAmount> {
        self.0.checked_sub(rhs.0).map(SignedAmount)
    }

    /// Checked multiplication.
    /// Returns [None] if overflow occurred.
    pub fn checked_mul(self, rhs: i64) -> Option<SignedAmount> {
        self.0.checked_mul(rhs).map(SignedAmount)
    }

    /// Checked integer division.
    /// Be aware that integer division loses the remainder if no exact division
    /// can be made.
    /// Returns [None] if overflow occurred.
    pub fn checked_div(self, rhs: i64) -> Option<SignedAmount> {
        self.0.checked_div(rhs).map(SignedAmount)
    }

    /// Checked remainder.
    /// Returns [None] if overflow occurred.
    pub fn checked_rem(self, rhs: i64) -> Option<SignedAmount> {
        self.0.checked_rem(rhs).map(SignedAmount)
    }

    /// Subtraction that doesn't allow negative [SignedAmount]s.
    /// Returns [None] if either [self], [rhs] or the result is strictly negative.
    pub fn positive_sub(self, rhs: SignedAmount) -> Option<SignedAmount> {
        if self.is_negative() || rhs.is_negative() || rhs > self {
            None
        } else {
            self.checked_sub(rhs)
        }
    }

    /// Convert to an unsigned amount.
    pub fn to_unsigned(self) -> Result<Amount, ParseAmountError> {
        if self.is_negative() {
            Err(ParseAmountError::Negative)
        } else {
            Ok(Amount::from_sat(self.as_sat() as u64))
        }
    }
}

impl default::Default for SignedAmount {
    fn default() -> Self {
        SignedAmount::ZERO
    }
}

impl fmt::Debug for SignedAmount {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SignedAmount({} satoshi)", self.as_sat())
    }
}

// No one should depend on a binding contract for Display for this type.
// Just using Bitcoin denominated string.
impl fmt::Display for SignedAmount {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_value_in(f, Denomination::Bitcoin)?;
        write!(f, " {}", Denomination::Bitcoin)
    }
}

impl ops::Add for SignedAmount {
    type Output = SignedAmount;

    fn add(self, rhs: SignedAmount) -> Self::Output {
        self.checked_add(rhs).expect("SignedAmount addition error")
    }
}

impl ops::AddAssign for SignedAmount {
    fn add_assign(&mut self, other: SignedAmount) {
        *self = *self + other
    }
}

impl ops::Sub for SignedAmount {
    type Output = SignedAmount;

    fn sub(self, rhs: SignedAmount) -> Self::Output {
        self.checked_sub(rhs).expect("SignedAmount subtraction error")
    }
}

impl ops::SubAssign for SignedAmount {
    fn sub_assign(&mut self, other: SignedAmount) {
        *self = *self - other
    }
}

impl ops::Rem<i64> for SignedAmount {
    type Output = SignedAmount;

    fn rem(self, modulus: i64) -> Self {
        self.checked_rem(modulus).expect("SignedAmount remainder error")
    }
}

impl ops::RemAssign<i64> for SignedAmount {
    fn rem_assign(&mut self, modulus: i64) {
        *self = *self % modulus
    }
}

impl ops::Mul<i64> for SignedAmount {
    type Output = SignedAmount;

    fn mul(self, rhs: i64) -> Self::Output {
        self.checked_mul(rhs).expect("SignedAmount multiplication error")
    }
}

impl ops::MulAssign<i64> for SignedAmount {
    fn mul_assign(&mut self, rhs: i64) {
        *self = *self * rhs
    }
}

impl ops::Div<i64> for SignedAmount {
    type Output = SignedAmount;

    fn div(self, rhs: i64) -> Self::Output {
        self.checked_div(rhs).expect("SignedAmount division error")
    }
}

impl ops::DivAssign<i64> for SignedAmount {
    fn div_assign(&mut self, rhs: i64) {
        *self = *self / rhs
    }
}

impl FromStr for SignedAmount {
    type Err = ParseAmountError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        SignedAmount::from_str_with_denomination(s)
    }
}

#[cfg(feature = "serde")]
pub mod serde {
    // methods are implementation of a standardized serde-specific signature
    #![allow(missing_docs)]

    //! This module adds serde serialization and deserialization support for Amounts.
    //! Since there is not a default way to serialize and deserialize Amounts, multiple
    //! ways are supported and it's up to the user to decide which serialiation to use.
    //! The provided modules can be used as follows:
    //!
    //! ```rust,ignore
    //! use serde::{Serialize, Deserialize};
    //! use bitcoin::Amount;
    //!
    //! #[derive(Serialize, Deserialize)]
    //! pub struct HasAmount {
    //!     #[serde(with = "bitcoin::util::amount::serde::as_btc")]
    //!     pub amount: Amount,
    //! }
    //! ```

    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use util::amount::{Amount, Denomination, SignedAmount};

    /// This trait is used only to avoid code duplication and naming collisions
    /// of the different serde serialization crates.
    pub trait SerdeAmount: Copy + Sized {
        fn ser_sat<S: Serializer>(self, s: S) -> Result<S::Ok, S::Error>;
        fn des_sat<'d, D: Deserializer<'d>>(d: D) -> Result<Self, D::Error>;
        fn ser_btc<S: Serializer>(self, s: S) -> Result<S::Ok, S::Error>;
        fn des_btc<'d, D: Deserializer<'d>>(d: D) -> Result<Self, D::Error>;
    }

    impl SerdeAmount for Amount {
        fn ser_sat<S: Serializer>(self, s: S) -> Result<S::Ok, S::Error> {
            u64::serialize(&self.as_sat(), s)
        }
        fn des_sat<'d, D: Deserializer<'d>>(d: D) -> Result<Self, D::Error> {
            Ok(Amount::from_sat(u64::deserialize(d)?))
        }
        fn ser_btc<S: Serializer>(self, s: S) -> Result<S::Ok, S::Error> {
            f64::serialize(&self.to_float_in(Denomination::Bitcoin), s)
        }
        fn des_btc<'d, D: Deserializer<'d>>(d: D) -> Result<Self, D::Error> {
            use serde::de::Error;
            Ok(Amount::from_btc(f64::deserialize(d)?).map_err(D::Error::custom)?)
        }
    }

    impl SerdeAmount for SignedAmount {
        fn ser_sat<S: Serializer>(self, s: S) -> Result<S::Ok, S::Error> {
            i64::serialize(&self.as_sat(), s)
        }
        fn des_sat<'d, D: Deserializer<'d>>(d: D) -> Result<Self, D::Error> {
            Ok(SignedAmount::from_sat(i64::deserialize(d)?))
        }
        fn ser_btc<S: Serializer>(self, s: S) -> Result<S::Ok, S::Error> {
            f64::serialize(&self.to_float_in(Denomination::Bitcoin), s)
        }
        fn des_btc<'d, D: Deserializer<'d>>(d: D) -> Result<Self, D::Error> {
            use serde::de::Error;
            Ok(SignedAmount::from_btc(f64::deserialize(d)?).map_err(D::Error::custom)?)
        }
    }

    pub mod as_sat {
        //! Serialize and deserialize [Amount] as real numbers denominated in satoshi.
        //! Use with `#[serde(with = "amount::serde::as_sat")]`.

        use serde::{Deserializer, Serializer};
        use util::amount::serde::SerdeAmount;

        pub fn serialize<A: SerdeAmount, S: Serializer>(a: &A, s: S) -> Result<S::Ok, S::Error> {
            a.ser_sat(s)
        }

        pub fn deserialize<'d, A: SerdeAmount, D: Deserializer<'d>>(d: D) -> Result<A, D::Error> {
            A::des_sat(d)
        }

        pub mod opt {
            //! Serialize and deserialize [Optoin<Amount>] as real numbers denominated in satoshi.
            //! Use with `#[serde(default, with = "amount::serde::as_sat::opt")]`.

            use serde::{Deserializer, Serializer};
            use util::amount::serde::SerdeAmount;

            pub fn serialize<A: SerdeAmount, S: Serializer>(
                a: &Option<A>,
                s: S,
            ) -> Result<S::Ok, S::Error> {
                match *a {
                    Some(a) => a.ser_sat(s),
                    None => s.serialize_none(),
                }
            }

            pub fn deserialize<'d, A: SerdeAmount, D: Deserializer<'d>>(
                d: D,
            ) -> Result<Option<A>, D::Error> {
                Ok(Some(A::des_sat(d)?))
            }
        }
    }

    pub mod as_btc {
        //! Serialize and deserialize [Amount] as JSON numbers denominated in BTC.
        //! Use with `#[serde(with = "amount::serde::as_btc")]`.

        use serde::{Deserializer, Serializer};
        use util::amount::serde::SerdeAmount;

        pub fn serialize<A: SerdeAmount, S: Serializer>(a: &A, s: S) -> Result<S::Ok, S::Error> {
            a.ser_btc(s)
        }

        pub fn deserialize<'d, A: SerdeAmount, D: Deserializer<'d>>(d: D) -> Result<A, D::Error> {
            A::des_btc(d)
        }

        pub mod opt {
            //! Serialize and deserialize [Option<Amount>] as JSON numbers denominated in BTC.
            //! Use with `#[serde(default, with = "amount::serde::as_btc::opt")]`.

            use serde::{Deserializer, Serializer};
            use util::amount::serde::SerdeAmount;

            pub fn serialize<A: SerdeAmount, S: Serializer>(
                a: &Option<A>,
                s: S,
            ) -> Result<S::Ok, S::Error> {
                match *a {
                    Some(a) => a.ser_btc(s),
                    None => s.serialize_none(),
                }
            }

            pub fn deserialize<'d, A: SerdeAmount, D: Deserializer<'d>>(
                d: D,
            ) -> Result<Option<A>, D::Error> {
                Ok(Some(A::des_btc(d)?))
            }
        }
    }
}