// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

use avtas::lmcp::{Error, ErrorType, Lmcp, SrcLoc};

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum -<enum_name>- {-<declare_enum_fields>-
}

impl -<enum_name>- {
    fn from_i32(x: i32) -> Option<-<enum_name>-> {
        match x {
            -<match_enum_from_i32>-
            _ => None,
        }
    }
}

impl Lmcp for -<enum_name>- {
    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> {
        (*self as i32).ser(buf)
    }

    fn deser(buf: &[u8]) -> Result<(-<enum_name>-, usize), Error> {
        let (i, readb) = i32::deser(buf)?;
        let out = -<enum_name>-::from_i32(i).ok_or(error!(ErrorType::InvalidEnumValue))?;
        Ok((out, readb))
    }

    fn size(&self) -> usize { 0i32.size() }
}

impl Default for -<enum_name>- {
    fn default() -> -<enum_name>- {
        -<enum_name>-::-<enum_default_variant>-
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use quickcheck::*;

    impl Arbitrary for -<enum_name>- {
        fn arbitrary<G: Gen>(g: &mut G) -> -<enum_name>- {
            let choices = &[-<enum_variant_choices>-];
            *g.choose(choices).unwrap()
        }
    }

    quickcheck! {
        fn serializes(x: -<enum_name>-) -> Result<TestResult, Error> {
            let mut buf: Vec<u8> = vec![0; x.size()];
            let sx = x.ser(&mut buf)?;
            Ok(TestResult::from_bool(sx == x.size()))
        }

        fn roundtrips(x: -<enum_name>-) -> Result<TestResult, Error> {
            let mut buf: Vec<u8> = vec![0; x.size()];
            let sx = x.ser(&mut buf)?;
            let (y, sy) = -<enum_name>-::deser(&buf)?;
            Ok(TestResult::from_bool(sx == sy && x == y))
        }
    }
}
