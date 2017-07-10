// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

use avtas::lmcp::{LmcpSer};

#[derive(PartialEq, Debug, Copy, Clone, LmcpDerives)]
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
        fn serializes(x: -<enum_name>-) -> TestResult {
            let mut buf: Vec<u8> = vec![0; x.lmcp_size()];
            if let Some(sx) = x.lmcp_ser(&mut buf) {
                return TestResult::from_bool(sx == x.lmcp_size());
            } else {
                return TestResult::failed();
            }
        }

        fn roundtrips(x: -<enum_name>-) -> TestResult {
            let mut buf: Vec<u8> = vec![0; x.lmcp_size()];
            if let Some(sx) = x.lmcp_ser(&mut buf) {
                if let Some((y, sy)) = -<enum_name>-::lmcp_deser(&buf) {
                    return TestResult::from_bool(sx == sy && x == y);
                }
            }
            return TestResult::failed();
        }
    }
}
