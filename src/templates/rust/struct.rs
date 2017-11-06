// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

use avtas::lmcp::{LmcpSer, LmcpStruct, LmcpSubscription, StructInfo};
use std::fmt::Debug;

#[derive(Clone, -<struct_copy>-Debug, Default)]
#[repr(C)]
pub struct -<datatype_name>- {-<declare_fields>-
}

impl PartialEq for -<datatype_name>- {
    fn eq(&self, _other: &-<datatype_name>-) -> bool {
        true
        -<struct_partialeq_cases>-
    }
}

impl LmcpSubscription for -<datatype_name>- {
    fn subscription() -> &'static str { "-<longdatatype_name_dots>-" }
}

impl LmcpStruct for -<datatype_name>- {
    fn struct_info() -> StructInfo {
        StructInfo {
            exist: 1,
            series: -<series_id>-u64,
            version: -<series_version>-,
            struct_ty: -<datatype_id>-,
        }
    }
}

impl LmcpSer for -<datatype_name>- {
    fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> {
        let mut pos = 0;
        {
            let x = get!(Self::struct_info().lmcp_ser(buf));
            pos += x;
        }
        -<struct_lmcp_ser_body>-
        Some(pos)
    }

    fn lmcp_deser(buf: &[u8]) -> Option<(-<datatype_name>-, usize)> {
        let mut pos = 0;
        {
            let (_si, u) = get!(StructInfo::lmcp_deser(buf));
            // TODO: assert correctness properties of StructInfo
            pos += u;
        }
        -<struct_lmcp_deser_body>-
        Some((out, pos))
    }

    fn lmcp_size(&self) -> usize {
        -<struct_lmcp_size_body>-
        size
    }
}

pub trait -<datatype_name>-T: Debug -<declare_parent_trait>- {
    -<declare_trait_methods>-
}

impl Clone for Box<-<datatype_name>-T> {
    fn clone(&self) -> Box<-<datatype_name>-T> {
        if let Some(x) = -<datatype_name>-T::as_-<series_snake_name>-_-<datatype_snake_name>-(self.as_ref()) {
            Box::new(x.clone())
        -<trait_clone_cases>-
        } else {
            panic!("clone error for: {:?}", self)
        }
    }
}

impl Default for Box<-<datatype_name>-T> {
    fn default() -> Box<-<datatype_name>-T> { Box::new(-<datatype_name>-::default()) }
}

impl PartialEq for Box<-<datatype_name>-T> {
    fn eq(&self, other: &Box<-<datatype_name>-T>) -> bool {
        if let (Some(x), Some(y)) =
            (-<datatype_name>-T::as_-<series_snake_name>-_-<datatype_snake_name>-(self.as_ref()),
             -<datatype_name>-T::as_-<series_snake_name>-_-<datatype_snake_name>-(other.as_ref())) {
                x == y
        -<trait_partialeq_cases>-
        } else {
            false
        }
    }
}

impl LmcpSer for Box<-<datatype_name>-T> {
    fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> {
        if let Some(x) = -<datatype_name>-T::as_-<series_snake_name>-_-<datatype_snake_name>-(self.as_ref()) {
            x.lmcp_ser(buf)
        -<trait_lmcp_ser_cases>-
        } else {
            unreachable!()
        }
    }

    fn lmcp_deser(buf: &[u8]) -> Option<(Box<-<datatype_name>-T>, usize)> {
        let (si, _) = get!(StructInfo::lmcp_deser(buf));
        if si == -<datatype_name>-::struct_info() {
            let (x, readb) = get!(-<datatype_name>-::lmcp_deser(buf));
            Some((Box::new(x), readb))
        -<trait_lmcp_deser_cases>-
        } else {
            None
        }
    }

    fn lmcp_size(&self) -> usize {
        if let Some(x) = -<datatype_name>-T::as_-<series_snake_name>-_-<datatype_snake_name>-(self.as_ref()) {
            x.lmcp_size()
        -<trait_lmcp_size_cases>-
        } else {
            unreachable!()
        }
    }
}

-<declare_trait_impls>-

#[cfg(test)]
pub mod tests {
    use super::*;
    use quickcheck::*;

    unsafe impl Send for -<datatype_name>- {}

    impl Arbitrary for -<datatype_name>- {
        fn arbitrary<G: Gen>(_g: &mut G) -> -<datatype_name>- {
            -<datatype_name>- {
                -<declare_arbitrary_fields>-
            }
        }
    }

    quickcheck! {
        fn serializes(x: -<datatype_name>-) -> TestResult {
            -<discard_long_fields>-
            let mut buf: Vec<u8> = vec![0; x.lmcp_size()];
            if let Some(sx) = x.lmcp_ser(&mut buf) {
                return TestResult::from_bool(sx == x.lmcp_size());
            } else {
                return TestResult::failed();
            }
        }

        fn roundtrips(x: -<datatype_name>-) -> TestResult {
            -<discard_long_fields>-
            let mut buf: Vec<u8> = vec![0; x.lmcp_size()];
            if let Some(sx) = x.lmcp_ser(&mut buf) {
                if let Some((y, sy)) = -<datatype_name>-::lmcp_deser(&buf) {
                    return TestResult::from_bool(sx == sy && x == y);
                }
            }
            return TestResult::failed();
        }
    }
}
