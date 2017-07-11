// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

use avtas::lmcp::{LmcpSer, LmcpStruct, StructInfo};
-<use_dependents>-

#[derive(PartialEq, Clone, Debug, Default)]
#[repr(C)]
pub struct -<datatype_name>- {-<declare_fields>-
}

impl LmcpStruct for -<datatype_name>- {
    fn get_struct_info() -> StructInfo {
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
            let x = get!(Self::get_struct_info().lmcp_ser(buf));
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

pub trait -<datatype_name>-T-<declare_parent_trait>- {-<declare_trait_methods>-
}
-<declare_trait_impls>-

#[cfg(test)]
pub mod tests {
    use super::*;
    use quickcheck::*;

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
