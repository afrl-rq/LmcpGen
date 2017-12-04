// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

use avtas::lmcp::{Error, ErrorType, Lmcp, LmcpSubscription, SrcLoc, Struct, StructInfo};
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

impl Struct for -<datatype_name>- {
    fn struct_info() -> StructInfo {
        StructInfo {
            exist: 1,
            series: -<series_id>-u64,
            version: -<series_version>-,
            struct_ty: -<datatype_id>-,
        }
    }
}

impl Lmcp for -<datatype_name>- {
    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> {
        let mut pos = 0;
        {
            let x = Self::struct_info().ser(buf)?;
            pos += x;
        }
        -<struct_ser_body>-
        Ok(pos)
    }

    fn deser(buf: &[u8]) -> Result<(-<datatype_name>-, usize), Error> {
        let mut pos = 0;
        let (si, u) = StructInfo::deser(buf)?;
        pos += u;
        if si == -<datatype_name>-::struct_info() {
            -<struct_deser_body>-
            Ok((out, pos))
        } else {
            Err(error!(ErrorType::InvalidStructInfo))
        }
    }

    fn size(&self) -> usize {
        -<struct_size_body>-
        size
    }
}

pub trait -<datatype_name>-T: Debug + Send -<declare_parent_trait>- {
    -<declare_trait_methods>-
}

impl Clone for Box<-<datatype_name>-T> {
    fn clone(&self) -> Box<-<datatype_name>-T> {
        if let Some(x) = -<datatype_name>-T::as_-<series_snake_name>-_-<datatype_snake_name>-(self.as_ref()) {
            Box::new(x.clone())
        -<trait_clone_cases>-
        } else {
            unreachable!()
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

impl Lmcp for Box<-<datatype_name>-T> {
    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> {
        if let Some(x) = -<datatype_name>-T::as_-<series_snake_name>-_-<datatype_snake_name>-(self.as_ref()) {
            x.ser(buf)
        -<trait_ser_cases>-
        } else {
            unreachable!()
        }
    }

    fn deser(buf: &[u8]) -> Result<(Box<-<datatype_name>-T>, usize), Error> {
        let (si, _) = StructInfo::deser(buf)?;
        if si == -<datatype_name>-::struct_info() {
            let (x, readb) = -<datatype_name>-::deser(buf)?;
            Ok((Box::new(x), readb))
        -<trait_deser_cases>-
        } else {
            Err(error!(ErrorType::InvalidStructInfo))
        }
    }

    fn size(&self) -> usize {
        if let Some(x) = -<datatype_name>-T::as_-<series_snake_name>-_-<datatype_snake_name>-(self.as_ref()) {
            x.size()
        -<trait_size_cases>-
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

    impl Arbitrary for -<datatype_name>- {
        fn arbitrary<G: Gen>(_g: &mut G) -> -<datatype_name>- {
            -<datatype_name>- {
                -<declare_arbitrary_fields>-
            }
        }
    }

    quickcheck! {
        fn serializes(x: -<datatype_name>-) -> Result<TestResult, Error> {
            -<discard_long_fields>-
            let mut buf: Vec<u8> = vec![0; x.size()];
            let sx = x.ser(&mut buf)?;
            Ok(TestResult::from_bool(sx == x.size()))
        }

        fn roundtrips(x: -<datatype_name>-) -> Result<TestResult, Error> {
            -<discard_long_fields>-
            let mut buf: Vec<u8> = vec![0; x.size()];
            let sx = x.ser(&mut buf)?;
            let (y, sy) = -<datatype_name>-::deser(&buf)?;
            Ok(TestResult::from_bool(sx == sy && x == y))
        }
    }
}
