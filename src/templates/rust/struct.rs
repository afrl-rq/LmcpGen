// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

#![allow(non_snake_case)]
use avtas::lmcp::{LmcpSer, LmcpStruct, StructInfo};
-<use_dependents>-

#[derive(Debug, Default, LmcpDerives)]
#[repr(C)]
pub struct -<datatype_name>- {-<declare_fields>-
}

impl LmcpStruct for -<datatype_name>- {
    fn GetStructInfo() -> StructInfo {
        StructInfo {
            exist: 1,
            series: -<series_id>-u64,
            version: -<series_version>-,
            struct_ty: -<datatype_id>-,
        }
    }
}

pub trait -<datatype_name>-T-<declare_parent_trait>- {-<declare_trait_methods>-
}
-<declare_trait_impls>-
