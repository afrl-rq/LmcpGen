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

#[derive(Debug, Copy, Clone, LmcpDerives)]
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
