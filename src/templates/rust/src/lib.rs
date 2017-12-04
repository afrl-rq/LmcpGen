// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

#![allow(non_camel_case_types)]

#[macro_use]
#[cfg(test)]
extern crate quickcheck;

#[macro_use]
mod avtas;
-<declare_top_level_modules>-

use avtas::lmcp::{Lmcp, StructInfo};
pub use avtas::lmcp::{Error, ErrorType, LmcpSubscription, SrcLoc};

#[derive(Debug)]
pub enum Message {
    -<declare_top_enum>-
}

impl Lmcp for Message {
    fn ser(&self, buf: &mut [u8]) -> Result<usize, Error> {
        match *self {
            -<match_ser>-
        }
    }

    fn size(&self) -> usize {
        match *self {
            -<match_size>-
        }
    }

    fn deser(buf: &[u8]) -> Result<(Message, usize), Error> {
        let (si, _) = StructInfo::deser(buf)?; // TODO: support null structs or get rid of them
        match (si.series, si.struct_ty) {
            -<match_deser>-
            _ => Err(error!(ErrorType::UnknownStruct(si)))
        }
    }
}

impl Message {
    pub fn subscription(&self) -> &'static str {
        match *self {
            -<match_subscription>-
        }
    }

    pub fn deser(buf: &[u8]) -> Result<Option<Message>, Error> {
        let h = get!(buf.get(0..4));
        if h[0] != 76 || h[1] != 77 || h[2] != 67 || h[3] != 80 {
            return Err(error!(ErrorType::InvalidLmcpHeader));
        }

        let r = get!(buf.get(4..8));
        let (size, _) = u32::deser(r)?;

        if (size + 12) as usize != buf.len() {
            // 12 = 8 for header + 4 for checksum
            return Err(error!(ErrorType::MessageSizeMismatch));
        }
        let h = get!(buf.get(8..));
        let (si, _) = StructInfo::deser(h)?;
        let ch = get!(buf.get(buf.len() - 4..buf.len()));
        let (_, _): (u32, _) = u32::deser(ch)?; // TODO: checksum
        if si.exist == 0 {
            Ok(None)
        } else {
            let (msg, _) = Lmcp::deser(h)?;
            Ok(Some(msg))
        }
    }

    pub fn ser(&self, buf: &mut [u8]) -> Result<usize, Error> {
        let size = self.size();
        if size > (u32::max_value() as usize) {
            return Err(error!(ErrorType::MessageTooLarge));
        }

        {
            let r = get!(buf.get_mut(0..4));
            r[0] = 76;
            r[1] = 77;
            r[2] = 67;
            r[3] = 80;
        }
        {
            let r = get!(buf.get_mut(4..));
            (size as u32).ser(r)?;
        }
        {
            let r = get!(buf.get_mut(8..));
            let wr = self.ser(r)?;
            if wr != size {
                return Err(error!(ErrorType::MessageSizeMismatch));
            }
        }
        {
            // TODO: optionally implement checksum
            let r = get!(buf.get_mut(size + 8..size + 12));
            r[0] = 0;
            r[1] = 0;
            r[2] = 0;
            r[3] = 0;
        }

        Ok(size + 12)
    }

    pub fn size(&self) -> usize {
        Lmcp::size(self) + 12
    }
}
