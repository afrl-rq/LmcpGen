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

#[macro_use]
extern crate lmcp_derive;

#[macro_use]
#[cfg(test)]
extern crate quickcheck;

#[macro_use]
pub mod avtas;
-<declare_top_level_modules>-

use avtas::lmcp::{LmcpSer, StructInfo};
-<use_all_structs>-

macro_rules! get(
    ($e:expr) =>
    (match $e { Some(e) => e, None => {println!("{:?}", stringify!($e)); return None} })
);

macro_rules! get2(
    ($e:expr) => (match $e { Some(e) => e, None => {println!("error: {:?}", stringify!($e)); return Err(()) } })
);

#[derive(Debug)]
pub enum LmcpType {
    -<declare_top_enum>-
}

impl LmcpSer for LmcpType {
    fn lmcp_ser(&self, buf: &mut [u8]) -> Option<usize> {
        match *self {
            -<match_lmcp_ser>-
        }
    }

    fn lmcp_size(&self) -> usize {
        match *self {
            -<match_lmcp_size>-
        }
    }

    fn lmcp_deser(buf: &[u8]) -> Option<(LmcpType, usize)> {
        let (si, _) = get!(StructInfo::lmcp_deser(buf)); // TODO: support null structs or get rid of them
        match (si.series, si.struct_ty) {
            -<match_lmcp_deser>-
            _ => None,
        }
    }
}

pub fn LmcpParseMsg(buf: &[u8]) -> Result<Option<LmcpType>, ()> {
    let size: u32;
    {
        let h = get2!(buf.get(0..4));
        if h[0] != 76 || h[1] != 77 || h[2] != 67 || h[3] != 80 {
            return Err(());
        }
    }
    {
        let r = get2!(buf.get(4..8));
        let (a, _) = get2!(u32::lmcp_deser(r));
        size = a;
    }

    if (size + 12) as usize != buf.len() {
        // 12 = 8 for header + 4 for checksum
        return Err(());
    }
    let h = get2!(buf.get(8..));
    let (si, _) = get2!(StructInfo::lmcp_deser(h));
    let ch = get2!(buf.get(buf.len() - 4..buf.len()));
    let (_, _): (u32, _) = get2!(u32::lmcp_deser(ch)); // checksum
    if si.exist == 0 {
        return Ok(None);
    }


    match LmcpSer::lmcp_deser(h) {
        Some((e, _)) => Ok(Some(e)),
        None => Err(()),
    }


}


pub fn LmcpMakeMsg(obj: &LmcpType, buf: &mut [u8]) -> Result<usize, ()> {
    let size = obj.lmcp_size();
    if size > (u32::max_value() as usize) {
        return Err(());
    }

    {
        let r = get2!(buf.get_mut(0..4));
        r[0] = 76;
        r[1] = 77;
        r[2] = 67;
        r[3] = 80;
    }
    {
        let r = get2!(buf.get_mut(4..));
        get2!((size as u32).lmcp_ser(r));
    }
    {
        let r = get2!(buf.get_mut(8..));
        let wr = get2!(obj.lmcp_ser(r));
        if wr != size {
            return Err(());
        }
    }
    {
        let r = get2!(buf.get_mut(size + 8..size + 12));
        r[0] = 0;
        r[1] = 0;
        r[2] = 0;
        r[3] = 0;
    }

    Ok(size + 12)
}

pub fn LmcpMsgSize(obj: &LmcpType) -> usize {
    obj.lmcp_size() + 12
}
