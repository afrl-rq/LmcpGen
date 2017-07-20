// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

use std::mem;

macro_rules! get(
    ($e:expr) => (match $e { Some(e) => e, None => {println!("error: {:?}", stringify!($e)); return None} })
);

pub trait LmcpSer where Self : Sized {
    fn lmcp_ser(&self, buf : &mut[u8]) -> Option<usize>;
    fn lmcp_deser(data : &[u8]) -> Option<(Self, usize)>;
    fn lmcp_size(&self) -> usize;
}

#[derive(Debug, Default, PartialEq)]
pub struct StructInfo {
    pub exist : u8,
    pub series : u64,
    pub struct_ty : u32,
    pub version : u16
}
impl LmcpSer for StructInfo {
    fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> {
        {
            let m = get!(buf.get_mut(0..1));
            self.exist.lmcp_ser(m);
        }
        {
            let m = get!(buf.get_mut(1..9));
            self.series.lmcp_ser(m);
        }
        {
            let m = get!(buf.get_mut(9..13));
            self.struct_ty.lmcp_ser(m);
        }
        {
            let m = get!(buf.get_mut(13..15));
            self.version.lmcp_ser(m);
        }
        Some(15)
    }

    fn lmcp_deser(buf: &[u8]) -> Option<(StructInfo, usize)> {
        let mut out : StructInfo = Default::default();
        {
            let (t, _) = get!(u8::lmcp_deser(buf));
            out.exist = t;
        }
        {
            let b = get!(buf.get(1..));
            let (t, _) = get!(u64::lmcp_deser(b));
            out.series = t;
        }
        {
            let b = get!(buf.get(9..));
            let (t,_) = get!(u32::lmcp_deser(b));
            out.struct_ty = t;
        }
        {
            let b = get!(buf.get(13..));
            let (t,_) = get!(u16::lmcp_deser(b));
            out.version = t;
        }
        Some((out, 15))
    }

    fn lmcp_size(&self) -> usize { 15 }
}


pub trait LmcpStruct {
    fn struct_info() -> StructInfo;
    fn lmcp_write_struct_header(buf: &mut[u8]) -> Option<usize> {
        Self::struct_info().lmcp_ser(buf)
    }
}

impl LmcpSer for bool {
    fn lmcp_size(&self) -> usize {1}
    fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> {
        let x = get!(buf.first_mut());
        match *self {
            true => {*x = 1;}
            false => {*x = 0;}
        }
        Some(1)
    }
    fn lmcp_deser(data: &[u8]) -> Option<(bool, usize)> { data.first().map(|p| (match *p {
        0 => {(false, 1)}
        _ => {(true, 1)}
    }
    ))}
}

impl LmcpSer for u8 {
    fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> {
        let x = get!(buf.first_mut());
        *x = *self;
        Some(1)
    }
    fn lmcp_deser(data : &[u8]) -> Option<(u8, usize)> { data.first().map(|p| (*p, 1)) }
    fn lmcp_size(&self) -> usize { 1 }
}

impl LmcpSer for u16 {
    fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> {
        let m = get!(buf.get_mut(0..2));
        m[0] = (*self >> 8) as u8;
        m[1] = *self as u8;
        Some(2)
    }
    fn lmcp_deser(data : &[u8]) -> Option<(u16, usize)> {
        let d = get!(data.get(0..2));
        Some((((d[0] as u16) << 8) | (d[1] as u16), 2))
    }
    fn lmcp_size(&self) -> usize { 2 }
}

impl LmcpSer for u32 {
    fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> {
        let m = get!(buf.get_mut(0..4));
        m[0] = (*self >> 24) as u8;
        m[1] = (*self >> 16) as u8;
        m[2] = (*self >> 8) as u8;
        m[3] = *self as u8;
        Some(4)
    }
    fn lmcp_deser(data : &[u8]) -> Option<(u32, usize)> {
        let d = get!(data.get(0..4));
        Some((((d[0] as u32) << 24)
             | ((d[1] as u32) << 16)
             | ((d[2] as u32) << 8)
             | (d[3] as u32), 4))
    }
    fn lmcp_size(&self) -> usize { 4 }
}

impl LmcpSer for u64 {
    fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> {
        let m = get!(buf.get_mut(0..8));
        m[0] = (*self >> 56) as u8;
        m[1] = (*self >> 48) as u8;
        m[2] = (*self >> 40) as u8;
        m[3] = (*self >> 32) as u8;
        m[4] = (*self >> 24) as u8;
        m[5] = (*self >> 16) as u8;
        m[6] = (*self >> 8) as u8;
        m[7] = *self as u8;
        Some(8)
    }
    fn lmcp_deser(data : &[u8]) -> Option<(u64, usize)> {
        let d = get!(data.get(0..8));
        Some((((d[0] as u64) << 56)
             | ((d[1] as u64) << 48)
             | ((d[2] as u64) << 40)
             | ((d[3] as u64) << 32)
             | ((d[4] as u64) << 24)
             | ((d[5] as u64) << 16)
             | ((d[6] as u64) << 8)
             | ((d[7] as u64)), 8))
    }
    fn lmcp_size(&self) -> usize { 8 }
}

impl LmcpSer for i8 {
    fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> { (*self as u8).lmcp_ser(buf) }
    fn lmcp_deser(data : &[u8]) -> Option<(i8, usize)> { u8::lmcp_deser(data).map(|(y,z)| (y as i8,z) ) }
    fn lmcp_size(&self) -> usize { 1 }
}

impl LmcpSer for i16 {
    fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> { (*self as u16).lmcp_ser(buf) }
    fn lmcp_deser(data : &[u8]) -> Option<(i16, usize)> { u16::lmcp_deser(data).map(|(y,z)| (y as i16, z) ) }
    fn lmcp_size(&self) -> usize { 2 }
}

impl LmcpSer for i32 {
    fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> { (*self as u32).lmcp_ser(buf) }
    fn lmcp_deser(data : &[u8]) -> Option<(i32, usize)> { u32::lmcp_deser(data).map(|(y,z)| (y as i32,z)) }
    fn lmcp_size(&self) -> usize { 4 }
}

impl LmcpSer for i64 {
    fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> { (*self as u64).lmcp_ser(buf) }
    fn lmcp_deser(data : &[u8]) -> Option<(i64, usize)> { u64::lmcp_deser(data).map(|(y,z)| (y as i64, z)) }
    fn lmcp_size(&self) -> usize { 8 }
}

impl LmcpSer for f32 {
    fn lmcp_size(&self) -> usize { 4 }
    fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> {
        let i : u32 = unsafe {mem::transmute(*self)};
        i.lmcp_ser(buf)
    }
    fn lmcp_deser(data : &[u8]) -> Option<(f32,usize)> {
        u32::lmcp_deser(data).map(|(y,z)| (unsafe {mem::transmute(y)}, z)) }
}

impl LmcpSer for f64 {
    fn lmcp_size(&self) -> usize { 8 }
    fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> {
        let i : u64 = unsafe {mem::transmute(*self)};
        i.lmcp_ser(buf)
    }
    fn lmcp_deser(data : &[u8]) -> Option<(f64,usize)> {
        u64::lmcp_deser(data).map(|(y,z)| (unsafe {mem::transmute(y)}, z)) }
}

impl<T> LmcpSer for Option<T> where T : LmcpSer {
    // format: 0x00 for None, full struct serialization for Some

    fn lmcp_size(&self) -> usize {
        if let &Some(ref x) = self {
            x.lmcp_size()
        } else {
            1
        }
    }

    fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> {
        if let &Some(ref x) = self {
            x.lmcp_ser(buf)
        } else {
            0u8.lmcp_ser(buf)
        }
    }

    fn lmcp_deser(buf: &[u8]) -> Option<(Option<T>, usize)> {
        let b = get!(buf.get(0..1));
        if b[0] == 1u8 {
            let (x, readb) = get!(T::lmcp_deser(buf));
            Some((Some(x), readb))
        } else {
            Some((None, 1))
        }
    }
}

impl<T> LmcpSer for Vec<T> where T : LmcpSer {
    // format: (vector length as u32), e_1, e_2, ..., e_k
    // TODO: the below assumes u16 for vector size all the time, not u32. if i want to change this,
    // i can make a wrapper for Vec<T>.
    fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> {
        if self.len() > (u16::max_value() as usize) {
            println!("length issue");
            return None
        }

        let size : u16 = self.len() as u16;
        let mut outlen : usize = 0;
        {
            let sizewr = get!(buf.get_mut(0..2));
            let r = get!(size.lmcp_ser(sizewr));
            outlen += r;
        }

        for t in self {
            let b = get!(buf.get_mut(outlen .. ));
            let r = get!(t.lmcp_ser(b));
            outlen += r;
        }

        Some(outlen)

    }

    fn lmcp_deser(data: &[u8]) -> Option<(Vec<T>, usize)> {
        let mut out : Vec<T> = vec![];
        let sd = get!(data.get(0..2));
        let (size, r) = get!(u16::lmcp_deser(sd));
        let mut pos : usize = r;
        for _ in 0 .. size {
            let d = get!(data.get(pos..));
            let (ob, readb) = get!(T::lmcp_deser(d));
            out.push(ob);
            pos += readb;
        }

        Some((out, pos))
    }
    fn lmcp_size(&self) -> usize {
        let mut size = 2;
        for t in self {
            size += t.lmcp_size();
        }
        return size;
    }
}
