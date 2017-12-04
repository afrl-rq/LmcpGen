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

macro_rules! error {
    ( $ty:expr ) => {
        Error {
            error_type: $ty,
            src_loc: SrcLoc { file: file!(), line: line!(), col: column!() },
        }
    }
}

macro_rules! get {
    ( $e:expr ) => {
        $e.ok_or(error!(ErrorType::NotEnoughBytes))?
    }
}

pub trait Lmcp where Self : Sized {
    fn ser(&self, buf : &mut[u8]) -> Result<usize, Error>;
    fn deser(data : &[u8]) -> Result<(Self, usize), Error>;
    fn size(&self) -> usize;
}

pub trait LmcpSubscription {
    fn subscription() -> &'static str;
}

#[derive(Debug)]
pub struct Error {
    pub error_type: ErrorType,
    pub src_loc: SrcLoc,
}

#[derive(Debug)]
pub enum ErrorType {
    ArrayTooLong,
    InvalidEnumValue,
    InvalidLmcpHeader,
    InvalidStructInfo,
    MessageSizeMismatch,
    MessageTooLarge,
    NotEnoughBytes,
    UnknownStruct(StructInfo),
}

#[derive(Debug)]
pub struct SrcLoc {
    pub file: &'static str,
    pub line: u32,
    pub col: u32,
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct StructInfo {
    pub exist : u8,
    pub series : u64,
    pub struct_ty : u32,
    pub version : u16
}

impl Lmcp for StructInfo {
    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> {
        {
            let m = get!(buf.get_mut(0..1));
            self.exist.ser(m)?;
        }
        {
            let m = get!(buf.get_mut(1..9));
            self.series.ser(m)?;
        }
        {
            let m = get!(buf.get_mut(9..13));
            self.struct_ty.ser(m)?;
        }
        {
            let m = get!(buf.get_mut(13..15));
            self.version.ser(m)?;
        }
        Ok(15)
    }

    fn deser(buf: &[u8]) -> Result<(StructInfo, usize), Error> {
        let mut out : StructInfo = Default::default();
        {
            let (t, _) = u8::deser(buf)?;
            out.exist = t;
        }
        {
            let b = get!(buf.get(1..));
            let (t, _) = u64::deser(b)?;
            out.series = t;
        }
        {
            let b = get!(buf.get(9..));
            let (t,_) = u32::deser(b)?;
            out.struct_ty = t;
        }
        {
            let b = get!(buf.get(13..));
            let (t,_) = u16::deser(b)?;
            out.version = t;
        }
        Ok((out, 15))
    }

    fn size(&self) -> usize { 15 }
}


pub trait Struct {
    fn struct_info() -> StructInfo;
    fn lmcp_write_struct_header(buf: &mut[u8]) -> Result<usize, Error> {
        Self::struct_info().ser(buf)
    }
}

impl Lmcp for bool {
    fn size(&self) -> usize {1}
    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> {
        let x = get!(buf.first_mut());
        match *self {
            true => {*x = 1;}
            false => {*x = 0;}
        }
        Ok(1)
    }
    fn deser(data: &[u8]) -> Result<(bool, usize), Error> {
        data.first().map(|p| (match *p {
            0 => {(false, 1)}
            _ => {(true, 1)}
        })).ok_or(error!(ErrorType::NotEnoughBytes))
    }
}

impl Lmcp for u8 {
    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> {
        let x = get!(buf.first_mut());
        *x = *self;
        Ok(1)
    }
    fn deser(data : &[u8]) -> Result<(u8, usize), Error> {
        data.first().map(|p| (*p, 1)).ok_or(error!(ErrorType::NotEnoughBytes))
    }
    fn size(&self) -> usize { 1 }
}

impl Lmcp for u16 {
    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> {
        let m = get!(buf.get_mut(0..2));
        m[0] = (*self >> 8) as u8;
        m[1] = *self as u8;
        Ok(2)
    }
    fn deser(data : &[u8]) -> Result<(u16, usize), Error> {
        let d = get!(data.get(0..2));
        Ok((((d[0] as u16) << 8) | (d[1] as u16), 2))
    }
    fn size(&self) -> usize { 2 }
}

impl Lmcp for u32 {
    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> {
        let m = get!(buf.get_mut(0..4));
        m[0] = (*self >> 24) as u8;
        m[1] = (*self >> 16) as u8;
        m[2] = (*self >> 8) as u8;
        m[3] = *self as u8;
        Ok(4)
    }
    fn deser(data : &[u8]) -> Result<(u32, usize), Error> {
        let d = get!(data.get(0..4));
        Ok((((d[0] as u32) << 24)
            | ((d[1] as u32) << 16)
            | ((d[2] as u32) << 8)
            | (d[3] as u32), 4))
    }
    fn size(&self) -> usize { 4 }
}

impl Lmcp for u64 {
    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> {
        let m = get!(buf.get_mut(0..8));
        m[0] = (*self >> 56) as u8;
        m[1] = (*self >> 48) as u8;
        m[2] = (*self >> 40) as u8;
        m[3] = (*self >> 32) as u8;
        m[4] = (*self >> 24) as u8;
        m[5] = (*self >> 16) as u8;
        m[6] = (*self >> 8) as u8;
        m[7] = *self as u8;
        Ok(8)
    }
    fn deser(data : &[u8]) -> Result<(u64, usize), Error> {
        let d = get!(data.get(0..8));
        Ok((((d[0] as u64) << 56)
            | ((d[1] as u64) << 48)
            | ((d[2] as u64) << 40)
            | ((d[3] as u64) << 32)
            | ((d[4] as u64) << 24)
            | ((d[5] as u64) << 16)
            | ((d[6] as u64) << 8)
            | ((d[7] as u64)), 8))
    }
    fn size(&self) -> usize { 8 }
}

impl Lmcp for i8 {
    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> { (*self as u8).ser(buf) }
    fn deser(data : &[u8]) -> Result<(i8, usize), Error> { u8::deser(data).map(|(y,z)| (y as i8,z) ) }
    fn size(&self) -> usize { 1 }
}

impl Lmcp for i16 {
    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> { (*self as u16).ser(buf) }
    fn deser(data : &[u8]) -> Result<(i16, usize), Error> { u16::deser(data).map(|(y,z)| (y as i16, z) ) }
    fn size(&self) -> usize { 2 }
}

impl Lmcp for i32 {
    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> { (*self as u32).ser(buf) }
    fn deser(data : &[u8]) -> Result<(i32, usize), Error> { u32::deser(data).map(|(y,z)| (y as i32,z)) }
    fn size(&self) -> usize { 4 }
}

impl Lmcp for i64 {
    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> { (*self as u64).ser(buf) }
    fn deser(data : &[u8]) -> Result<(i64, usize), Error> { u64::deser(data).map(|(y,z)| (y as i64, z)) }
    fn size(&self) -> usize { 8 }
}

impl Lmcp for f32 {
    fn size(&self) -> usize { 4 }
    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> {
        let i : u32 = unsafe {mem::transmute(*self)};
        i.ser(buf)
    }
    fn deser(data : &[u8]) -> Result<(f32,usize), Error> {
        u32::deser(data).map(|(y,z)| (unsafe {mem::transmute(y)}, z)) }
}

impl Lmcp for f64 {
    fn size(&self) -> usize { 8 }
    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> {
        let i : u64 = unsafe {mem::transmute(*self)};
        i.ser(buf)
    }
    fn deser(data : &[u8]) -> Result<(f64,usize), Error> {
        u64::deser(data).map(|(y,z)| (unsafe {mem::transmute(y)}, z)) }
}

impl<T> Lmcp for Option<T> where T : Lmcp {
    // format: 0x00 for None, full struct serialization for Some

    fn size(&self) -> usize {
        if let &Some(ref x) = self {
            x.size()
        } else {
            1
        }
    }

    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> {
        if let &Some(ref x) = self {
            x.ser(buf)
        } else {
            0u8.ser(buf)
        }
    }

    fn deser(buf: &[u8]) -> Result<(Option<T>, usize), Error> {
        let b = get!(buf.get(0..1));
        if b[0] == 1u8 {
            let (x, readb) = T::deser(buf)?;
            Ok((Some(x), readb))
        } else {
            Ok((None, 1))
        }
    }
}

impl<T> Lmcp for Vec<T> where T : Lmcp {
    // format: (vector length as u32), e_1, e_2, ..., e_k
    // TODO: the below assumes u16 for vector size all the time, not u32. if i want to change this,
    // i can make a wrapper for Vec<T>.
    fn ser(&self, buf: &mut[u8]) -> Result<usize, Error> {
        if self.len() > (u16::max_value() as usize) {
            println!("length issue");
            return Err(error!(ErrorType::ArrayTooLong))
        }

        let size : u16 = self.len() as u16;
        let mut outlen : usize = 0;
        {
            let sizewr = get!(buf.get_mut(0..2));
            let r = size.ser(sizewr)?;
            outlen += r;
        }

        for t in self {
            let b = get!(buf.get_mut(outlen .. ));
            let r = t.ser(b)?;
            outlen += r;
        }

        Ok(outlen)

    }

    fn deser(data: &[u8]) -> Result<(Vec<T>, usize), Error> {
        let mut out : Vec<T> = vec![];
        let sd = get!(data.get(0..2));
        let (size, r) = u16::deser(sd)?;
        let mut pos : usize = r;
        for _ in 0 .. size {
            let d = get!(data.get(pos..));
            let (ob, readb) = T::deser(d)?;
            out.push(ob);
            pos += readb;
        }

        Ok((out, pos))
    }
    fn size(&self) -> usize {
        let mut size = 2;
        for t in self {
            size += t.size();
        }
        return size;
    }
}
