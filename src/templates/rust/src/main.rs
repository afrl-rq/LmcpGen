// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

extern crate lmcp;

fn main() {
    let buf : Vec<u8>  = vec!
        [76, 77, 67, 80, 0, 0, 0, 34, 1, 67, 77, 65, 83, 73, 0, 0, 0, 0, 0, 0, 2, 0, 3, 0, 2, 52, 54, 0, 13, 72, 101, 108, 108, 111, 32, 102, 114, 111, 109, 32, 35, 49, 0, 0, 7, 118];
    if let lmcp::LmcpType::KeyValuePair(kv) = lmcp::lmcp_msg_deser(&buf).unwrap().unwrap() {
        println!("{:?}", String::from_utf8(kv.value));
    }
}
