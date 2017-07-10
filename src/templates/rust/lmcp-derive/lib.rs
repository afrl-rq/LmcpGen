// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

#[proc_macro_derive(LmcpDerives)]
pub fn descend(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_macro_input(&s).unwrap();
    let gen = impl_descend(&ast);
    gen.parse().unwrap()
}

fn impl_descend(ast: &syn::MacroInput) -> quote::Tokens {
    let name = &ast.ident;
    let is_struct = match ast.body {
        syn::Body::Struct(_) => {true}
        _ => {false}
    };

    let ser_body_top = if is_struct {
        quote! {
        let mut pos = 0;
        {
            let x = get!(Self::GetStructInfo().lmcp_ser(buf));
            pos += x;
        }
        }
        }
    else { quote! { let mut pos = 0; } }
    ;


    let ser_body = match ast.body {
        syn::Body::Struct(syn::VariantData::Struct(ref body)) => {
            let fields=body.iter().filter_map(|field| field.ident.as_ref()).map(|ident| quote! {
                {
                    let r = get!(buf.get_mut(pos ..));
                    let b : usize = get!(self.#ident.lmcp_ser(r));
                    pos += b;
                }
            }).collect::<Vec<_>>();
            quote! { #(#fields)* }
        },
        syn::Body::Struct(_) => { panic!("only variant structs supported"); }

        syn::Body::Enum(_) => {
            quote! {
                let r = get!(buf.get_mut(pos ..));
                let b : usize = get!((*self as i32).lmcp_ser(r));
                pos += b;
            }
        }
    };

    let deser_top =
        if is_struct {
        quote!{ let mut out : #name = Default::default(); let mut pos = 0;
        {
            let (_si, u) = get!(StructInfo::lmcp_deser(buf)); // TODO certify correct structinfo
            pos += u;
        }
    }
        } else {
            quote! { let mut out: #name = Default::default(); let mut pos = 0; }
        };

    let deser_body = match ast.body {
        syn::Body::Struct(syn::VariantData::Struct(ref body)) => {
            let fields=body.iter().map(|field| {
                let ident = field.ident.as_ref();
                let ref ty = field.ty;
                                       quote! {
                {
                    let r = get!(buf.get(pos ..));
                    let (x, readb) : (#ty, usize) = get!(LmcpSer::lmcp_deser(r));
                    out.#ident = x;
                    pos += readb;
                }
                                       }}).collect::<Vec<_>>();
            quote! { #(#fields)* }
        },
        syn::Body::Struct(_) => { panic!("only variant structs supported"); }

        syn::Body::Enum(_) => {
            quote! {
                let r = get!(buf.get(pos ..));
                let (i, readb) : (i32, usize) = get!(i32::lmcp_deser(r));
                out = get!(#name::from_i32(i));
                pos += readb;
            }
        }

    };

    let size_body = match ast.body {
        syn::Body::Struct(syn::VariantData::Struct(ref body)) => {
            let fields=body.iter().map(|field| {
                let ident = field.ident.as_ref();
                quote!{ size += self.#ident.lmcp_size(); }
            }).collect::<Vec<_>>();
            quote! { size += 15; #(#fields)* }
        },
        syn::Body::Struct(_) => {panic!("only variant structs supported"); }
        syn::Body::Enum(_) => {
            quote! { size += 4; }
        }
    };

    quote! {

        impl LmcpSer for #name {
        fn lmcp_ser(&self, buf: &mut[u8]) -> Option<usize> { #ser_body_top
             #ser_body
             Some(pos) }
        fn lmcp_deser(buf: &[u8]) -> Option<(#name, usize)> { #deser_top
            #deser_body
            Some((out,pos))
        }
        fn lmcp_size(&self) -> usize { let mut size = 0; #size_body size }

        }
    }

}
