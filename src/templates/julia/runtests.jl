## ===============================================================================
## Authors: AFRL/RQQA
## Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
## 
## Copyright (c) 2017 Government of the United State of America, as represented by
## the Secretary of the Air Force.  No copyright is claimed in the United States under
## Title 17, U.S. Code.  All Other Rights Reserved.
## ===============================================================================

## This file was auto-created by LmcpGen. Modifications will be overwritten.

import LMCP
import Random
using Test, InteractiveUtils

function _subtypes(type::Type)
    out = Any[]
    return _subtypes!(out, type)
end

function _subtypes!(out, type::Type)
    if !isabstracttype(type)
        push!(out, type)
    else
        foreach(T->_subtypes!(out, T), subtypes(type))
    end
    return out
end


function _rand(type, _) return Random.rand(type) end
function _rand(::Type{String}, _) l = Random.rand(UInt8); return Random.randstring(Int(l)) end
function _rand(enum::Type{T}, _) where T<:Enum return rand(instances(enum)) end
function _rand(::Type{Vector{T}}, depthLimit) where T return [_rand(T, depthLimit) for x in 1:3] end
function _rand(::Type{T}, depthLimit) where T <: LMCP.AbstractLmcpMessage ret = rand(_subtypes(T))(); randomize!(ret, depthLimit); return ret end
function _rand(::Type{Union{T, Nothing}}, depthLimit) where T <: LMCP.AbstractLmcpMessage 
    retNothingCheck = Random.rand([0,1]) == 1
    if retNothingCheck return nothing end
    ret = rand(_subtypes(T))()
    randomize!(ret, depthLimit)
    return ret
end

function randomize!(o::LMCP.AbstractLmcpMessage, depthLimit=3)

    if depthLimit == 0 return end

    type = typeof(o)
    for (n, t) in zip(fieldnames(type), fieldtypes(type))
        #print("$n $t\n")
        value = _rand(t, depthLimit - 1)
        setproperty!(o, n, value)
    end   
end 


function test_pack_unpack(o::LMCP.AbstractLmcpMessage)
    buffer = LMCP.pack_message(o)
    tmp2 = LMCP.unpack_message(buffer)
    return o == tmp2
end

function test_xml(o::LMCP.AbstractLmcpMessage)
    xml = LMCP.to_xml(o)
    tmp2 = LMCP.from_xml(xml)
    return o == tmp2
end

const msg_types = _subtypes(LMCP.AbstractLmcpMessage)

@testset "Default constructors" begin
    for T ∈ msg_types
        @test T() isa T
    end
end

@testset "pack_unpack" begin
    Random.seed!(1234)
    for T ∈ msg_types
        msg = T()
        randomize!(msg)
        @test test_pack_unpack(msg)
    end
end

@testset "to_from_xml" begin
    Random.seed!(1234)
    for T ∈ msg_types
        msg = T()
        randomize!(msg)
        @test test_xml(msg)
    end
end
