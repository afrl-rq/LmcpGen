## ===============================================================================
## Authors: AFRL/RQQA
## Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
## 
## Copyright (c) 2017 Government of the United State of America, as represented by
## the Secretary of the Air Force.  No copyright is claimed in the United States under
## Title 17, U.S. Code.  All Other Rights Reserved.
## ===============================================================================

## This file was auto-created by LmcpGen. Modifications will be overwritten.

abstract type AbstractLmcpMessage end

function ==(a::T, b::T) where {T <: AbstractLmcpMessage}
    names = fieldnames(T)
    for name ∈ names
        getfield(a, name) == getfield(b, name) ? nothing : return false
    end
    return true
end


full_lmcp_type_name(::Type{T}) where T <: AbstractLmcpMessage = "Lmcp"
full_lmcp_type_name(::T) where T <: AbstractLmcpMessage = full_lmcp_type_name(T)

lmcp_type(::Type{T}) where T <: AbstractLmcpMessage = "abstract"
lmcp_type(::T) where T <: AbstractLmcpMessage = lmcp_type(T)

lmcp_type_id(::Type{T}) where T <: AbstractLmcpMessage = UInt32(0)
lmcp_type_id(::T) where T <: AbstractLmcpMessage = lmcp_type_id(T)

series_name(::Type{T}) where T <: AbstractLmcpMessage = "ABSTRACT"
series_name(::T) where T <: AbstractLmcpMessage  = series_name(T)

series_name_id(::Type{T}) where T <: AbstractLmcpMessage = Int64(0)
series_name_id(::T) where T <: AbstractLmcpMessage  = series_name_id(T)

series_version(::Type{T}) where T <: AbstractLmcpMessage = UInt16(0)
series_version(::T) where T <: AbstractLmcpMessage = series_version(T)
    


function _write(io::IO, o)
    write(io, hton(o))
end
    
function _write(io::IO, o::Enum)
    _write(io, Int32(o))
end

function _write(io::IO, ::Nothing)
    _write(io, false)
end

function _write(io::IO, o::String)
    _write(io, UInt16(length(o))) +   #length
    write(io, o)                      #contents
end

function _write(io::IO, o::Vector, ltype::Type{T}=UInt16) where T<:Union{UInt16, UInt32}
    bytes = _write(io, T(length(o)))
    for i in o
        bytes += _write(io, i)
    end
    return bytes
end

_write(io::IO, o::AbstractLmcpMessage) = write(io, o)  #Forward to high level write
function write(io::IO, o::T) where T <: AbstractLmcpMessage

    #write header
    bytes = _write(io, true)
    bytes += _write(io, series_name_id(T))
    bytes += _write(io, lmcp_type_id(T))
    bytes += _write(io, series_version(T))

    # loop through all fields and write
    for field in fieldnames(T)
        if (T, field) in LARGE_ARRAY_REGISTRY
            bytes += _write(io, getfield(o, field), UInt32)
            continue
        end
        bytes += _write(io, getfield(o, field))
    end

    return bytes
end


function _read(io::IO, o)
    ntoh(read(io, o))
end

function _read(io::IO, ::Type{String})
    l = _read(io, UInt16)
    String(read(io, l))
end

function _read(io::IO, ::Type{Vector{T}}, ltype::Type{T2}=UInt16) where {T, T2<:Union{UInt16, UInt32}}
    l = _read(io, T2)
    vec = T[]
    sizehint!(vec, l)
    for i ∈ 1:l
        push!(vec, _read(io, T))
    end
    return vec
end

function _read(io::IO, ::Type{T}) where T <: Enum
    T(_read(io, Int32))    # read as Int32 then conveert to Enum type T
end

read(io::IO, t::Type{AbstractLmcpMessage}) = _read(io, t)  #Forward to high level read
function _read(io::IO, ::Type{Union{T, Nothing}}) where T <: AbstractLmcpMessage
    
    # check if exists
    exists = _read(io, Bool)
    if !exists return nothing end

    series_name = _read(io, Int64)
    object_type = _read(io, UInt32)
    series_version = _read(io, UInt16)

    T2 = OBJECT_ID_REGISTRY[(series_name, object_type)]

    _read(io, T2; skip_header = true)    # skip header for message since already read
end


function _read(io::IO, ::Type{T}; skip_header = false) where T <: AbstractLmcpMessage
    type = T
    if !skip_header
        exists = _read(io, Bool)
        series_name = _read(io, Int64)
        object_type = _read(io, UInt32)
        series_version = _read(io, UInt16)
        type = OBJECT_ID_REGISTRY[(series_name, object_type)]
    end

    values = map(fieldnames(type), fieldtypes(type)) do n, t
        if (type, n) in LARGE_ARRAY_REGISTRY
            v = _read(io, t, UInt32)
            return v
        end
        v = _read(io, t)
        v
    end

    type(values...)
end
    

function _toLmcpType(t::Type)
    if t == Bool return "bool" end
    if t == String return "string" end
    if t == UInt8 return "byte" end
    if t == Char return "char"end 
    if t == Float64 return "real64" end
    if t == Float32 return "float32" end
    if t == Int64 return "int64" end
    if t == Int32 return "int32" end
    if t == Int16 return "int16" end
    if t == UInt32 return "uint32" end
    if t == UInt16 return "uint16" end
    if t <: AbstractLmcpMessage return lmcp_type(t) end
    return "LMCPObject"
end
    
function _to_xml(xroot::LightXML.AbstractXMLNode, o)
    LightXML.add_text(xroot, string(o))
    return xroot
end
    
function _to_xml(xroot::LightXML.AbstractXMLNode, o::Type{T}) where T <: Enum
    LightXML.add_text(xroot, string(Symbol(o)))
end
    
function _to_xml(xroot::LightXML.AbstractXMLNode, vector::Vector)
    for v in vector
        tmpChild = LightXML.new_child(xroot, _toLmcpType(typeof(v)))
        if typeof(v) <: AbstractLmcpMessage
            _to_xml(tmpChild, v)
        else
            LightXML.add_text(tmpChild, string(v))
        end
    end
    return xroot
end

function _to_xml(xroot::LightXML.AbstractXMLNode, o::Nothing)
    return xroot
end


function _to_xml(xroot::LightXML.AbstractXMLNode, o::T) where T <: AbstractLmcpMessage
    LightXML.set_attribute(xroot, "Series", series_name(T))
    type = typeof(o)
    for (n, t) in zip(fieldnames(type), fieldtypes(type))
            value = getproperty(o, n)
            if isnothing(value) continue end
            child = LightXML.new_child(xroot, string(n))
            if t <: AbstractLmcpMessage || Nothing <: t #check if inner LMCP object
                child = LightXML.new_child(child, lmcp_type(value)) 
            end
            _to_xml(child, getproperty(o, n))
    end    
    return xroot
end


function _from_xml(xroot::LightXML.AbstractXMLNode, o::Type{T})  where T <: Number
    text = LightXML.content(xroot)
    return parse(T, text)
end
    
function _from_xml(xroot::LightXML.AbstractXMLNode, ::Type{String})
    text = LightXML.content(xroot)
    return text
end

function _from_xml(xroot::LightXML.AbstractXMLNode, ::Type{Vector{T}}) where T
    vec = T[]
    memberName = LightXML.child_elements(xroot)
    for subChild in memberName
        push!(vec, _from_xml(subChild, T))
    end
    return vec
end

function  _from_xml(xroot::LightXML.AbstractXMLNode, ::Type{T}) where T <: Enum
    text = LightXML.content(xroot)
    return T(text)
end

function _from_xml(xroot::LightXML.AbstractXMLNode, o::Type{T}) where T <: AbstractLmcpMessage
    lmcpNode = LightXML.has_attribute(xroot, "Series") ? xroot : collect(LightXML.child_elements(xroot))[1]
    name = LightXML.name(lmcpNode)
    series = LightXML.attribute(lmcpNode, "Series", required=true)
    obj = TEXT_CREATION_REGISTRY[(series, name)]()
    return _from_xml(lmcpNode, obj)
end

function _from_xml(xroot::LightXML.AbstractXMLNode, o::Type{Union{T, Nothing}}) where T <: AbstractLmcpMessage
    lmcpNode = LightXML.has_attribute(xroot, "Series") ? xroot : collect(LightXML.child_elements(xroot))[1]
    name = LightXML.name(lmcpNode)
    series = LightXML.attribute(lmcpNode, "Series", required=true)
    
    obj = TEXT_CREATION_REGISTRY[(series, name)]()
    return _from_xml(lmcpNode, obj)
end

function _from_xml(xroot::LightXML.AbstractXMLNode, o::AbstractLmcpMessage)
    type = typeof(o)
    for (n, t) in zip(fieldnames(type), fieldtypes(type))    
         children = xroot[string(n)]
         if length(children) == 0 continue end    
         value = _from_xml(children[1], t)
         setproperty!(o, n, value)
    end
    return o
end
