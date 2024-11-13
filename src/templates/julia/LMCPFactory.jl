## ===============================================================================
## Authors: AFRL/RQQA
## Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
## 
## Copyright (c) 2017 Government of the United State of America, as represented by
## the Secretary of the Air Force.  No copyright is claimed in the United States under
## Title 17, U.S. Code.  All Other Rights Reserved.
## ===============================================================================

## This file was auto-created by LmcpGen. Modifications will be overwritten.


const LMCP_CONTROL_SEQUENCE = 0x4c4d4350

const OBJECT_ID_REGISTRY = Dict{Tuple{Int64, UInt32}, DataType}() 
const TEXT_CREATION_REGISTRY = Dict{Tuple{String, String}, DataType}()

const LARGE_ARRAY_REGISTRY = Set{Tuple{DataType, Symbol}}()

function calculate_checksum(bytes::AbstractVector{UInt8})
    checksum::UInt64 = 0
    for byte in bytes
        checksum += byte
    end
    return UInt32(checksum)
end


function pack_message(o::AbstractLmcpMessage)

    buffer = IOBuffer()
    write(buffer, hton(LMCP_CONTROL_SEQUENCE))

    innerBuffer = IOBuffer()
    write(innerBuffer, o)
    write(buffer, hton(UInt32(innerBuffer.size)))
    write(buffer, innerBuffer.data[1:innerBuffer.size])

    #checksum
    checksum = calculate_checksum(buffer.data[1:buffer.size])
    write(buffer, hton(checksum))


    return buffer.data[1:buffer.size]
end

function validate(bytes::AbstractVector{UInt8})
    #retrieves the checksum value in BIG_ENDIAN order
    cs::UInt64 = 0
    len = length(bytes)
    cs |= (bytes[len-3]) 
    cs <<= 8
    cs |= (bytes[len-2])
    cs <<= 8
    cs |= (bytes[len-1])
    cs <<= 8
    cs |= (bytes[len-0])

    return (cs == 0) || (calculate_checksum(bytes[1:length(bytes) - 4]) == cs);
end

function unpack_message(buffer::IOBuffer)
    controlSequence = ntoh(read(buffer, Int32))
    length = ntoh(read(buffer, UInt32))

    message = read(buffer, length)
    checksum = ntoh(read(buffer, UInt32))

    messageBuffer = IOBuffer(message)

    return read(messageBuffer, AbstractLmcpMessage)
end

function unpack_message(bytes::AbstractVector{UInt8})
    buffer = IOBuffer(bytes)
    return unpack_message(buffer)
end

function unpack_message(str::String)
    buffer = IOBuffer(str)
    return unpack_message(buffer)
end

function unpack_messages(bytes::AbstractVector{UInt8})
    buffer = IOBuffer(bytes)
    messages = Vector{LmcpBaseInterface}()
    while bytesavailable(buffer) > 0
        push!(messages, unpack_message(buffer))
    end
    return messages
end

function unpack_messages(msgs::String)
    bytes = Vector{UInt8}(transcode(UInt8, msgs))
    return unpack_messages(bytes)
end

function to_xml(o::AbstractLmcpMessage; extraAttributes...)
    xdoc = LightXML.XMLDocument()
    xroot = LightXML.create_root(xdoc, lmcp_type(o))
    LightXML.set_attributes(xroot, extraAttributes)
    xroot = _to_xml(xroot, o)
    ret = string(xdoc)
    LightXML.free(xdoc)
    return ret
end

function from_xml(xroot::LightXML.XMLElement)
    name = LightXML.name(xroot)
    series = LightXML.attribute(xroot, "Series", required=true)

    obj = TEXT_CREATION_REGISTRY[(series, name)]()
    _from_xml(xroot, obj)

end

function from_xml(root::String)
    if isempty(root)
        return nothing
    end
    xdoc = LightXML.parse_string(root)
    xroot = LightXML.root(xdoc)

    ret = from_xml(xroot)

    LightXML.free(xdoc)
    return ret
end
