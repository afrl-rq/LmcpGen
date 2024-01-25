## ===============================================================================
## Authors: AFRL/RQQA
## Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
## 
## Copyright (c) 2017 Government of the United State of America, as represented by
## the Secretary of the Air Force.  No copyright is claimed in the United States under
## Title 17, U.S. Code.  All Other Rights Reserved.
## ===============================================================================

## This file was auto-created by LmcpGen. Modifications will be overwritten.

#example using the JAVA style sentinal strings.

#import ZMQ
import Sockets
#import Pkg
#Pkg.activate(".")
import LMCP

port = 5555

function serialize(o::lmcp.LmcpBaseInterface)

    attributes = lmcp.FULL_LMCP_TYPE_NAME(o) * "\$lmcp|" * lmcp.FULL_LMCP_TYPE_NAME(o) * "||0|0\$"
    smsg = LMCP.pack_message(o)

    sentinel = "+=+=+=+="
    sentinel *= string(length(attributes) + length(smsg))
    sentinel *= "#@#@#@#@"
    addressedPayload = Vector{UInt8}(attributes)
    append!(addressedPayload, smsg)

    # sentinelized checksum
    val::UInt32 = 0
    for byte in addressedPayload
        val += byte
    end

    footer = "!%!%!%!%" * string(val) * "?^?^?^?^"

    totalmsg = Vector{UInt8}(transcode(UInt8, sentinel))
    append!(totalmsg, addressedPayload)
    append!(totalmsg, transcode(UInt8, footer))

    return totalmsg
end

function deserialize(bytes)
    stringOutput = transcode(String, bytes)
    tokens = split(stringOutput, "\$")
    address = tokens[1]
    attributes = tokens[2]
    msg = join(tokens[3:length(tokens)], "\$") #SOMETIMES '$' is valid in the data. Just reconstruct everything back.
    lmcpMessage = LMCP.unpack_message(msg)
    return lmcpMessage
end

function send(socket, obj)
    bytes = serialize(obj)
    write(socket, bytes)
end

function run()
    sock = Sockets.connect(port)

    while  isopen(sock)
        bytes = readavailable(sock)
        lmcpMessage = deserialize(bytes)
        println(LMCP.to_xml(lmcpMessage))
    end
end

run()
#client
