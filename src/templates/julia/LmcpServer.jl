## ===============================================================================
## Authors: AFRL/RQQA
## Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
## 
## Copyright (c) 2017 Government of the United State of America, as represented by
## the Secretary of the Air Force.  No copyright is claimed in the United States under
## Title 17, U.S. Code.  All Other Rights Reserved.
## ===============================================================================

## This file was auto-created by LmcpGen. Modifications will be overwritten.

import Sockets
import LMCP

host = "127.0.0.1"
port = 11041

serverSocket = Sockets.listen(port)

while true
    sock = Sockets.accept(serverSocket)
    println("Connected client")
    while  isopen(sock)
        bytes = read(sock)
        objects = LMCP.unpack_messages(bytes)
        for object in objects
            println()
            println(lmcp.to_xml(object))
        end
    end
end
