import socket
from lmcp import LMCPFactory

## ===============================================================================
## Authors: AFRL/RQQA
## Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
## 
## Copyright (c) 2017 Government of the United State of America, as represented by
## the Secretary of the Air Force.  No copyright is claimed in the United States under
## Title 17, U.S. Code.  All Other Rights Reserved.
## ===============================================================================

## This file was auto-created by LmcpGen. Modifications will be overwritten.

-<import_all_messages>-

s = socket.socket()
host = socket.gethostname()
port = 11041
s.connect((host, port))
buf = bytearray()

-<pack_all_messages>-

s.send(buf)


