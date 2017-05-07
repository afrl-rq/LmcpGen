#! /usr/bin/python

import struct
from lmcp import LMCPObject

## ===============================================================================
## Authors: AFRL/RQQA
## Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
## 
## Copyright (c) 2017 Government of the United State of America, as represented by
## the Secretary of the Air Force.  No copyright is claimed in the United States under
## Title 17, U.S. Code.  All Other Rights Reserved.
## ===============================================================================

## This file was auto-created by LmcpGen. Modifications will be overwritten.

-<list_imports>-

class -<classname>-(-<extends_name>-):

    def __init__(self):
        -<call_superclass_init>-
        self.LMCP_TYPE = -<lmcp_type>-
        self.SERIES_NAME = -<series_name>-
        -<struct_series_name_setup>-
        #Define message fields
        -<define_vars>-

    def pack(self):
        """
        Packs the object data and returns a string that contains all of the serialized
        members.
        """
        buffer = []
        -<pack_vars>-
        return "".join(buffer)

    def unpack(self, buffer, _pos):
        """
        Unpacks data from a string buffer and sets class members
        """
        -<unpack_vars>-

    -<gets_and_sets>-

    def toString(self):
        """
        Returns a string representation of all variables
        """
        -<print_vars>-
        return buf;

    def getLMCPType(self):
        return self.LMCP_TYPE

    def getSeriesName(self):
        return self.SERIES_NAME

    def getSeriesNameID(self):
        return self.SERIES_NAME_ID

    def getSeriesVersion(self):
        return self.SERIES_VERSION

    def toXMLStr(self, ws):
        str = ws + "<-<datatype_name>->\n";
        #str += -<extends_name>-.toXMLMembersStr(self, ws + "  ")
        str += self.toXMLMembersStr(ws + "  ")
        str += ws + "</-<datatype_name>->\n";
        return str

    def toXMLMembersStr(self, ws):
        buf = ""
        -<to_xml_members>-
        return buf
        
