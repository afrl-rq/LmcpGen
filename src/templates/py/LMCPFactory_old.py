import struct
import LMCPObject
import xml.dom.minidom

## ===============================================================================
## Authors: AFRL/RQQA
## Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
## 
## Copyright (c) 2017 Government of the United State of America, as represented by
## the Secretary of the Air Force.  No copyright is claimed in the United States under
## Title 17, U.S. Code.  All Other Rights Reserved.
## ===============================================================================

## This file was auto-created by LmcpGen. Modifications will be overwritten.

HEADER_SIZE = 22  # don't modify this
CHECKSUM_SIZE = 4 # don't modify this

class LMCPFactory:

    def getObject(self, buffer):
        if len(buffer) < HEADER_SIZE:
            print  "getObject() : buffer too small for message"
            return None
        type = getLMCPType(buffer)
        obj = self.createObject( type )
        if obj != None:
           obj.unpack(buffer, HEADER_SIZE, self)
        return obj

    def getObjFromStream(self, fileobj):
        """
        reads an LMCP object from a file source, such as a socket (or file on the disk)
        """
        header = fileobj.read(HEADER_SIZE)
        msgSize = getSize(header)
        msgBody = fileobj.read(msgSize + CHECKSUM_SIZE)
        if  validate(header + msgBody) != True:
            print "LMCPFactory : bad checksum. "
            return None
        return self.getObject(header + msgBody)

    def createObject(self, LMCPTypeNumber):
        return None

    def createObjectByName(self, name):
        """
        Returns a new LMCP object based on its name
        """
        return None

    def unpackFromXMLNode(self, domNode):
        """
        Reads in an XML node, unpacks objects, adds them to a list and
        returns the list
        """
        objs = []
        for e in domNode.childNodes:
            if e.nodeType == xml.dom.Node.ELEMENT_NODE:
                obj = self.createObjectByName(e.localName)
                if obj != None:
                    obj.unpackFromXMLNode(e, self)
                    objs.append(obj)
        return objs

    def createObjectFromNode(self, xmlNode):
        return None

    def unpackFromXMLString(self, xmlStr):
        """
        Reads in an XML string, unpacks objects, adds them to a list and
        returns the list
        """
        doc = xml.dom.minidom.parseString(xmlStr)
        return self.unpackFromXMLNode(doc.documentElement)

    def unpackFromXMLFile(self, file):
        """
        Reads in an XML document, unpacks objects, adds them to a list and
        returns the list
        """
        doc = xml.dom.minidom.parse(file)
        return self.unpackFromXMLNode(doc.documentElement)


def packMessage(lmcpObject, instanceId, calcChecksum):
    """
    packs a buffer (string) object and returns it
    """
    buffer = ""

    buffer += lmcpObject.SERIES_NAME
    leftover = 10 - len(lmcpObject.SERIES_NAME)
    if leftover < 10:
        buffer += struct.pack(`leftover` + "x")

    buffer += struct.pack(">I", instanceId)
    buffer += struct.pack(">I", lmcpObject.LMCP_TYPE)

    #pack the main object
    obj_buffer = lmcpObject.pack()
    buffer += struct.pack(">I", len(obj_buffer))
    buffer += obj_buffer

    #pack the checksum
    if calcChecksum:
        buffer += struct.pack(">I", calculateChecksum(buffer, 0))
    else:
        buffer += struct.pack(">I", 0)

    return buffer

def getInstanceID(buffer):
    return struct.unpack_from(">I", buffer, 10)[0]

def getLMCPType(buffer):
    return struct.unpack_from(">I", buffer, 14)[0]

def getSize(buffer):
    return struct.unpack_from(">I", buffer, 18)[0]

def calculateChecksum(buffer, offset):
    """
    Calculates the checksum.  This should be called after pack().
    The checksum sums all bytes in the packet between 0 and
    buf.limit() - CHECKSUM_SIZE.
    """
    sum = 0
    for x in range(len(buffer)-offset):
        sum += struct.unpack_from("b", buffer, x)[0] & 0xFF
    return sum

def validate(buffer):
    """
    checks the bytebuffer's checksum value against the calculated checksum
    returns true if the calculated and stored values match, or if the buffer value is
    zero (indicating that checksum was not calculated.  This method rewinds the buffer and
    returns it to LIMIT - 4 bytes (start position of checksum)
    """
    cs = struct.unpack_from(">I", buffer, len(buffer)-5)[0]
    if cs == 0:
        return True
    else:
        return cs == calculateChecksum(buffer, 5)

