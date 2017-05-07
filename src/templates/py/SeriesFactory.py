from lmcp.LMCPObject import *
from lmcp.LMCPFactory import *

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

-<import_all>-

class SeriesFactory(LMCPFactory):

    -<series_name_setup>-
    
    def createObject(self, LMCPTypeNumber):
        """
        Returns a new LMCP object of the given type number, or None if the
        type number is not recognized.
        """
        -<series_create_object>-

    def createObjectByName(self, name):
        """
        Returns a new LMCP object based on its name
        """
        -<obj_by_name>-
        return None

    def createObjectFromNode(self, xmlNode):
        for n in xmlNode.childNodes:
            if n.nodeType == xml.dom.Node.ELEMENT_NODE:
                return self.createObjectByName(n.localName)
        return None


def getLMCPName(LMCPTypeNumber):
    """
    Returns a string representation for the given type number.
    """
    -<get_lmcp_name>-



# A default factory for use by  LMCP code
DEFAULT_FACTORY = SeriesFactory()

    

