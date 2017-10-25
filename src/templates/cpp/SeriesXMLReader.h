// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

#ifndef _-<series_name_caps>-_XMLREADER_H_
#define _-<series_name_caps>-_XMLREADER_H_

#include "avtas/lmcp/Object.h"
#include "avtas/lmcp/Node.h"
#include "avtas/lmcp/NodeUtil.h"

#include <iostream>


using namespace avtas::lmcp;

-<open_namespace>-

class -<series_name>-XMLReader {

    /** reads an LMCP XML Storage file and returns a list of LMCPObjects */
    public:

        -<series_name>-XMLReader(void) {}

        virtual ~-<series_name>-XMLReader(){}  

        static avtas::lmcp::Object * visitType(avtas::lmcp::Node* el );

};

-<close_namespace>-

#endif // _-<series_name_caps>-_XMLREADER_H_