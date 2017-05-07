// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

#ifndef _AVTAS_LMCP_XMLPARSER_H_
#define _AVTAS_LMCP_XMLPARSER_H_

#include "Node.h"

namespace avtas {
namespace lmcp {


    class XMLParser {

    public:

        XMLParser();
        ~XMLParser();


        static Node* parse (std::istream &reader, bool treatAttributesAsChildren);
        static Node* parse (std::string filename, bool treatAttributesAsChildren);
        static Node* parseString (std::string xmlString, bool treatAttributesAsChildren);
        static bool isWhitespace(char ch);

    private:

        static std::string readChunk(std::istream &reader);

        static void subChars(std::string &srcStr);

        static std::string putAttributes(std::string str, Node* node, bool treatAsChildren);

        static bool startsWith(std::string str, std::string search);

        static bool endsWith(std::string str, std::string search);



    };



}
}
#endif //_AVTAS_LMCP_XMLPARSER_H_
