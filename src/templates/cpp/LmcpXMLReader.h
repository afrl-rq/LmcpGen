// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

#ifndef _AVTAS_LMCP_LMCPXMLREADER_H_
#define _AVTAS_LMCP_LMCPXMLREADER_H_

#include "avtas/lmcp/Object.h"
#include "avtas/lmcp/Node.h"
#include "avtas/lmcp/NodeUtil.h"
#include <vector>
#include <string>
#include <cstdint>
#include <iostream>
#include "avtas/lmcp/XMLParser.h"

-<xml_include_readers>-

namespace avtas {
namespace lmcp {
namespace xml {

        inline avtas::lmcp::Object* readXML(Node* node) {
            if (node == nullptr) return nullptr;
            -<xml_visit_series>-
            return nullptr;
        }

        /** reads an LMCP XML string and returns an LMCP object */
        inline avtas::lmcp::Object* readXML(std::string input) {
            Node* el = avtas::lmcp::XMLParser::parseString(input, false);
            return readXML(el);
        }

        inline bool get_bool(Node* node) {
            return node->getBool(false);
        }

        inline char get_byte(Node* node) {
            return (char) node->getInt(0);
        }
        
        inline char get_char(Node* node) {
            std::string str = node->getText();
            return (char) str.size() == 0 ? 0 : str[0];
        }

        inline int16_t get_int16(Node* node) {
            return (int16_t) node->getInt(0);
        }

        inline uint16_t get_uint16(Node* node) {
            return (uint16_t) node->getInt(0);
        }

        inline int32_t get_int32(Node* node) {
            return node->getInt(0);
        }

        inline uint32_t get_uint32(Node* node) {
            return (uint32_t) node->getLong(0);
        }

        inline int64_t get_int64(Node* node) {
            return node->getLong(0);
        }

        inline float get_real32(Node* node) {
            return node->getFloat(0);
        }

        inline double get_real64(Node* node) {
            return node->getDouble(0);
        }

        inline std::string get_string(Node* node) {
            return node->getText();
        }

} // end namespace xml
} // end namespace lmcp
} // end namespace avtas

#endif //_AVTAS_LMCP_LMCPXMLREADER_H

