// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

#include "avtas/lmcp/NodeUtil.h"
#include "avtas/lmcp/Node.h"
#include "avtas/lmcp/XMLParser.h"

#include <vector>
#include <string>
#include <cstdint>
#include <iostream>

namespace avtas {
namespace lmcp {
    
    /** returns a list of nodes that contain the childName */
    std::vector <Node*> NodeUtil :: getList(Node* parent, std::string childName) {
        
        if (parent == 0 ) return std::vector <Node*> ();
        
        return parent->getChildren(childName);
    }
    
    std::string NodeUtil :: get( Node* parent, std::string pathName, std::string defaultVal) {
        
        if ( parent == 0 ) return defaultVal;
        
        Node* node = parent->getChild(pathName);
        
        if ( node == 0 ) return defaultVal;
        
        return node->getText();
    }
    
    int NodeUtil :: getInt(Node* parent, std::string pathName, int32_t defaultVal) {
        
        if ( parent == 0 ) return defaultVal;
        
        Node* node = parent->getChild(pathName);
        
        if ( node == 0 ) return defaultVal;
        
        return atoi(node->getText().c_str());
    }
    
    int64_t NodeUtil :: getLong(Node* parent, std::string pathName, int64_t defaultVal) {
        
        if ( parent == 0 ) return defaultVal;
        
        Node* node = parent->getChild(pathName);
        
        if ( node == 0 ) return defaultVal;
        
        return atoll(node->getText().c_str());
    }
    
    double NodeUtil :: getDouble(Node* parent, std::string pathName, double defaultVal) {
        
        if ( parent == 0 ) return defaultVal;
        
        Node* node = parent->getChild(pathName);
        
        if ( node == 0 ) return defaultVal;
        
        return atof(node->getText().c_str());
    }
    
    float NodeUtil :: getFloat(Node* parent, std::string pathName, float defaultVal) {
        
        if ( parent == 0 ) return defaultVal;
        
        Node* node = parent->getChild(pathName);
        
        if ( node == 0 ) return defaultVal;
        
        return (float) atof(node->getText().c_str());
    }
    
    bool NodeUtil :: getBool(Node* parent, std::string pathName, bool defaultVal) {
        
        if ( parent == 0 ) return defaultVal;
        
        Node* node = parent->getChild(pathName);
        if ( node == 0 ) return defaultVal;
        
        std::string text = node->getText();
        
        for(size_t i=0; i<text.length(); i++) {
            text[i] = toupper(text[i]);
        }
        
        if ( text == "TRUE") return true;
        if ( text == "FALSE") return false;
        
        return defaultVal;
    }
    
    std::vector<std::string> NodeUtil :: splitString(std::string inStr, char splitChar) {
        
        std::vector<std::string> retV;
        
        size_t startLoc = 0;
        
        const char* charStr = inStr.c_str();
        size_t len = inStr.length();
        
        size_t i = 0;
        while( i < len ) {
            
            //trim leading delimeters
            while( i < len && ( charStr[i] == splitChar || XMLParser::isWhitespace(charStr[i])) ) {
                i++;
            }
            
            startLoc = i;
            i++;
            
            while( i < len && charStr[i] != splitChar && !XMLParser::isWhitespace(charStr[i]) ) {
                i++;
            }
            
            if ( i > len) break;
            
            retV.push_back( inStr.substr(startLoc, i - startLoc));
            
        }
        
        return retV;
        
    }
    
}
}

