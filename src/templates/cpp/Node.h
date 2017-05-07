// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

#ifndef _AVTAS_LMCP_NODE_H_
#define _AVTAS_LMCP_NODE_H_

#include <vector>
#include <map>
#include <iostream>
#include <string>
#include <cstdint>
#include <cstdlib>

namespace avtas {
namespace lmcp {

    class Node {

    public:

        Node();

        Node( std::string tagName);

        Node( std::string name, std::string text);
        
        Node( Node& );

        ~Node();

        std::string getTagName();

        void setTagName(std::string name);

        Node* addChild(std::string tagName);
        
        Node* addChild(Node* child);

        size_t getChildCount();
        
        Node* getChild(std::string childName);

        std::vector <Node*> getChildren(std::string childName);
        
        Node* getChild(unsigned int index);

        void putAttribute(std::string name, std::string val);

        std::string getAttribute(std::string name);

        void getAttributeNames(std::string* storeArray);

        size_t getAttributeCount();

        std::string getText();

        void setText( std::string text);

        Node* getParent();

        void setParent(Node *parentNode);

        std::string toString();
        
        bool remove(Node* node);

        /* convenience methods to get node text value */
        int32_t getInt(int32_t defaultVal) { return atoi(getText().c_str()); }
	
        int64_t getLong(int64_t defaultVal) { return atoll(getText().c_str()); }
	
        float getFloat(float defaultVal) { return static_cast<float>(atof(getText().c_str())); }
	
        double getDouble(double defaultVal) { return atof(getText().c_str()); }
	
        bool getBool(bool defaultVal) {
            std::string text = getText();
        
            for(unsigned int i=0; i<text.length(); i++) {
                text[i] = toupper(text[i]);
            }
        
            if ( text == "TRUE") return true;
            if ( text == "FALSE") return false;
        
            return defaultVal;
        }
	
        std::vector<std::string> splitString(std::string instr, char splitChar);

    private:
        
        std::string name;
        std::string text;
        std::map <std::string, std::string> attrMap;
        std::vector <Node*> childList;

        Node* parentNode;

    };

}
}

#endif // _AVTAS_LMCP_NODE_H_
