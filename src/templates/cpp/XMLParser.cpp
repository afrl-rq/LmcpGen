// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

#include "avtas/lmcp/XMLParser.h"
#include <fstream>
#include <sstream>
#include <iostream>
#include <stdlib.h>
#include <string>
#include <cstdint>

namespace avtas {
namespace lmcp {

    XMLParser :: XMLParser() {}

    XMLParser :: ~XMLParser() {}

    /** @brief (one liner)
      *
      * (documentation goes here)
      */
    Node* XMLParser :: parse( std::string filename, bool treatAttributesAsChildren) {
        std::ifstream fin( filename.c_str() );

        if ( !fin ) {
            std::cerr << "could not read file" << filename << std::endl;
            return new Node("");
        }

        Node* node = parse( fin, treatAttributesAsChildren );
        fin.close();
        return node;
    }
    
    Node* XMLParser :: parseString( std::string xmlString, bool treatAttributesAsChildren) {
        std::istringstream fin( xmlString );

        if ( !fin ) {
            std::cerr << "could not read string" << xmlString << std::endl;
            return new Node("");
        }

        Node* node = parse( fin, treatAttributesAsChildren );
        return node;
    }


    /** @brief (one liner)
      *
      * (documentation goes here)
      */
    Node* XMLParser::parse( std::istream &r, bool treatAttributesAsChildren ) {

        Node* rootNode = 0;
        Node* node = 0;

        std::string str = "";

        while ( true) {
            str = readChunk(r);

            if (str == "")
                break;

            // declaration
            if ( startsWith(str, "<?") ) {
                while ( !endsWith(str, "?") ) {
                    str += readChunk(r);
                }
                continue;
            }

            // comment
            if ( startsWith(str, "<!--")) {
                while ( !endsWith(str, "--")) {
                    str += readChunk(r);
                }
                continue;
            }

            // instruction
            if ( startsWith(str, "<!")) {
                continue;
            }

            // cdata node
            if ( startsWith(str, "<![CDATA[") ) {
                while ( !endsWith(str, "]]") ) {
                    str += readChunk(r);
                }
                continue;
            }

            // if this is a text node or close tag, set the element text
            size_t i = str.find("</");
            if ( i != std::string::npos && node != 0) {
                node->setText( str.substr(0, i));
                str = str.substr(i + 1);

                    if ( (str.substr(1) == node->getTagName()) && node->getParent() != 0) {
                        node = node->getParent();
                    }

                continue;
            }

            // regular element node
            if ( startsWith(str, "<") ) {

                str = str.substr(1);
                if (rootNode == 0) {
                    rootNode = new Node("");
                    node = rootNode;
                }
                else {
                    Node* tmpNode = node->addChild("");
                    node = tmpNode;
                }

                size_t splitPt = str.find(" ");
                if ( splitPt != std::string::npos) {
                    std::string tag = str.substr(0, splitPt);
                    node->setTagName(tag);
                    putAttributes( str.substr(splitPt), node, treatAttributesAsChildren);
                }
                else {
                    node->setTagName(str);
                }

                if ( str.rfind("/") == str.length() - 1 && node->getParent() != 0) {
                    node = node->getParent();
                }
            }

        }

        return rootNode;

    }

    /** @brief (one liner)
      *
      * (documentation goes here)
      */
    std::string XMLParser::readChunk( std::istream &r ) {

        std::string buf;
        try {
            int ch = r.get();
            while ( ((char) ch) != '>') {
                if ( ch == -1) {
                    if (buf.length() > 0) {
                        throw 0;
                    }
                    return "";
                }
                if ( !isWhitespace( (char) ch) || buf.length() > 0 ) {
                    buf += (char) ch;
                }
                ch = r.get();
            }

        } catch (...) {
            return "";
        }

        return buf;
    }

    /** @brief (one liner)
      *
      * (documentation goes here)
      */
    void XMLParser::subChars( std::string &srcStr ) {

        size_t loc = srcStr.find( "&lt" );
        while ( loc != std::string::npos ) {
            srcStr.replace( loc, 3, "<" );
            loc = srcStr.find( "&lt" );
        }

        loc = srcStr.find( "&gt" );
        while ( loc != std::string::npos ) {
            srcStr.replace( loc, 3, ">" );
            loc = srcStr.find( "&gt" );
        }

        loc = srcStr.find( "&amp" );
        while ( loc != std::string::npos ) {
            srcStr.replace( loc, 4, "&" );
            loc = srcStr.find( "&amp" );
        }

        loc = srcStr.find( "&apos" );
        while ( loc != std::string::npos ) {
            srcStr.replace( loc, 5, "'" );
            loc = srcStr.find( "&apos" );
        }

        loc = srcStr.find( "&quot" );
        while ( loc != std::string::npos ) {
            srcStr.replace( loc, 5, "\"" );
            loc = srcStr.find( "&quot" );
        }

    }

    /** @brief (one liner)
      *
      * (documentation goes here)
      */
    std::string XMLParser::putAttributes( std::string str, Node* node, bool treatAsChildren ) {

        try {
            while ( str.length() > 0 ) {

                while ( isWhitespace( str[ 0 ] ) ) {
                    str = str.substr( 1, str.length() - 1 );
                }
                size_t nameEnd = str.find( "=\"", 0 );
                if ( nameEnd == std::string::npos )
                    return str;

                size_t attrEnd = str.find( "\"", nameEnd + 3 );
                if ( attrEnd == std::string::npos )
                    throw 1;

                std::string name = str.substr( 0, nameEnd );
                std::string attr = str.substr( nameEnd + 2, attrEnd - nameEnd - 2 );

                //subChars( name );
                //subChars( attr );
				if (treatAsChildren) {
					Node* tmp = node->addChild(name);
					tmp->setText(attr);	
				} 
				else {
                	node->putAttribute( name, attr );
				}
                str = str.substr( attrEnd + 1 );

            }
        }
        catch ( ... ) {
            std::cerr << "error" << std::endl;
        }
        return str;

    }


    /** @brief (one liner)
    *
    * (documentation goes here)
    */
    bool XMLParser :: isWhitespace( char ch ) {

        if ( ch == '\t' || ch == '\n' || ch == '\r' || ch == ' ' ) {
            return true;
        }
        return false;
    }

    bool XMLParser :: startsWith(std::string str, std::string search) {

            return  (str.find(search, 0) == 0);
    }

    bool XMLParser :: endsWith(std::string str, std::string search) {
            size_t searchLimit = str.length() - search.length();
            return ( str.rfind(search, searchLimit) == searchLimit );
    }

}
}
