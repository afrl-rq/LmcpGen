// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

#include "avtas/lmcp/Node.h"
#include <algorithm>
#include <iterator>
#include <iostream>
#include "avtas/lmcp/NodeUtil.h"

namespace avtas {
namespace lmcp {
    
    Node :: Node() {
        this->name = "";
        this->text = "";
        this->parentNode = 0;
    }
    
    Node :: Node(std::string tagName) {
        this->name = tagName;
        this->text = "";
        this->parentNode = 0;
    }
    
    Node :: Node(std::string tagName, std::string text) {
        this->name = tagName;
        this->text = text;
        this->parentNode = 0;
    }
    
    Node :: Node( Node& node) {
        
        name = node.getTagName();
        text = node.getText();
        
        size_t cnt = node.getAttributeCount();
        std::string* arry = new std::string[cnt] ;
        node.getAttributeNames(arry);
        
        for(uint32_t i=0; i<cnt; i++) {
            putAttribute(arry[i], node.getAttribute(arry[i]));
        }
        
        cnt = node.getChildCount();
        
        for(uint32_t i=0; i<cnt; i++) {
            Node* tmp = new Node( *node.getChild(i) );
            addChild( tmp );
        }
        
        delete [] arry;
        
    }
    
    Node :: ~Node() {
        for(uint32_t i=0; i<childList.size(); i++)
            delete childList[i];
		childList.clear();
    }
    
    
    /** @brief (one liner)
     *
     * (documentation goes here)
     */
    std::string Node::getTagName(){
        return this->name;
    }
    
    /** @brief (one liner)
     *
     * (documentation goes here)
     */
    void Node::setTagName(std::string name) {
        this->name = name;
    }
    
    /** @brief (one liner)
     *
     * (documentation goes here)
     */
    Node* Node::addChild(std::string tagName) {
        Node* child = new Node(tagName);
        return addChild(child);
    }
    
    Node* Node :: addChild(Node* child) {
        childList.push_back(child);
        child->setParent( this );
        return child;
    }
    
    /** @brief (one liner)
     *
     * (documentation goes here)
     */
    size_t Node :: getChildCount() {
        return childList.size();
    }
    
    Node* Node :: getChild(uint32_t index) {
        if (index < getChildCount())
            return childList[index];
        
        return 0;
    }
    
    
    /**
     * Returns the first child encountered with specified name, or null
     * if none is found.  To find a nested child, specify the childname
     * as tags separated by "/"
     */
    Node* Node :: getChild(std::string childName) {
        
        Node* tmp = this;
        
        size_t splitPt = childName.find_first_of("/");
        std::string tail = "";
        
        if (splitPt != std::string::npos) {
            tail = childName.substr(splitPt + 1);
            childName = childName.substr(0, splitPt);
        }
        
        for(uint32_t i=0; i<tmp->childList.size(); i++) {
            if ( tmp->childList[i]->getTagName() == childName){
                if (tail != "")
                    return childList[i]->getChild(tail);
                else
                    return childList[i];
            }
        }
        return 0;
    }

    
    /**
     * Returns a vector containing all children encountered with specified name, or null
     * if none are found.
     */
    std::vector<Node*> Node :: getChildren(std::string childName) {

        std::vector <Node*> list;

        Node* tmp = this;
        
        size_t splitPt = childName.find_first_of("/");
        std::string tail = "";
        
        if (splitPt != std::string::npos) {
            tail = childName.substr(splitPt + 1);
            childName = childName.substr(0, splitPt);
        }
        
        for(uint32_t i=0; i<tmp->childList.size(); i++) {
            if ( tmp->childList[i]->getTagName() == childName){
                if (tail != "") {
                    std::vector <Node*> sublist = childList[i]->getChildren(tail);
                    for(uint32_t j=0; j<sublist.size(); j++) {
                        list.push_back( sublist[j]);
                    }
                } else {
                    list.push_back( childList[i]);
                }
            }
        }
        
        return list;
        
    }
    
    
    /** @brief (one liner)
     *
     * (documentation goes here)
     */
    void Node::putAttribute(std::string name, std::string val) {
        attrMap.insert( std::pair<std::string, std::string> (name, val) );
    }
    
    /** @brief (one liner)
     *
     * (documentation goes here)
     */
    std::string Node::getAttribute(std::string name) {
        if ( attrMap.count(name) == 1) {
            return attrMap[name];
        }
        return 0;
    }
    
    /** @brief (one liner)
     *
     * (documentation goes here)
     */
    void Node::getAttributeNames( std::string* storeArray ) {
        std::map<std::string, std::string> :: const_iterator iter;
        int i=0;
        
        for( iter = attrMap.begin(); iter != attrMap.end(); ++iter) {
            storeArray[i] =  iter->first;
            i++;
        }
    }
    
    /** @brief (one liner)
     *
     * (documentation goes here)
     */
    size_t Node :: getAttributeCount(){
        
        return this->attrMap.size();
    }
    
    /** @brief (one liner)
     *
     * (documentation goes here)
     */
    std::string Node::getText(){
        return this->text;
    }
    
    /** @brief (one liner)
     *
     * (documentation goes here)
     */
    void Node::setText(std::string text) {
        this->text = text;
    }
    
    /** @brief (one liner)
     *
     * (documentation goes here)
     */
    Node* Node::getParent(){
        return this->parentNode;
    }
    
    /** @brief (one liner)
     *
     * (documentation goes here)
     */
    void Node::setParent(Node *parentNode) {
        this->parentNode = parentNode;
    }
    
    bool Node :: remove(Node* node) {
        
        std::vector <Node*> :: iterator it;
        
        for( it = childList.begin(); it<childList.end(); it++) {
            
            if ( *it == node ) {
                childList.erase(it);
                return true;
            }
        }
        return false;
    }

    
    /** @brief (one liner)
     *
     * (documentation goes here)
     */
    std::string Node::toString() {
        std::string ret = "<" + getTagName() + " ";
        std::string* attrNames = new std::string[getAttributeCount()];
        getAttributeNames( attrNames );
        
        for(uint32_t i=0; i<getAttributeCount(); i++) {
            ret += (attrNames[i] + "=" + getAttribute(attrNames[i]) + " ");
        }
        ret += ">";
        
        for(uint32_t i=0; i<childList.size(); i++) {
            ret += "\n";
            ret += childList[i]->toString();
        }
        
        if (getText() != "") {
            ret += ("\n  " + getText() );
        }
        
        ret += "\n</" + getTagName() + " ";
        delete [] attrNames;
        return ret;
    }
    
    
}
}
