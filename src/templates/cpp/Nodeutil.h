// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

#ifndef _AVTAS_LMCP_NODEUTIL_H_
#define _AVTAS_LMCP_NODEUTIL_H_

#include <string>
#include <cstdint>
#include <vector>
#include "avtas/lmcp/Node.h"

namespace avtas {
namespace lmcp {

	class NodeUtil {
		
		public:
		
		/** traverses the parent's childList and returns a new Node
		 *  that contains only the children that have the specified pathName.
		 *  All children are added to the root of the childList
		 */
		static std::vector<Node*> getList(Node* parent, std::string childName);
		
		static std::string get(Node* node, std::string pathName, std::string defaultVal);
		static int32_t getInt(Node* node, std::string pathName, int32_t defaultVal);
		static int64_t getLong(Node* node, std::string pathName, int64_t defaultVal);
		static float getFloat(Node* node, std::string pathName, float defaultVal);
		static double getDouble(Node* node, std::string pathName, double defaultVal);
		static bool getBool(Node* node, std::string pathName, bool defaultVal);
		static std::vector<std::string> splitString(std::string instr, char splitChar);		
	};	
	
}
}

#endif /* _AVTAS_LMCP_NODEUTIL_H_ */
