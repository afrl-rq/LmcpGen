// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

// This class defines the base class for all LMCP objects

#ifndef _AVTAS_LMCP_OBJECT_H_
#define _AVTAS_LMCP_OBJECT_H_

#include <string>
#include <cstdint>
#include "avtas/lmcp/ByteBuffer.h"

namespace avtas {
namespace lmcp {

   class Factory;

   class Object {
   public:
      // Destructor
      virtual ~Object(void) {}

      // Equals operators
      bool operator==(const Object & that) { return true; };
      bool operator!=(const Object & that) { return false; };

      // Serializes calling object into a ByteBuffer.
      // To be implemented by subclass.
      virtual void pack(ByteBuffer & buf) const {}

      // Deserializes ByteBuffer into calling object.
      // To be implemented by subclass.
      virtual void unpack(ByteBuffer & buf) {}

      // Returns packed size, in bytes, of the object
      // To be implemented by subclass.
      virtual uint32_t calculatePackedSize(void) const { return 0; }

      // Prints the object contents in a human readable format to a string.
      // To be implemented by subclass.
      virtual std::string toString(int32_t depth=0) const { return ""; }

      // Prints the Object contents as an XML String.
      // To be implemented by subclass.
      virtual std::string toXML(int32_t depth =0) { return ""; }

      // Returns the object type id corresponding to MDM
      // To be implemented by subclass.
      virtual uint32_t getLmcpType(void) const = 0;

      // To be implemented by subclass.
      virtual std::string getLmcpTypeName(void) const {
         static std::string s_string("Object");
         return s_string;
      }
	  
      // To be implemented by subclass.
      virtual std::string getFullLmcpTypeName(void) const {
         static std::string s_string("avtas.lmcp.Object");
         return s_string;
      }

      // creates a copy of this object and returns a pointer to it.
      // To be implemented by subclass.
      virtual Object* clone() const { return 0; }

      // To be implemented by subclass.
      virtual std::string getSeriesName(void) const = 0;
      
      // gets the series name as a long value
      virtual int64_t getSeriesNameAsLong(void) const = 0;

      //gets the version number of the series
      virtual uint16_t getSeriesVersion(void) const = 0;

   protected:
      // Constructor
      Object(void) {}

      // Copy Constructor
      Object(const Object & that) {};

      // Assignment Operator
      Object & operator=(const Object & that) { return *this; }
   };

} // end namespace lmcp
} // end namespace avtas

#endif // _AVTAS_LMCP_OBJECT_H_
