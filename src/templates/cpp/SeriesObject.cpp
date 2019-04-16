// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

#include <cassert>
#include <limits>
#include <sstream>
#include <iomanip>
#include "-<series_dir>-/-<datatype_name>-.h"
-<include_sub_dependencies>-

-<open_namespace>-

   // Subscription string is namespace separated by '.' followed by type name
   const std::string -<datatype_name>-::Subscription = "-<longdatatype_name_dots>-";
   const std::string -<datatype_name>-::TypeName = "-<datatype_name>-";
   const std::string -<datatype_name>-::SeriesName = "-<series_name>-";
   const int64_t -<datatype_name>-::SeriesId = -<series_id>-LL;
   const uint16_t -<datatype_name>-::SeriesVersion = -<series_version>-;
   const uint32_t -<datatype_name>-::TypeId = -<datatype_id>-;
   
   bool is-<datatype_name>-(avtas::lmcp::Object* obj)
   {
      if(!obj) return false;
      if(obj->getSeriesNameAsLong() != -<series_id>-LL) return false;
      if(obj->getSeriesVersion() != -<series_version>-) return false;
      if(obj->getLmcpType() != -<datatype_id>-) return false;
      return true;
   }
   
   bool is-<datatype_name>-(std::shared_ptr<avtas::lmcp::Object>& obj)
   {
      if(!obj) return false;
      if(obj->getSeriesNameAsLong() != -<series_id>-LL) return false;
      if(obj->getSeriesVersion() != -<series_version>-) return false;
      if(obj->getLmcpType() != -<datatype_id>-) return false;
      return true;
   }
   
   std::vector< std::string > -<datatype_name>-Descendants()
   {
       std::vector< std::string > descendants;
       
       -<all_descendants>-
       return descendants;
   }
   
   -<datatype_name>-::-<datatype_name>-(void) : -<full_parent_datatype>-()
   {
      -<initialize_attributes>-
   }
     
   -<datatype_name>-::-<datatype_name>-(const -<datatype_name>- &that) : -<full_parent_datatype>-(that)
   {
        -<copy_initializer_list>-
   }
   
   -<datatype_name>- & -<datatype_name>-::operator=(const -<datatype_name>- &that)
   {
      if (this != &that)
      {
         -<full_parent_datatype>-::operator=(that);
         -<destroy_attributes>-
         -<copy_initializer_list>-
      }
      return *this;
   }

   -<datatype_name>-* -<datatype_name>-::clone() const
   {
        return new -<datatype_name>-(*this);
   }
   
   -<datatype_name>-::~-<datatype_name>-(void)
   {
      -<destroy_attributes>-
   }
  
   void -<datatype_name>-::pack(avtas::lmcp::ByteBuffer & buf) const
   {
      // Call parent's pack method
      -<full_parent_datatype>-::pack(buf);
      // Copy the class into the buffer
      -<pack_attributes>-
   }
   
   void -<datatype_name>-::unpack(avtas::lmcp::ByteBuffer & buf)
   {
      // Call parent's unpack method
      -<full_parent_datatype>-::unpack(buf);
      // Copy the buffer into the class
      -<unpack_attributes>-
   }

   uint32_t -<datatype_name>-::calculatePackedSize(void) const
   {
      uint32_t size = 0;
      size += -<full_parent_datatype>-::calculatePackedSize();
      -<calculate_packed_size>-
      return size;
   }

   std::string -<datatype_name>-::toString(int32_t depth) const
   {
      std::string indent(depth*3, ' ');
      std::ostringstream oss;
      oss << std::setprecision(std::numeric_limits<double>::max_digits10);
      oss << indent << "Object ( -<datatype_name>- ) {\n";
      indent = std::string((++depth)*3, ' ');
      -<tostring_attributes>-
      indent = std::string((--depth)*3, ' ');
      oss << indent << "}\n";
      return oss.str();
   }

   std::string -<datatype_name>-::toXML(int32_t depth)
   {
      std::string ws(depth*3, ' ');
      std::ostringstream str;
      str << std::setprecision(std::numeric_limits<double>::max_digits10);
      -<xml_write_object>-
      return str.str();
   }

   bool -<datatype_name>-::operator==(const -<datatype_name>- & that)
   {
      if( -<full_parent_datatype>-::operator!=(that) )
      {
          return false;
      }
      -<equals_attributes>-
   }

   bool -<datatype_name>-::operator!=(const -<datatype_name>- & that)
   {
      return( !(operator==(that)) );
   }

   -<gets_and_sets_implementation>-
-<close_namespace>-
