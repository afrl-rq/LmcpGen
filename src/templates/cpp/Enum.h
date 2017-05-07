// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

#ifndef _-<namespace_caps>-_-<enum_name_caps>-_H_
#define _-<namespace_caps>-_-<enum_name_caps>-_H_

#include <string>

-<open_namespace>-


   namespace -<enum_name>- {
   enum -<enum_name>- {
       -<gen_enum_fields>-
   };

   // generates a new -<enum_name>- value for the passed string
   inline -<enum_name>- get_-<enum_name>-(std::string str) {
       -<gen_enum_for_string>-
   }


   // generates a string value for the given enum
   inline std::string get_string(-<enum_name>- e) {
       switch(e) {
        -<gen_string_for_enum>-
       }
   }

   }  // namespace -<enum_name>-

-<close_namespace>-

#endif // _-<namespace_caps>-_-<enum_name_caps>-_H_
