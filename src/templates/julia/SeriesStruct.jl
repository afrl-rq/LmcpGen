## ===============================================================================
## Authors: AFRL/RQQA
## Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
## 
## Copyright (c) 2017 Government of the United State of America, as represented by
## the Secretary of the Air Force.  No copyright is claimed in the United States under
## Title 17, U.S. Code.  All Other Rights Reserved.
## ===============================================================================

## This file was auto-created by LmcpGen. Modifications will be overwritten.

#LMCP_TYPE = "-<lmcp_type>-"
#SERIES_NAME = "-<series_name>-"
#FULL_LMCP_TYPE_NAME = "-<full_datatype_name>-"

#LMCP_TYPE_ID = -<lmcp_type_id>-
#SERIES_NAME_ID = -<series_name_id>-
#SERIES_VERSION = -<series_version>-

-<struct_imports>-


abstract type Abstract-<typeName>- <: -<extends_series>-Abstract-<extends_impl>- end

"""
-<doc_string_struct>-
"""
mutable struct -<typeName>- <: Abstract-<typeName>-
    -<define_vars>-
end


#empty default constructor for ease of use if needed
-<define_defaults>-


full_lmcp_type_name(::Type{-<typeName>-}) = "-<full_datatype_name>-"
lmcp_type(::Type{-<typeName>-}) = "-<lmcp_type>-"
lmcp_type_id(::Type{-<typeName>-}) = UInt32(-<lmcp_type_id>-)
series_name(::Type{-<typeName>-}) = "-<series_name>-"
series_name_id(::Type{-<typeName>-}) = Int64(-<series_name_id>-)
series_version(::Type{-<typeName>-}) = UInt16(-<series_version>-)

LMCP.OBJECT_ID_REGISTRY[(-<series_name_id>-, -<lmcp_type_id>-)] = -<typeName>-
LMCP.TEXT_CREATION_REGISTRY[("-<series_name>-", "-<lmcp_type>-")] = -<typeName>-

-<check_large_arrays>-