## ===============================================================================
## Authors: AFRL/RQQA
## Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
## 
## Copyright (c) 2017 Government of the United State of America, as represented by
## the Secretary of the Air Force.  No copyright is claimed in the United States under
## Title 17, U.S. Code.  All Other Rights Reserved.
## ===============================================================================

## This file was auto-created by LmcpGen. Modifications will be overwritten.

module -<enumType>-Module

"""
-<doc_string_enum>-
"""
@enum -<enumType>- begin
    -<enum_gen_entries>-
end

function -<enumType>-(str::String)
    """
    Returns a numerical value from a string
    """
    -<enum_from_string>-
end


end