## ===============================================================================
## Authors: AFRL/RQQA
## Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
## 
## Copyright (c) 2017 Government of the United State of America, as represented by
## the Secretary of the Air Force.  No copyright is claimed in the United States under
## Title 17, U.S. Code.  All Other Rights Reserved.
## ===============================================================================

## This file was auto-created by LmcpGen. Modifications will be overwritten.

module LMCP

import LightXML
import Base: ==, write, read

using PrecompileTools

include("AbstractLmcpMessage.jl")
include("LMCPFactory.jl")

#MDM submodules
-<root_module_includes>-

@compile_workload begin
   include("precompilePack.jl")
   include("precompileXML.jl")
end
end