# AADL template listing

# Include the common data types
ONCE            aadl/Common_Data.aadl       Common_Data.aadl

# For every MDM add a collection of all associated data types
PER_MDM         aadl/EntireMdm.aadl         -<series_name>-.aadl
