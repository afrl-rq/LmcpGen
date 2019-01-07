# ada template listing

ONCE ada/avtas/avtas.ads src/avtas.ads
ONCE ada/avtas/lmcp/avtas-lmcp.ads src/avtas/lmcp/avtas-lmcp.ads
ONCE ada/avtas/lmcp/avtas-lmcp-object.ads src/avtas/lmcp/avtas-lmcp-object.ads
ONCE ada/avtas/lmcp/avtas-lmcp-types.ads src/avtas/lmcp/avtas-lmcp-types.ads
# ONCE ada/default.gpr default.gpr

PER_MDM ada/namespace.ads  src/-<series_dir>-/-<full_series_name_dashes>-.ads
PER_MDM ada/enumerations.ads src/-<series_dir>-/-<full_series_name_dashes>--enumerations.ads
PER_MDM ada/mdm_object.ads src/-<series_dir>-/-<full_series_name_dashes>--object.ads

PER_STRUCT	ada/series_object.ads          src/-<series_dir>-/-<full_datatype_name_dashes>-.ads
PER_STRUCT	ada/series_object.adb          src/-<series_dir>-/-<full_datatype_name_dashes>-.adb