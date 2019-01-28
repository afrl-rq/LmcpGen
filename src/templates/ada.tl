# ada template listing

ONCE ada/avtas/avtas.ads src/avtas.ads
ONCE ada/avtas/lmcp/avtas-lmcp.ads src/avtas/lmcp/avtas-lmcp.ads
ONCE ada/avtas/lmcp/avtas-lmcp-object.ads src/avtas/lmcp/avtas-lmcp-object.ads
ONCE ada/avtas/lmcp/avtas-lmcp-object.adb src/avtas/lmcp/avtas-lmcp-object.adb
ONCE ada/avtas/lmcp/avtas-lmcp-types.ads src/avtas/lmcp/avtas-lmcp-types.ads
ONCE ada/avtas/lmcp/avtas-lmcp-bytebuffers.ads src/avtas/lmcp/avtas-lmcp-bytebuffers.ads
ONCE ada/avtas/lmcp/avtas-lmcp-bytebuffers.adb src/avtas/lmcp/avtas-lmcp-bytebuffers.adb
ONCE ada/avtas/lmcp/avtas-lmcp-factory.ads src/avtas/lmcp/avtas-lmcp-factory.ads
ONCE ada/avtas/lmcp/avtas-lmcp-factory.adb src/avtas/lmcp/avtas-lmcp-factory.adb
ONCE ada/default.gpr default.gpr
ONCE ada/main.adb src/main.adb

PER_MDM ada/namespace.ads  src/-<series_dir>-/-<full_series_name_dashes>-.ads
PER_MDM ada/mdm-factory.ads src/-<series_dir>-/-<full_series_name_dashes>--factory.ads
PER_MDM ada/mdm-factory.adb src/-<series_dir>-/-<full_series_name_dashes>--factory.adb
PER_MDM ada/enumerations.ads src/-<series_dir>-/-<full_series_name_dashes>--enumerations.ads
PER_MDM ada/mdm-object.ads src/-<series_dir>-/-<full_series_name_dashes>--object.ads
PER_MDM ada/mdm-object.adb src/-<series_dir>-/-<full_series_name_dashes>--object.adb


PER_STRUCT	ada/series_object.ads          src/-<series_dir>-/-<full_datatype_name_dashes_lowercase>-.ads
PER_STRUCT	ada/series_object.adb          src/-<series_dir>-/-<full_datatype_name_dashes_lowercase>-.adb