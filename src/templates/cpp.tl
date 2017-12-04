# C++ template listing

ONCE            cpp/ByteBuffer.h            avtas/lmcp/ByteBuffer.h
ONCE            cpp/ByteBuffer.cpp          avtas/lmcp/ByteBuffer.cpp
ONCE            cpp/Object.h                avtas/lmcp/Object.h
ONCE            cpp/Factory.h               avtas/lmcp/Factory.h
ONCE            cpp/Factory.cpp             avtas/lmcp/Factory.cpp
ONCE            cpp/Node.cpp                avtas/lmcp/Node.cpp
ONCE            cpp/Node.h                  avtas/lmcp/Node.h
ONCE            cpp/NodeUtil.cpp            avtas/lmcp/NodeUtil.cpp
ONCE            cpp/Nodeutil.h              avtas/lmcp/NodeUtil.h
ONCE            cpp/XMLParser.cpp           avtas/lmcp/XMLParser.cpp
ONCE            cpp/XMLParser.h             avtas/lmcp/XMLParser.h
ONCE            cpp/LmcpXMLReader.h         avtas/lmcp/LmcpXMLReader.h
ONCE            cpp/Makefile                Makefile
ONCE            cpp/meson.build             meson.build
ONCE            cpp/LMCP.vcproj             LMCP.vcproj
ONCE            cpp/LMCP.vcxproj            LMCP.vcxproj
ONCE            cpp/LMCP.vcxproj.filters    LMCP.vcxproj.filters
#ONCE           cpp/Readme.html             Readme.html

PER_MDM         cpp/EntireSeriesHeader.h    -<series_dir>-/-<series_name>-.h
PER_MDM         cpp/SeriesEnum.h            -<series_dir>-/-<series_name>-Enum.h
PER_MDM         cpp/SeriesXMLReader.cpp     -<series_dir>-/-<series_name>-XMLReader.cpp
PER_MDM         cpp/SeriesXMLReader.h       -<series_dir>-/-<series_name>-XMLReader.h
PER_MDM         cpp/SeriesFactory.cpp       -<series_dir>-/-<series_name>-Factory.cpp
PER_MDM         cpp/SeriesFactory.h         -<series_dir>-/-<series_name>-Factory.h

PER_STRUCT	cpp/SeriesObject.h          -<series_dir>-/-<datatype_name>-.h
PER_STRUCT	cpp/SeriesObjectDescend.h   -<series_dir>-/-<datatype_name>-Descendants.h
PER_STRUCT	cpp/SeriesObject.cpp        -<series_dir>-/-<longdatatype_name>-.cpp

PER_ENUM        cpp/Enum.h                  -<series_dir>-/-<enum_name>-.h

ONCE            cpp/serverMain.cpp          test/server/main.cpp
ONCE            cpp/TestServer.vcproj       test/server/TestServer.vcproj
ONCE            cpp/TestServer.vcxproj      test/server/TestServer.vcxproj
ONCE            cpp/test_server_makefile    test/server/Makefile
ONCE            cpp/clientMain.cpp          test/client/main.cpp
ONCE            cpp/TestClient.vcproj       test/client/TestClient.vcproj
ONCE            cpp/TestClient.vcxproj      test/client/TestClient.vcxproj
ONCE            cpp/test_client_makefile    test/client/Makefile
