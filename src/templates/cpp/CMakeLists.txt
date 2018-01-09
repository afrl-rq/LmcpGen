cmake_minimum_required(VERSION 3.0)
project(LMCP)

set(LIB lmcp)
add_library( ${LIB} STATIC
    avtas/lmcp/ByteBuffer.cpp
    avtas/lmcp/Factory.cpp
    avtas/lmcp/NodeUtil.cpp
    avtas/lmcp/Node.cpp
    avtas/lmcp/XMLParser.cpp
    -<cmake_source_list>-)

if (WIN32)
    target_compile_definitions( ${LIB}
        PUBLIC  "WIN32"
    )
else()
    target_compile_definitions( ${LIB}
        PUBLIC  "LINUX"
    )
    target_compile_options( ${LIB}
        PUBLIC  "-std=c++11"
    )
endif()

target_include_directories( ${LIB}
    PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}"
)

install(TARGETS ${LIB} DESTINATION lib)
