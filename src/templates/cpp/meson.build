incs_lmcp = include_directories('.')

lib_lmcp = static_library(
  'lmcp',
  [
    'avtas/lmcp/ByteBuffer.cpp',
    'avtas/lmcp/Factory.cpp',
    'avtas/lmcp/Node.cpp',
    'avtas/lmcp/NodeUtil.cpp',
    'avtas/lmcp/XMLParser.cpp',
    -<meson_source_list>-
  ],
  cpp_args: [
    '-std=c++11',
    '-Wall',
  ]
)
