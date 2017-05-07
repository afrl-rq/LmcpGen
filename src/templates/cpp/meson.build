project('LMCP', 'cpp')

os = target_machine.system()
if os == 'linux'
  add_project_arguments('-DLINUX', language: ['c', 'cpp'])
endif

lib = static_library('lmcp', [
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
