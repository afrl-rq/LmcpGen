// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
//
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

#include "avtas/lmcp/Factory.h"
#include "avtas/lmcp/Object.h"
#include "avtas/lmcp/LmcpXMLReader.h"

#include <cstdint>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

std::string GetExtension(const std::string& filename)
{
	auto dot_pos = filename.find_last_of('.');
	auto sep_pos = std::min(filename.find_last_of('/'), filename.find_last_of('\\'));

	// valid when last '.' is after last separator {'\', '//'} for paths
	return ((dot_pos != std::string::npos) && ((sep_pos == std::string::npos) || (dot_pos > sep_pos)))
		? filename.substr(dot_pos+1) : std::string();
}

int main(int argc, char* argv[])
{
	std::string filename;
	bool enableChecksum = false;

	if (argc == 2)
	{
		if (std::strcmp(argv[1], "--checksum") == 0)
		{
			enableChecksum = true;
		}
		else if ((std::strcmp(argv[1], "-h") == 0) || (std::strcmp(argv[1], "--help") == 0))
		{
			std::cout << "Verify generated C++ LMCP classes against reference lmcp message set, which\n";
			std::cout << "includes serialized xml and binary representations.\n\n";
			std::cout << "Usage: verify [options]\n";
			std::cout << "   FILE(s) read from standard input (e.g. \'find reference/ -type f | ./verify\')\n\n";
			std::cout << "options:\n";
			std::cout << "   -h, --help       Display this information\n";
			std::cout << "   -c, --checksum   Compute checksum when packing messages\n";

			return 0;
		}
	}

	while (std::getline(std::cin, filename))
	{
		auto ext = GetExtension(filename);

		if (ext.empty()) // binary
		{
			std::ifstream ifs(filename, std::ios::binary);

			if (ifs.is_open())
			{
				std::cout << "Checking '" << filename << "'" << std::endl;

				std::vector<uint8_t> buffer((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());

				avtas::lmcp::ByteBuffer refByteBuffer;
				refByteBuffer.allocate(buffer.size());
				memcpy(refByteBuffer.array(), buffer.data(), buffer.size());

				std::unique_ptr<avtas::lmcp::Object> msg(avtas::lmcp::Factory::getObject(refByteBuffer));

				if (msg)
				{
					std::unique_ptr<avtas::lmcp::ByteBuffer> testByteBuffer(avtas::lmcp::Factory::packMessage(msg.get(), enableChecksum));

					if (refByteBuffer.capacity() == testByteBuffer->capacity())
					{
						for (size_t i=0; i < refByteBuffer.capacity(); i++)
						{
							if (testByteBuffer->get(i) != refByteBuffer.get(i))
							{
								std::cerr << "    Error: binary -> msg -> binary doesn't match reference" << std::endl;
								break;
							}
						}
					}
					else
					{
						std::cerr << "    Error: binary -> msg -> binary doesn't match reference" << std::endl;
					}
				}
				else
				{
					std::cerr << "    Error: unable to construct message object from '" << filename << "'" << std::endl;
				}

				ifs.close();
			}
		}
		else if (ext.compare("xml") == 0)
		{
			std::ifstream ifs(filename);

			if (ifs.is_open())
			{
				std::cout << "Checking '" << filename << "'" << std::endl;

				std::stringstream buffer;
				buffer << ifs.rdbuf();
				auto refXML = buffer.str();

				std::unique_ptr<avtas::lmcp::Object> testMsg(avtas::lmcp::xml::readXML(refXML));

				if (testMsg)
				{
					if (testMsg->toXML().compare(refXML) != 0)
					{
						std::cerr << "    Error: xml -> msg -> xml doesn't match reference" << std::endl;
					}
				}
				else
				{
					std::cerr << "    Error: unable to construct message object from '" << filename << "'" << std::endl;
				}

				ifs.close();
			}
		}
		else
		{
			std::cerr << "Ignoring '" << filename << "'" << " " << ext << std::endl;
		}
	}

	return 0;
}