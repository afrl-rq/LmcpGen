// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

// Utility libraries from standard c++
# ifdef LINUX
#include <arpa/inet.h>
#include <cstring>
#endif
#include <string>
#include <cstdint>
#include <iostream>
#include <vector>

// Include appropriate socket implementation headers.
# ifdef WIN32
#include <winsock.h>
#endif
#define socklen_t int

#include "avtas/lmcp/ByteBuffer.h"
#include "avtas/lmcp/Factory.h"
#include "avtas/lmcp/Object.h"
-<include_every_series>-

# ifdef LINUX
typedef int SOCKET;
int SOCKET_ERROR = -1;  // error return code for socket()
int INVALID_SOCKET = -1;  // error return code for connect()
#endif

bool EstablishConnection(SOCKET&, int);

// Define the main method.
int main(int argc, char* argv[])
{
	int port = 11041;
  
	// create connection
	SOCKET connectionSocket;
	while( !EstablishConnection(connectionSocket,port) )
	{
		std::cout << "Could not establish connection!" << std::endl;
	}

	// Create the buffer to hold incoming messages. Choosing an arbitrarily large sized
	// buffer big enough to hold any message we want to receive.
	int bufferSize = 1048576;
	char* buffer = new char[bufferSize];

	// display any messages coming across network
	avtas::lmcp::ByteBuffer buf;
	avtas::lmcp::Object* obj;
	for(;;)
	{
		int bytesReceived = recv(connectionSocket, buffer, bufferSize, 0);

		if(bytesReceived <= 0)
		{
			std::cout << "Connection closed or message receive error. Waiting for new connection ..." << std::endl;
			while( !EstablishConnection(connectionSocket,port) )
			{
				std::cout << "Could not establish connection!" << std::endl;
			}
			continue;
		}
		
		// potentially received multiple messages back-to-back
		int offsetindex = 0;
		while(bytesReceived > static_cast<int>(avtas::lmcp::Factory::HEADER_SIZE))
		{
			uint8_t* startByte = (uint8_t*) &buffer[offsetindex];
			uint32_t objsize = avtas::lmcp::Factory::getObjectSize(startByte, avtas::lmcp::Factory::HEADER_SIZE);
			objsize += avtas::lmcp::Factory::HEADER_SIZE + avtas::lmcp::Factory::CHECKSUM_SIZE;
				
			// process message
			buf.allocate(objsize);
                        buf.rewind();
			memcpy(buf.array(),startByte,objsize);
			//std::cout << "Received: " << std::endl;
			//std::cout << buf.toString() << std::endl;
			bytesReceived -= objsize;
			offsetindex += objsize;
			obj = avtas::lmcp::Factory::getObject(buf);
			if(!obj)
			{
				//std::cout << "Invalid message format" << std::endl;
                                std::cout << buf.toString() << std::endl;
				continue;
			}

			std::cout << obj->toXML() << std::endl;
			delete obj;
		}
	}
}

bool EstablishConnection(SOCKET& connectionSocket, int port)
{
# ifdef WIN32
	// Start Winsock
	WSAData wsaData;
	WSAStartup(MAKEWORD(1, 1), &wsaData);
#endif
  
	// wait for connection
	SOCKET listeningSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	if(listeningSocket == INVALID_SOCKET) 
		return false;
	sockaddr_in source;
	source.sin_family = AF_INET;
	source.sin_addr.s_addr = htonl(INADDR_ANY);
	source.sin_port = htons((u_short)port);
	memset(&(source.sin_zero), '\0', 8);
	socklen_t source_len = sizeof(source);
	bind(listeningSocket, (sockaddr*)&source, source_len);
	listen(listeningSocket, SOMAXCONN);
	connectionSocket = accept(listeningSocket, NULL, NULL);
	if(connectionSocket == INVALID_SOCKET)
		return false;

	return true;
}