// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

#ifndef _-<series_name_caps>-_FACTORY_H_
#define _-<series_name_caps>-_FACTORY_H_

#include <cstdint>
#include "avtas/lmcp/ByteBuffer.h"
#include "avtas/lmcp/Object.h"

-<open_namespace>-

   class -<series_name>-Factory {
   public:
      // Size (in bytes) of the default message header
      static const uint32_t HEADER_SIZE;

      // Size (in bytes) of the message checksum
      static const uint32_t CHECKSUM_SIZE;

      // Length in bytes of the message series name
      static const uint32_t SERIESNAME_SIZE;

      // "LMCP" control sequence, stated as a 4-byte integer
      static const int32_t LMCP_CONTROL_STR;

      // Destructor
      virtual ~-<series_name>-Factory(void);

      // Adds an object to the buffer with all the proper header information
      static void putObject(const avtas::lmcp::Object* o, avtas::lmcp::ByteBuffer & buffer);

      // Should return pointer to a new message unpacked from given buffer.
      // The buffer is assumed to be in bug endian byte order.
      // To be implemented by subclass
      static avtas::lmcp::Object * getObject(avtas::lmcp::ByteBuffer & buffer);

      // Instantiates an empty object of the specified type
      static avtas::lmcp::Object * createObject(int64_t series_id, uint32_t type, uint16_t version);

      // Fills the given buffer with a message with the given instance id and 
      // serialized root object. The checksum will be calculated if the
      // enableChecksum flag is true. Returns true on success, false otherwise.
      // The buffer is assumed to be in big endian byte order.
      static avtas::lmcp::ByteBuffer * packMessage(const avtas::lmcp::Object * const rootObject, const bool enableChecksum = false);

      // Computes the 32-bit checksum of a buffer using all but the last
      // CHECKSUM_SIZE bytes in the buffer. Assumes the buffer is in
      // big endian byte order.
      static uint32_t calculateChecksum(const uint8_t * bytes, const uint32_t size);

      // Returns size (in bytes) of the root object of the packed message in
      // buffer. Assumes the buffer is in big endian byte order.
      static uint32_t getObjectSize(const uint8_t * bytes, const uint32_t size);

      // Validates a buffer by comparing the buffer's checksum with the
      // calculated checksum value of that buffer. Assumes the buffer is
      // in big endian byte order.
      static bool validate(const uint8_t * bytes, const uint32_t size);

   protected:
      // Constructor
      -<series_name>-Factory(void);

      // Copy Constructor
      -<series_name>-Factory(const -<series_name>-Factory & that);

      // Assignment Operator
      -<series_name>-Factory & operator=(const -<series_name>-Factory & that);
   };

-<close_namespace>-

#endif // _-<series_name_caps>-_FACTORY_H_
