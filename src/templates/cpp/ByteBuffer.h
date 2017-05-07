// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

#ifndef _AVTAS_LMCP_BYTEBUFFER_H_
#define _AVTAS_LMCP_BYTEBUFFER_H_

#include <string>
#include <cstdint>


namespace avtas {
    namespace lmcp {

        class ByteBuffer {
        public:

            enum ByteOrder {
                ENDIAN_BIG, ENDIAN_LITTLE
            };
            ByteBuffer(ByteOrder order = ENDIAN_BIG);
            ByteBuffer(const ByteBuffer &that);
            ByteBuffer & operator=(const ByteBuffer &that);
            virtual ~ByteBuffer(void);

			// Note: 4GB capacity limit
            inline uint32_t capacity(void) const {
                return _capacity;
            }

            inline ByteBuffer & clear(void) {
                allocate(0);
                return position(0);
            }

            inline bool hasRemaining(void) const {
                return (remaining() > 0);
            }

            inline uint32_t position(void) const {
                return _position;
            }

            inline ByteBuffer & position(uint32_t pos) {
                if (pos < capacity()) {
                    _position = pos;
                }
                return *this;
            }

            inline uint32_t remaining(void) const {
                return (capacity() - position());
            }

            inline ByteBuffer & rewind(void) {
                return position(0);
            }

            // Relative get/put
            uint8_t get(void);
            ByteBuffer & get(uint8_t* dst, uint32_t length, uint32_t offset = 0);
            bool getBool(void);
            uint8_t getByte(void);
            int16_t getShort(void);
            uint16_t getUShort(void);
            int32_t getInt(void);
            uint32_t getUInt(void);
            int64_t getLong(void);
            uint64_t getULong(void);
            float getFloat(void);
            double getDouble(void);
            std::string getString(void);

            ByteBuffer & getBoolArray(bool * b, uint32_t length);
            ByteBuffer & getByteArray(uint8_t * c, uint32_t length);
            ByteBuffer & getShortArray(int16_t * s, uint32_t length);
            ByteBuffer & getUShortArray(uint16_t * us, uint32_t length);
            ByteBuffer & getIntArray(int32_t * i, uint32_t length);
            ByteBuffer & getUIntArray(uint32_t * ui, uint32_t length);
            ByteBuffer & getLongArray(int64_t * l, uint32_t length);
            ByteBuffer & getULongArray(uint64_t * ul, uint32_t length);
            ByteBuffer & getFloatArray(float * f, uint32_t length);
            ByteBuffer & getDoubleArray(double * d, uint32_t length);
            ByteBuffer & put(const uint8_t b);
            ByteBuffer & put(const uint8_t * src, uint32_t length, uint32_t offset = 0);
            ByteBuffer & put(ByteBuffer & buf);
            ByteBuffer & putBool(bool b);
            ByteBuffer & putByte(uint8_t c);
            ByteBuffer & putShort(int16_t s);
            ByteBuffer & putUShort(uint16_t us);
            ByteBuffer & putInt(int32_t i);
            ByteBuffer & putUInt(uint32_t ui);
            ByteBuffer & putLong(int64_t l);
            ByteBuffer & putULong(uint64_t ul);
            ByteBuffer & putFloat(float f);
            ByteBuffer & putDouble(double d);
            ByteBuffer & putBoolArray(const bool * b, uint32_t length);
            ByteBuffer & putByteArray(const uint8_t * b, uint32_t length);
            ByteBuffer & putShortArray(const int16_t * s, uint32_t length);
            ByteBuffer & putUShortArray(const uint16_t * us, uint32_t length);
            ByteBuffer & putIntArray(const int32_t * i, uint32_t length);
            ByteBuffer & putUIntArray(const uint32_t * ui, uint32_t length);
            ByteBuffer & putLongArray(const int64_t * l, uint32_t length);
            ByteBuffer & putULongArray(const uint64_t * ul, uint32_t length);
            ByteBuffer & putFloatArray(const float * f, uint32_t length);
            ByteBuffer & putDoubleArray(const double * d, uint32_t length);
            ByteBuffer & putString(std::string s);

            // Absolute get/put
            uint8_t get(uint32_t index) const;
            ByteBuffer & put(uint32_t index, const uint8_t b);

            virtual void allocate(uint32_t size);
            uint8_t * array(void);
            const uint8_t * array(void) const;

            ByteOrder getByteOrder(void) const {
                return _byteOrder;
            }
            virtual std::string toString(void) const;

        protected:
            uint32_t _position;
            uint32_t _capacity;
            uint8_t * _buf;
            ByteOrder _byteOrder;

        private:
            // Copies from src to internal buffer, swapping if neccessary
            // No bounds checking is done here, but the position is changed.
            void copyToBuffer(const uint8_t * src, uint32_t length, uint32_t elem = 1);
            // Copies from internal buffer to dest, swapping if neccessary
            // No bounds checking is done here, but the position is changed.
            void copyFromBuffer(uint8_t * dst, uint32_t length, uint32_t elem = 1);
        };

    } // end namespace lmcp
} // end namespace avtas

#endif //_AVTAS_LMCP_BYTEBUFFER_H_

