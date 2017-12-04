// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

#include "avtas/lmcp/ByteBuffer.h"
#include <string.h>
#include <sstream>
#include <iostream>
#include <iomanip>

namespace avtas {
namespace lmcp {

   ByteBuffer::ByteBuffer(ByteOrder order) :
      _position(0),
      _capacity(0),
      _buf(0),
      _byteOrder(order)
   {
   }

   ByteBuffer::ByteBuffer(const ByteBuffer & that) :
      _position(0),
      _capacity(0),
      _buf(0),
      _byteOrder(that._byteOrder)
   {
      allocate(that.capacity());
      put(that.array(), that.capacity());
      rewind();
   }

   ByteBuffer & ByteBuffer::operator=(const ByteBuffer & that) {
      if (this != &that) {
         _byteOrder = that._byteOrder;
         allocate(that.capacity());
         put(that.array(), that.capacity());
         rewind();
      }
      return *this;
   }

   ByteBuffer::~ByteBuffer(void) {
      allocate(0);
   }

   uint8_t ByteBuffer::get(void) {
      if (_buf && hasRemaining()) {
         return _buf[_position++];
      }
      return 0;
   }

   ByteBuffer & ByteBuffer::get(uint8_t * dst, uint32_t length, uint32_t offset) {
      if (_buf && dst && offset<length && length>0 && remaining()>=length) {
         memcpy(&dst[offset], &_buf[_position], static_cast<size_t>(length));
         _position += length;
      }
      return *this;
   }

   bool ByteBuffer::getBool(void) {
      if (_buf && remaining()>=1) {
         bool b = *(reinterpret_cast<bool *>(&_buf[_position]));
         _position += 1;
         return b;
      }
      return 0;
   }

   uint8_t ByteBuffer::getByte(void) {
      if (_buf && remaining()>=1) {
         uint8_t c = _buf[_position];
         _position += 1;
         return c;
      }
      return 0;
   }

   int16_t ByteBuffer::getShort(void) {
      if (_buf && remaining()>=2) {
         int16_t b;
         copyFromBuffer(reinterpret_cast<uint8_t*>(&b), 2);
         return b;
      }
      return 0;
   }

   uint16_t ByteBuffer::getUShort(void) {
      if (_buf && remaining()>=2) {
         uint16_t b;
         copyFromBuffer(reinterpret_cast<uint8_t*>(&b), 2);
         return b;
      }
      return 0;
   }

   int32_t ByteBuffer::getInt(void) {
      if (_buf && remaining()>=4) {
         int32_t b;
         copyFromBuffer(reinterpret_cast<uint8_t*>(&b), 4);
         return b;
      }
      return 0;
   }

   uint32_t ByteBuffer::getUInt(void) {
      if (_buf && remaining()>=4) {
         uint32_t b;
         copyFromBuffer(reinterpret_cast<uint8_t*>(&b), 4);
         return b;
      }
      return 0;
   }

   int64_t ByteBuffer::getLong(void) {
      if (_buf && remaining()>=8) {
         int64_t b;
         copyFromBuffer(reinterpret_cast<uint8_t*>(&b), 8);
         return b;
      }
      return 0;
   }

   uint64_t ByteBuffer::getULong(void) {
      if (_buf && remaining()>=8) {
         uint64_t b;
         copyFromBuffer(reinterpret_cast<uint8_t*>(&b), 8);
         return b;
      }
      return 0;
   }

   float ByteBuffer::getFloat(void) {
      if (_buf && remaining()>=4) {
         float f;
         copyFromBuffer(reinterpret_cast<uint8_t*>(&f), 4);
         return f;
      }
      return 0.f;
   }

   double ByteBuffer::getDouble(void) {
      if (_buf && remaining()>=8) {
         double d;
         copyFromBuffer(reinterpret_cast<uint8_t*>(&d), 8);
         return d;
      }
      return 0.;
   }

   ByteBuffer & ByteBuffer::getBoolArray(bool * b, uint32_t length) {
      if (_buf && remaining()>=length) {
         copyFromBuffer(reinterpret_cast<uint8_t *>(b), 1, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::getByteArray(uint8_t * c, uint32_t length) {
      if (_buf && remaining()>=length) {
         copyFromBuffer(reinterpret_cast<uint8_t *>(c), 1, length);
      }
      return *this;
   }
   
   std::string ByteBuffer::getString(void) {
       uint16_t len = getUShort();
       uint8_t * c = new uint8_t[len+1];
       c[len] = 0; // null terminate string
       getByteArray(c, len);
       std::string str = std::string(reinterpret_cast<char*>(c));
       delete [] c;
       return str;
   }

   ByteBuffer & ByteBuffer::getShortArray(int16_t * s, uint32_t length) {
      if (_buf && remaining()>=(length*2)) {
         copyFromBuffer(reinterpret_cast<uint8_t *>(s), 2, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::getUShortArray(uint16_t * us, uint32_t length) {
      if (_buf && remaining()>=(length*2)) {
         copyFromBuffer(reinterpret_cast<uint8_t *>(us), 2, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::getIntArray(int32_t * i, uint32_t length) {
      if (_buf && remaining()>=(length*4)) {
         copyFromBuffer(reinterpret_cast<uint8_t *>(i), 4, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::getUIntArray(uint32_t * ui, uint32_t length) {
      if (_buf && remaining()>=(length*4)) {
         copyFromBuffer(reinterpret_cast<uint8_t *>(ui), 4, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::getLongArray(int64_t * l, uint32_t length) {
      if (_buf && remaining()>=(length*8)) {
         copyFromBuffer(reinterpret_cast<uint8_t *>(l), 8, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::getULongArray(uint64_t * ul, uint32_t length) {
      if (_buf && remaining()>=(length*8)) {
         copyFromBuffer(reinterpret_cast<uint8_t *>(ul), 8, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::getFloatArray(float * f, uint32_t length) {
      if (_buf && remaining()>=(length*4)) {
         copyFromBuffer(reinterpret_cast<uint8_t *>(f), 4, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::getDoubleArray(double * d, uint32_t length) {
      if (_buf && remaining()>=(length*8)) {
         copyFromBuffer(reinterpret_cast<uint8_t *>(d), 8, length);
      }
      return *this;
   }

   uint8_t ByteBuffer::get(uint32_t index) const {
      if (_buf && index<capacity()) {
         return _buf[index];
      }
      return 0;
   }

   ByteBuffer & ByteBuffer::put(const uint8_t b) {
      if (_buf && hasRemaining()) {
         _buf[_position++] = b;
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::put(const uint8_t * src, uint32_t length, uint32_t offset) {
      if (_buf && src && offset<length && length>0 && remaining()>=length) {
         memcpy(&_buf[_position], &src[offset], static_cast<size_t>(length));
         _position += length;
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::put(ByteBuffer & src) {
      uint32_t length = src.remaining();
      if (&src!=this && length<=remaining()) {
         src.get(_buf, length, _position);
         _position += length;
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putBool(bool b) {
      if (_buf && remaining()>=1) {
         memcpy(&_buf[_position], &b, 1);
         _position += 1;
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putByte(uint8_t c) {
      if (_buf && remaining()>=1) {
         memcpy(&_buf[_position], &c, 1);
         _position += 1;
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putShort(int16_t s) {
      if (_buf && remaining()>=2) {
         copyToBuffer(reinterpret_cast<uint8_t *>(&s), 2);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putString(std::string s) {
      uint16_t len = 0;
      if(s.length() >= 65536)
      {
         len = 65535;
         std::cerr << "String was too long to be packed! Max string length is 65535" << std::endl;
         std::cerr << "String that could not be packed: " << std::endl << s << std::endl;
      }
      else
      {
         len = static_cast<uint16_t>(s.length());   
      }
      if (_buf && remaining() >= static_cast<uint32_t>((2 + len))) {
          putUShort(len);
          putByteArray(reinterpret_cast<const uint8_t*>(s.c_str()), len);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putUShort(uint16_t us) {
      if (_buf && remaining()>=2) {
         copyToBuffer(reinterpret_cast<uint8_t *>(&us), 2);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putInt(int32_t i) {
      if (_buf && remaining()>=4) {
         copyToBuffer(reinterpret_cast<uint8_t *>(&i), 4);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putUInt(uint32_t ui) {
      if (_buf && remaining()>=4) {
         copyToBuffer(reinterpret_cast<uint8_t *>(&ui), 4);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putLong(int64_t l) {
      if (_buf && remaining()>=8) {
         copyToBuffer(reinterpret_cast<uint8_t *>(&l), 8);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putULong(uint64_t ul) {
      if (_buf && remaining()>=8) {
         copyToBuffer(reinterpret_cast<uint8_t *>(&ul), 8);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putFloat(float f) {
      if (_buf && remaining()>=4) {
         copyToBuffer(reinterpret_cast<uint8_t *>(&f), 4);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putDouble(double d) {
      if (_buf && remaining()>=8) {
         copyToBuffer(reinterpret_cast<uint8_t *>(&d), 8);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putBoolArray(const bool * b, uint32_t length) {
      if (_buf && remaining()>=length) {
         copyToBuffer(reinterpret_cast<const uint8_t *>(b), 1, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putByteArray(const uint8_t * c, uint32_t length) {
      if (_buf && remaining()>=(length)) {
         copyToBuffer(reinterpret_cast<const uint8_t *>(c), 1, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putShortArray(const int16_t * s, uint32_t length) {
      if (_buf && remaining()>=(length*2)) {
         copyToBuffer(reinterpret_cast<const uint8_t *>(s), 2, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putUShortArray(const uint16_t * us, uint32_t length) {
      if (_buf && remaining()>=(length*2)) {
         copyToBuffer(reinterpret_cast<const uint8_t *>(us), 2, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putIntArray(const int32_t * i, uint32_t length) {
      if (_buf && remaining()>=(length*4)) {
         copyToBuffer(reinterpret_cast<const uint8_t *>(i), 4, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putUIntArray(const uint32_t * ui, uint32_t length) {
      if (_buf && remaining()>=(length*4)) {
         copyToBuffer(reinterpret_cast<const uint8_t *>(ui), 4, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putLongArray(const int64_t * l, uint32_t length) {
      if (_buf && remaining()>=(length*8)) {
         copyToBuffer(reinterpret_cast<const uint8_t *>(l), 8, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putULongArray(const uint64_t * ul, uint32_t length) {
      if (_buf && remaining()>=(length*8)) {
         copyToBuffer(reinterpret_cast<const uint8_t *>(ul), 8, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putFloatArray(const float * f, uint32_t length) {
      if (_buf && remaining()>=(length*4)) {
         copyToBuffer(reinterpret_cast<const uint8_t *>(f), 4, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::putDoubleArray(const double * d, uint32_t length) {
      if (_buf && remaining()>=(length*8)) {
         copyToBuffer(reinterpret_cast<const uint8_t *>(d), 8, length);
      }
      return *this;
   }

   ByteBuffer & ByteBuffer::put(uint32_t index, const uint8_t b) {
      if (_buf && index<capacity()) {
         _buf[index] = b;
      }
      return *this;
   }

   
   void ByteBuffer::allocate(uint32_t size) {
      if (size != capacity()) {
         if (_buf) {
            delete [] _buf;
            _buf = 0;
         }
         if (size > 0) {
            _buf = new uint8_t[(_capacity=size)];
         } else {
            _capacity = 0;
         }
         rewind();
      }
   }

   uint8_t * ByteBuffer::array(void) {
      return _buf;
   }

   const uint8_t * ByteBuffer::array(void) const {
      return _buf;
   }

   std::string ByteBuffer::toString(void) const {
      std::ostringstream oss;
      if (_buf) {
         for (uint32_t i=0; i<capacity(); i++) {
            oss << std::setw(2) << std::setfill('0') << std::hex << _buf[i];
            if (i % 4 == 3) {
               oss << std::endl;
            } else {
               oss << " ";
            }
         }
         oss << std::endl;
      }
      return oss.str();
   }

   void ByteBuffer::copyToBuffer(const uint8_t * src, uint32_t length, uint32_t nelem) {
      if (nelem == 0)
         return;
      switch (_byteOrder) {
      case ENDIAN_BIG:
         if (nelem == 1) {
            // Swap the bytes
            uint32_t i;
            for (i=0; i<length; i++)
               _buf[_position+(length-1-i)] = src[i];
            _position += length;
         } else if (nelem > 0) {
            // Swap the bytes on each element
            uint32_t i;
            for (i=0; i<nelem; i++) {
               copyToBuffer(&src[(length*i)], length);
            }
         }
         return;
      case ENDIAN_LITTLE:
         // No swapping
         if (nelem > 0) {
            memcpy(&_buf[_position], src, static_cast<size_t>(length*nelem));
            _position += length*nelem;
         }
         return;
      }
   }

   void ByteBuffer::copyFromBuffer(uint8_t * dst, uint32_t length, uint32_t nelem) {
      if (nelem == 0)
         return;
      switch (_byteOrder) {
      case ENDIAN_BIG:
         if (nelem == 1) {
            // Swap the bytes
            uint32_t i;
            for (i=0; i<length; i++)
               dst[i] = _buf[_position+(length-1-i)];
            _position += length;
         } else if (nelem > 0) {
            // Swap the bytes on each element
            uint32_t i;
            for (i=0; i<nelem; i++) {
               copyFromBuffer(&dst[(length*i)], length);
            }
         }
         return;
      case ENDIAN_LITTLE:
         // No swapping
         if (nelem > 0) {
            memcpy(dst, &_buf[_position], static_cast<size_t>(length*nelem));
            _position += length*nelem;
         }
         return;
      }
   }

} // end namespace lmcp
} // end namespace avtas

