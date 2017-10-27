// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

using System;
using System.IO;
using System.Collections.Generic;

namespace Avtas.Lmcp
{
    /// <summary>
    /// Factory for creating LMCP objects.
    /// </summary>
    public abstract class LmcpFactory
    {
        internal const int HEADER_SIZE = 8;
        internal const int CHECKSUM_SIZE = 4;
        internal const int LMCP_CONTROL_STR = 0x4c4d4350;

        private static List<ISeriesList> _seriesLists = new List<ISeriesList>();

        public static void RegisterSeries( ISeriesList seriesList )
        {
            _seriesLists.Add( seriesList );
        }
        /// <summary>
        /// Returns an array of bytes corresponding to the first message encountered in
        /// the input stream. The method blocks until all of the bytes are read.
        /// </summary>
        /// <returns>An array of bytes corresponding to the first message encountered in
        /// the input stream.</returns>
        public static byte[] GetMessageBytes(BinaryReader sr)
        {
            byte[] bytes = new byte[HEADER_SIZE];
            int i = sr.Read(bytes, 0, HEADER_SIZE);
            while(i <bytes.Length)
                i += sr.Read(bytes, i, bytes.Length-i);

            // retrieves the "size" value in BIG_ENDIAN order
            uint size = GetSize(bytes);

            byte[] buf = new byte[size + HEADER_SIZE + CHECKSUM_SIZE];
            bytes.CopyTo(buf, 0);

            i = bytes.Length;
            while(i <buf.Length)
                i += sr.Read(buf, i, buf.Length-i);

            return buf;
        }

        /// <summary>
        /// Returns an Lmcp message that is created by reading an array of bytes from some input
        /// source.  The message header items are read, the root object is created, and the
        /// checksum is validated. 
        /// </summary>
        /// <returns>An ILmcpObject or null if the root object type is not defined.</returns>
        public static ILmcpObject GetObject(byte[] bytes)
        {
            if (bytes == null || bytes.Length < HEADER_SIZE)
                throw new Exception("Lmcp Factory Exception: Null buffer or not enough bytes in buffer");

            if (!Validate(bytes))
                throw new Exception("Lmcp Factory Exception: Checksum does not match");

            LmcpBinaryReader buf = new LmcpBinaryReader(bytes);

            if (buf.ReadInt32() != LMCP_CONTROL_STR) {
                throw new Exception("Lmcp Factory Exception: Not a valid LMCP message.");
            }
            if ( buf.ReadInt32() > bytes.Length - HEADER_SIZE - CHECKSUM_SIZE) {
                throw new Exception("LMCP Factory Exception: not enough bytes in buffer to create object.");
            }

            return buf.ReadObject();
        }

        /// <summary>
        /// Returns an byte array containing an ILmcpObject packed as a message.
        /// </summary>
        /// <returns>The packed message.</returns>
        public static byte[] PackMessage(ILmcpObject rootObject, bool calculatechecksum)
        {
            if (rootObject == null) return null;

            int size = rootObject.CalculateSize();
            byte[] bytes = new byte[size + HEADER_SIZE + CHECKSUM_SIZE];

            MemoryStream buf = new MemoryStream(bytes);
            LmcpBinaryWriter bw = new LmcpBinaryWriter(buf);

            bw.Write(LMCP_CONTROL_STR);
            bw.Write(size);
            bw.Write(rootObject);

            uint cs = calculatechecksum ? CalculateChecksum(bytes) : 0;
            bw.Write(cs);

            return buf.ToArray();
        }

        internal static ILmcpObject CreateObject(long series_id, uint object_type, ushort version) {
            foreach ( var series in _seriesLists )
            {
                if ( series_id != series.SeriesId )
                    continue;
                return series.GetInstance( object_type, version );
            }
            return null;
        }

        /// <summary>
        /// Returns the size of a message that is represented by the given byte array.
        /// </summary>
        public static uint GetSize(byte[] bytes)
        {
            uint size = 0;
            size |= (bytes[4] & 0xFFu);
            size <<= 8;
            size |= (bytes[5] & 0xFFu);
            size <<= 8;
            size |= (bytes[6] & 0xFFu);
            size <<= 8;
            size |= (bytes[7] & 0xFFu);
            return size;
        }

        /// <summary>
        /// Returns a message read from an object.
        /// </summary>
        public static ILmcpObject GetObject(BinaryReader sr)
        {
            byte[] bytes = GetMessageBytes(sr);
            return GetObject(bytes);
        }


        /// <summary>
        /// Calculates the checksum.  This should be called after pack().
        /// The checksum sums all bytes in the packet between 0 and 
        /// buf.limit() - CHECKSUM_SIZE.
        /// </summary>
        public static uint CalculateChecksum(byte[] bytes)
        {
            uint val = 0;
            for (int i = 0; i < bytes.Length - CHECKSUM_SIZE; i++)
            {
            	val += ( (uint) bytes[i] );
            }
            return val;
        }

        /// <summary>
        /// Checks the bytebuffer's checksum value against the calculated checksum 
        /// returns true if the calculated and stored values match, or if the buffer value is
        /// zero (indicating that checksum was not calculated.  This method rewinds the buffer and 
        /// returns it to LIMIT - 4 bytes (start position of checksum).
        /// </summary>
        public static bool Validate(byte[] bytes)
        {
            // retrieves the checksum value in BIG_ENDIAN order
            uint cs = 0;
            int len = bytes.Length;
            cs |= (bytes[len - 4] & 0xFFu);
            cs <<= 8;
            cs |= (bytes[len - 3] & 0xFFu);
            cs <<= 8;
            cs |= (bytes[len - 2] & 0xFFu);
            cs <<= 8;
            cs |= (bytes[len - 1] & 0xFFu);

            return (cs == 0) || (CalculateChecksum(bytes) == cs);
        }
    }
}
