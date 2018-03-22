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

namespace Avtas.Lmcp
{
    /// <summary>
    /// Interface for all LMCP objects.
    /// </summary>
    public interface ILmcpObject : ICloneable
    {
        /// <summary>
        /// Calculates the size of this message not including the header or the checksum.
        /// </summary>
        /// <returns>The size, in bytes, as a signed integer.</returns> 
        int CalculateSize();

        /// <summary>
        /// Packs the data into a Stream.
        /// </summary>
        /// <param name="buf">The Stream that the message will be packed into.</param>
        void Pack(Stream buf);

        /// <summary>
        /// Unpacks data from a Stream.
        /// </summary>
        /// <param name="buf">The Stream that the message will be unpacked from.</param>
        void Unpack(Stream buf);

        /// <summary>
        /// Converts the object to an XML format.
        /// </summary>
        /// <param name="ws">The whitespace to use for padding.</param>
        string ToXml(string ws);

        /// <summary>
        /// Gets the numeric identifier for the LMCP type.
        /// </summary>
        uint LmcpType { get; }

        /// <summary>
        /// Gets the name of the series this object belongs to.
        /// </summary>
        string SeriesName { get; }

        /// <summary>
        /// Gets a numeric identifier for the series this object belongs to.
        /// </summary>
        long SeriesNameAsLong { get; }

        /// <summary>
        /// Gets a version number for the object.
        /// </summary>
        ushort SeriesVersion { get; }
    }
}
