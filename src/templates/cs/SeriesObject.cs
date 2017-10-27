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
using System.Text;
using System.Collections.Generic;
using Avtas.Lmcp;
-<import_series>-

-<series_namespace>- {

    /// <summary>
    /// -<object_comment>-
    /// </summary>
    public class -<object_type_name>- : -<parent_object_name>-
    {
        private const uint LMCP_TYPE = -<object_type_number>-;
private const string SERIES_NAME = -<series_name_setup>-;
private const ushort SERIES_VERSION = -<series_version>-;
/* Series name expressed as a long for quick comparisons */
private const long SERIES_ID = -<series_id>-;
private const string TYPE_NAME = "-<object_type_name>-";

        -<member_declaration>-

        /// <summary>
        /// Initializes a new instance of the <see cref="-<object_type_name>-"/> class.
        /// </summary>
        public -<object_type_name>-() {}

        /// <summary>
        /// Creates a new object that is a copy of the current instance.
        /// </summary>
        /// <returns>
        /// A new object that is a copy of this instance.
        /// </returns>
        public -<_override>- object Clone()
{
  return ( -<object_type_name>-) MemberwiseClone();
}

/// <summary>
/// Calculates the size of this message not including the header or the checksum.
/// </summary>
/// <returns>The size, in bytes, as a signed integer.</returns>
public -<_override>- int CalculateSize()
{
  -<calc_size>-
        }

/// <summary>
/// Unpacks data from a Stream.
/// </summary>
/// <param name="buf">The Stream that the data will be unpacked from.</param>
public -<_override>- void Unpack( Stream buf )
{
  LmcpBinaryReader br = new LmcpBinaryReader( buf );

  -<member_unpack>-
        }

/// <summary>
/// Packs the data into a Stream.
/// </summary>
/// <param name="buf">The Stream that the data will be packed into.</param>
public -<_override>- void Pack( Stream buf )
{
  LmcpBinaryWriter bw = new LmcpBinaryWriter( buf );

  -<member_pack>-
        }

/// <summary>
/// Returns a <see cref="T:System.String"/> that represents the current <see cref="T:System.Object"/>.
/// </summary>
/// <returns>
/// A <see cref="T:System.String"/> that represents the current <see cref="T:System.Object"/>.
/// </returns>
public override string ToString()
{
  StringBuilder buf = new StringBuilder();
  buf.Append( "Object Type -<object_type_name>- { \n" );

  -<member_print>-

  buf.Append( "}\n" );

  return buf.ToString();
}

/// <summary>
/// Determines whether the specified <see cref="T:System.Object"/> is equal to the current <see cref="T:System.Object"/>.
/// </summary>
/// <param name="obj">The <see cref="T:System.Object"/> to compare with the current <see cref="T:System.Object"/>.</param>
/// <returns>
/// true if the specified <see cref="T:System.Object"/> is equal to the current <see cref="T:System.Object"/>; otherwise, false.
/// </returns>
/// <exception cref="T:System.NullReferenceException">
/// The <paramref name="obj"/> parameter is null.
/// </exception>
public override bool Equals( object obj )
{
  if ( obj == null || GetType() != obj.GetType() )
    return false;

  -<equals_compare_object>-
        }

/// <summary>
/// Serves as a hash function for a particular type.
/// </summary>
/// <returns>
/// A hash code for the current <see cref="T:System.Object"/>.
/// </returns>
public override int GetHashCode()
{
  return base.GetHashCode();
}

/// <summary>
/// Converts the object to an XML format.
/// </summary>
/// <param name="ws">The whitespace to use for padding.</param>
/// <returns>The object in an XML format.</returns>
public -<_override>- string ToXml( string ws )
{
  StringBuilder buf = new StringBuilder();

  -<xml_write_object>-


            return buf.ToString();
}

/// <summary>
/// Gets the numeric identifier for the LMCP type.
/// </summary>
public -<_override>- uint LmcpType
{ get { return LMCP_TYPE; } }

/// <summary>
/// Gets the name of the series this object belongs to.
/// </summary>
public -<_override>- string SeriesName
{ get { return SERIES_NAME; } }

/// <summary>
/// Gets a numeric identifier for the series this object belongs to.
/// </summary>
public -<_override>- long SeriesNameAsLong
{ get { return SERIES_ID; } }

/// <summary>
/// Gets a version number for the object.
/// </summary>
public -<_override>- ushort Version
{ get { return SERIES_VERSION; } }

        -<member_gets_and_sets>-
    }
}