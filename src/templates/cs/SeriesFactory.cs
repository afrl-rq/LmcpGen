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
using Avtas.Lmcp;

-<series_namespace>- {

    public class SeriesFactory : LmcpFactory
{
  public static readonly string SERIES_NAME = -<series_name_setup>-;

  public static readonly SeriesFactory DEFAULT_FACTORY = new SeriesFactory();

  /// <summary>
  /// Constructor for this factory.  Use getDefaultFactory() to access a SeriesFactory.
  /// </summary>
  public SeriesFactory() { }

  /// <summary>
  /// Returns an instance of this factory that can be used anywhere in the application.  This is the
  /// preferred way to get and use the SeriesFactory.
  /// </summary>
  public static SeriesFactory getDefaultFactory() { return DEFAULT_FACTORY; }

  /// <summary>
  /// Returns an Lmcp message that is created by reading an array of bytes from some input
  /// source.  The message header items are read, the root object is created, and the 
  /// checksum is validated.  If the root object is of a type not defined by the -<series_namespace>-
  /// series, then a null message is returned.
  /// </summary>
  /// <returns>An ILmcpObject or null if the root object type is not defined.</returns>
  public override ILmcpObject getObject( byte[] bytes )
  {
    if ( bytes == null || bytes.Length < HEADER_SIZE )
      throw new Exception( "Lmcp Factory Exception: Null buffer or not enough bytes in buffer" );

    if ( !validate( bytes ) )
      throw new Exception( "Lmcp Factory Exception: Checksum does not match" );

    // make sure that the series names match
    for ( int i = 0; i < 10; i++ )
    {
      if ( bytes[i] == 0 && SERIES_NAME[i] == '\0' )
        break;
      if ( bytes[i] != SERIES_NAME[i] )
      {
        throw new Exception( "Lmcp Factory Exception: Series name does not match" );
      }
    }

    uint type = getLmcpType( bytes );
    ILmcpObject obj = createObject( type );

    if ( obj == null )
    {
      throw new Exception( "Lmcp Factory Exception: Invalid struct (" + type + ")" );
    }

    MemoryStream buf = new MemoryStream( bytes );
    buf.Seek( HEADER_SIZE, SeekOrigin.Begin );

    obj.unpack( buf );

    return obj;
  }

  public override ILmcpObject createObject( uint object_type )
  {
    switch ( object_type )
    {
                 -<factory_object_type>-
            } 
            return null;
        }

public override string getName( uint type )
{
  switch ( (int)type )
  {
                -<enum_name_for_type>-

                default: return "";
}
        }

        public override uint getType( string name )
{
  -<enum_type_for_name>-
            return 0;
}

/// <summary>
/// Returns an array of names of all structs in this series.
/// </summary>
public override string[] getAllTypes()
{
  return new string[] { -<factory_get_type_names>- };
}
    }
}
