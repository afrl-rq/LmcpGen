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

-<series_namespace>-
{

    /// <summary>
    /// Class containing access to information about the Series.
    /// </summary>
public class SeriesList : ISeriesList
{
  public const string SERIES_NAME = -<series_name_setup>-;
  public const ushort SERIES_VERSION = -<series_version>-;
  /* Series name expressed as a long for quick comparisons */
  public const long SERIES_ID = -<series_id>-;

  public static String GetName( uint type )
  {
    switch ( type )
    {
      -<list_name_for_type>-
      default: return "";
    }
  }

public static uint GetType( String name )
{
  -<list_type_for_name>-
  return 0;
}

public ILmcpObject GetInstance( uint object_type, ushort version )
{
  if ( version != SERIES_VERSION )
    throw new InvalidOperationException("-<series_name>- SeriesList Exception. Bad Version Number.");

  switch ( object_type )
  {
    -<list_instance_for_type>-
    default: return null;
  }
}

public long SeriesId => SERIES_ID;

}
}