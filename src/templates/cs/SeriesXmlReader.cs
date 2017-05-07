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
using System.Xml;
using Avtas.Lmcp;

-<series_namespace>- {

    public class SeriesXmlReader : LmcpXmlReader
{

  private static string SERIES_NAME = "-<xml_seriesname>-";


  public override ILmcpObject visitType( XmlElement el )
  {
    string type = el.Name;

    -<xml_create_visits>-

            return null;
  }

  public override string getSeriesName()
  {
    return SERIES_NAME;
  }
}
}