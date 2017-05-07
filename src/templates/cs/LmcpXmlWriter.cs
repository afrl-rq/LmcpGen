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
using System.Xml;

namespace Avtas.Lmcp
{
    public abstract class LmcpXmlWriter
    {
        /// <summary>
        /// Writes a set of LMCP objects to an Xml file.
        /// </summary>
        public static void WriteXml(ILmcpObject[] objs, FileInfo fileout, char[] seriesname)
        {
            if (fileout == null || objs == null || fileout.IsReadOnly) return;

            StreamWriter sw = fileout.CreateText();
            sw.Write("<LMCPObjectList SeriesName=\"");
            sw.Write(seriesname);
            sw.Write("\">\n");

            foreach (ILmcpObject o in objs)
            {
                if (o != null)
                    sw.Write(o.ToXml("  "));
            }

            sw.Write("</LMCPObjectList>");
            sw.Close();
        }
    }
}
