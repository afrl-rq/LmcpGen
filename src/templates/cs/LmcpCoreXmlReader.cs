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
using System.Collections.Generic;

namespace Avtas.Lmcp
{
    public abstract class LmcpXmlReader
    {

        private static List<LmcpXmlReader> readers = new List<LmcpXmlReader>();

        public void RegisterXmlReader( LmcpXmlReader reader )
        {
            readers.Add( reader );
        }

        /// <summary>
        /// Reads an LMCP XML Element and return the ILMCPObject it contains
        /// </summary>
        public static ILmcpObject ReadXml(string input) {
            StringReader sr = new StringReader(input);
            XmlDocument doc = new XmlDocument();
            doc.Load(sr);
            return ReadXML(doc.DocumentElement);
        }

        /// <summary>
        /// Reads an LMCP XML Element and return the ILMCPObject it contains
        /// </summary>
        public static ILmcpObject ReadXML(XmlElement el)
        {
            foreach( LmcpXmlReader reader in readers) {
                if (reader.getSeriesName().Equals(el.GetAttribute("Series"))) {
                    return reader.visitType(el);
                }
            }
            return null;
        }

        public abstract ILmcpObject visitType(XmlElement el);

        public abstract string getSeriesName();

        public static bool get_bool(XmlElement el, Object defaultVal) { return Convert.ToBoolean(get_string(el, defaultVal)); }

        public static char get_char(XmlElement el, Object defaultVal) { return Convert.ToChar(get_string(el, defaultVal)); }

        public static short get_int16(XmlElement el, Object defaultVal) { return Convert.ToInt16(get_string(el, defaultVal)); }

        public static int get_int32(XmlElement el, Object defaultVal) { return Convert.ToInt32(get_string(el, defaultVal)); }

        public static long get_int64(XmlElement el, Object defaultVal) { return Convert.ToInt64(get_string(el, defaultVal)); }

        public static float get_real32(XmlElement el, Object defaultVal) { return Convert.ToSingle(get_string(el, defaultVal)); }

        public static double get_real64(XmlElement el, Object defaultVal) { return Convert.ToDouble(get_string(el, defaultVal)); }

        public static byte get_byte(XmlElement el, Object defaultVal) { return Convert.ToByte(get_string(el, defaultVal)); }

        public static ushort get_uint16(XmlElement el, Object defaultVal) { return Convert.ToUInt16(get_string(el, defaultVal)); }

        public static uint get_uint32(XmlElement el, Object defaultVal) { return Convert.ToUInt32(get_string(el, defaultVal)); }

        public static String get_string(XmlElement el, Object defaultVal)
        {
            if (el.FirstChild != null)
            {
                return el.FirstChild.Value;
            }
            else
            {
                return Convert.ToString(defaultVal);
            }
        }
    }
}
