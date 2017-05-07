// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// File: XsdMethods.java
//
// Description:
// See Javadoc section for class description.
//
// Copyright ©2011 The Boeing Company All rights reserved.
// ------------------------------------------------------------------------------------------------------------------------------------------------

// Package
package avtas.lmcp.lmcpgen;

import java.io.File;
import java.util.ArrayList;
import java.util.Formatter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import avtas.lmcp.lmcpgen.EnumInfo.EnumEntry;

/**
 * The <i>XsdMethods</i> class provides the methods to generate an XML Schema
 * Definition (XSD) from one or more MDMs. <br>
 * 
 * @author The Boeing Company / Networked Systems Technology Group <br>
 * @version 1.0
 * @since Jan 14, 2011
 */
public class XsdMethods
{
    public static String series(MDMInfo[] infos, MDMInfo info, File outfile,
            StructInfo st, EnumInfo en, String ws) throws Exception
    {
        if (info != null)
        {
            return info.seriesName;
        }

        StringBuilder b = new StringBuilder(infos.length * 9);
        for (MDMInfo mdm : infos)
        {
            b.append(mdm.seriesName);
            b.append('-');
        }
        b.setLength(b.length() - 1);
        return b.toString();
    }

    public static String generate_structs(MDMInfo[] infos, MDMInfo info,
            File outfile, StructInfo st, EnumInfo en, String ws)
            throws Exception
    {
        Map<String, StructInfo> parentMap = getParentMap(infos);
        Map<String, List<String>> childMap = getChildMap(infos);
        Formatter out = new Formatter();
        if (st != null)
        {
            generateStruct(st, parentMap, childMap, ws, out);
        }
        else if (info != null)
        {
            generateStructs(info.structs, parentMap, childMap, ws, out);
        }
        else
        {
            generateSeriesStructs(infos, parentMap, childMap, ws, out);
        }
        return out.toString();
    }

    public static String generate_enums(MDMInfo[] infos, MDMInfo info,
            File outfile, StructInfo st, EnumInfo en, String ws)
            throws Exception
    {
        Formatter out = new Formatter();
        if (en != null)
        {
            generateEnum(en, ws, out);
        }
        else if (info != null)
        {
            generateEnums(info.enums, ws, out);
        }
        else
        {
            generateSeriesEnums(infos, ws, out);
        }
        return out.toString();
    }

    private static void generateSeriesStructs(MDMInfo[] series,
            Map<String, StructInfo> parentMap,
            Map<String, List<String>> childMap, String ws, Formatter out)
    {
        for (MDMInfo mdmInfo : series)
        {
            generateInlineComment("Series: " + mdmInfo.seriesName, ws, out);
            generateInlineComment(mdmInfo.comment, ws, out);
            generateStructs(mdmInfo.structs, parentMap, childMap, ws, out);
        }
    }

    private static void generateStructs(StructInfo[] structs,
            Map<String, StructInfo> parentMap,
            Map<String, List<String>> childMap, String ws, Formatter out)
    {
        for (StructInfo structInfo : structs)
        {
            generateStruct(structInfo, parentMap, childMap, ws, out);
        }
    }

    private static void generateStruct(StructInfo st,
            Map<String, StructInfo> parentMap,
            Map<String, List<String>> childMap, String ws, Formatter out)
    {
        List<FieldInfo> allFields = gatherFields(st, parentMap, new ArrayList<FieldInfo>());
        out.format("%s<xs:element name=\"%s\">%n", ws, st.name);
        generateComment(st.comment, ws, out);
        out.format("%s  <xs:complexType>%n", ws);
        generateFields(allFields, childMap, ws + "    ", out);
        out.format("%s    <xs:attribute ref=\"Series\" fixed=\"%s\" use=\"required\" />%n", ws, st.seriesName);
        out.format("%s  </xs:complexType>%n", ws);
        out.format("%s</xs:element>%n", ws);
    }

    private static void generateFields(List<FieldInfo> fields,
            Map<String, List<String>> childMap, String ws, Formatter out)
    {
        if (fields.size() > 0)
        {
            out.format("%s<xs:all>%n", ws);
            for (FieldInfo field : fields)
            {
                generateFieldRef(field, childMap, ws + "  ", out);
            }
            out.format("%s</xs:all>%n", ws);
        }
    }

    private static void generateFieldRef(FieldInfo field,
            Map<String, List<String>> childMap, String ws, Formatter out)
    {
        if (field.isStruct && !field.type.equals(MDMInfo.LMCP_OBJECT_NAME))
        {
            String max = "";
            if (field.isArray)
            {
                max = " maxOccurs=\"unbounded\"";
            }
            out.format("%s<xs:element name=\"%s\" minOccurs=\"0\">%n", ws, field.name);
            generateComment(field.comment, ws, out);
            out.format("%s  <xs:complexType>%n", ws);
            out.format("%s    <xs:choice minOccurs=\"0\"%s>%n", ws, max);
            out.format("%s      <xs:element ref=\"null\" />%n", ws);
            out.format("%s      <xs:element ref=\"%s\" />%n", ws, field.type);
            generateChildRefs(field.type, childMap, ws + "      ", out);
            out.format("%s    </xs:choice>%n", ws);
            out.format("%s  </xs:complexType>%n", ws);
            out.format("%s</xs:element>%n", ws);
        }
        else if (field.isArray)
        {
            out.format("%s<xs:element name=\"%s\" minOccurs=\"0\">%n", ws, field.name);
            generateComment(field.comment, ws, out);
            out.format("%s  <xs:complexType>%n", ws);
            out.format("%s    <xs:sequence>%n", ws);
            out.format("%s      <xs:element name=\"%s\" type=\"%s\" minOccurs=\"0\" maxOccurs=\"unbounded\" />%n", ws, field.type, field.type);
            out.format("%s    </xs:sequence>%n", ws);
            out.format("%s  </xs:complexType>%n", ws);
            out.format("%s</xs:element>%n", ws);
        }
        else
        {
            if (field.comment.length() != 0)
            {
                out.format("%s<xs:element name=\"%s\" type=\"%s\" minOccurs=\"0\">%n", ws, field.name, field.type);
                generateComment(field.comment, ws, out);
                out.format("%s</xs:element>%n", ws);
            }
            else
            {
                out.format("%s<xs:element name=\"%s\" type=\"%s\" minOccurs=\"0\" />%n", ws, field.name, field.type);
            }
        }
    }

    private static void generateChildRefs(String type,
            Map<String, List<String>> childMap, String ws, Formatter out)
    {
        List<String> types = childMap.get(type);
        if (types != null)
        {
            for (String childType : types)
            {
                out.format("%s<xs:element ref=\"%s\" />%n", ws, childType);
                generateChildRefs(childType, childMap, ws, out);
            }
        }
    }

    private static void generateSeriesEnums(MDMInfo[] series, String ws,
            Formatter out)
    {
        for (MDMInfo mdmInfo : series)
        {
            generateInlineComment("Series: " + mdmInfo.seriesName, ws, out);
            generateEnums(mdmInfo.enums, ws, out);
        }
    }

    private static void generateEnums(EnumInfo[] enums, String ws, Formatter out)
    {
        for (EnumInfo enumInfo : enums)
        {
            generateEnum(enumInfo, ws, out);
        }
    }

    private static void generateEnum(EnumInfo en, String ws, Formatter out)
    {
        out.format("%s<xs:simpleType name=\"%s\">%n", ws, en.name);
        generateComment(en.comment, ws, out);
        out.format("%s  <xs:restriction base=\"xs:string\">%n", ws);
        for (EnumEntry entry : en.entries)
        {
            generateEnumEntry(entry, ws + "    ", out);
        }
        out.format("%s  </xs:restriction>%n", ws);
        out.format("%s</xs:simpleType>%n", ws);
    }

    private static void generateEnumEntry(EnumEntry entry, String ws,
            Formatter out)
    {
        if (entry.comment.length() != 0)
        {
            out.format("%s<xs:enumeration value=\"%s\">%n", ws, entry.name);
            generateComment(entry.comment, ws, out);
            out.format("%s</xs:enumeration>%n", ws);
        }
        out.format("%s<xs:enumeration value=\"%s\" />%n", ws, entry.name);
    }

    private static void generateInlineComment(String comment, String ws,
            Formatter out)
    {
        comment = comment.trim();
        if (comment.length() != 0)
        {
            out.format("%s<!-- %s -->%n", ws, comment);
        }
    }

    private static void generateComment(String comment, String ws, Formatter out)
    {
        comment = comment.trim();
        if (comment.length() != 0)
        {
            comment = comment.replace("<", "&lt;");
            comment = comment.replace("&", "&amp;");
            out.format("%s  <xs:annotation>%n", ws);
            out.format("%s    <xs:documentation>%s </xs:documentation>%n", ws, comment);
            out.format("%s  </xs:annotation>%n", ws);
        }
    }

    private static List<FieldInfo> gatherFields(StructInfo st,
            Map<String, StructInfo> parentMap, List<FieldInfo> fields)
    {
        if (st != null)
        {
            gatherFields(parentMap.get(st.extends_name), parentMap, fields);
            for (FieldInfo fieldInfo : st.fields)
            {
                fields.add(fieldInfo);
            }
        }
        return fields;
    }

    private static Map<String, StructInfo> getParentMap(MDMInfo[] infos)
    {
        HashMap<String, StructInfo> map = new HashMap<String, StructInfo>();
        HashSet<String> parents = new HashSet<String>();
        for (MDMInfo mdmInfo : infos)
        {
            for (StructInfo struct : mdmInfo.structs)
            {
                map.put(struct.name, struct);
                parents.add(struct.extends_name);
            }
        }
        parents.remove("");
        map.keySet().retainAll(parents);
        return map;
    }

    private static Map<String, List<String>> getChildMap(MDMInfo[] infos)
    {
        Map<String, List<String>> map = new HashMap<String, List<String>>();
        for (MDMInfo mdmInfo : infos)
        {
            for (StructInfo struct : mdmInfo.structs)
            {
                List<String> childList = getChildList(map, struct);
                if (childList != null)
                {
                    childList.add(struct.name);
                }
            }
        }
        return map;
    }

    private static List<String> getChildList(
            Map<String, List<String>> childMap, StructInfo st)
    {
        String parent = st.extends_name;
        if (parent == null || parent.length() == 0)
        {
            return null;
        }
        List<String> ret = childMap.get(parent);
        if (ret == null)
        {
            ret = new ArrayList<String>();
            childMap.put(parent, ret);
        }
        return ret;
    }
}
