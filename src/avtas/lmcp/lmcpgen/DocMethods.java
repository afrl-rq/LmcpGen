// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

package avtas.lmcp.lmcpgen;

import java.io.File;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DocMethods {

//    public static String mdm_comment(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
//        return "<p>" + processComment(info.comment, infos, info) + "</p>\n";
//    }

    // Returns the date that the package was made
    public static String creation_date(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + DateFormat.getDateInstance(DateFormat.FULL).format(new Date());
    }

    public static String build_mdm_tables(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuilder buf = new StringBuilder();

        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            buf.append("<h1>Series Name: ").append(i.seriesName).append("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{").append(i.namespace.replace('/', '.')).append("}</h1>\n");
            buf.append("<p>").append(processComment(i.comment, infos, i)).append("</p>\n");
            buf.append("<h2>Enumerations</h2>\n");
            buf.append(buildEnums(i, ws, infos));
            buf.append("<h2>Structs</h2>\n");
            buf.append(buildStructs(i, infos, ws));
        }
        return buf.toString();
    }

    public static String buildStructs(MDMInfo info, MDMInfo[] infos, String ws) throws Exception {
        StringBuilder str = new StringBuilder();
        for (int i = 0; i < info.structs.length; i++) {
            StructInfo struct = info.structs[i];
            str.append("<table class=\"st\">\n");
            str.append("<tr><td class=\"name\" colspan=\"2\"><a name=\"").
                    append(info.seriesName).append("_").append("datatype_").append(struct.name).
                    append("\"</a>").append(struct.name).append("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{").append(struct.namespace.replace('/', '.')).append(".").append(struct.name).append("}</td>");
            str.append("<td class=\"id\">ID = ").append(String.valueOf(info.structs[i].id)).append("</td></tr>\n");
            str.append("<tr><td class=\"comment\" colspan=\"3\">");
            if (info.structs[i].extends_name.length() != 0) {
                str.append("<p class=\"id\">extends ").append("<a href=\"#").append(info.structs[i].extends_series).append("_datatype_").
                        append(info.structs[i].extends_name).append("\">").append(info.structs[i].extends_name).append("</a></p>");
            }
            str.append(getSubclassList(infos, info, info.structs[i].name));
            str.append(getfieldList(infos, info, info.structs[i].name));

            if (struct.comment.length() > 0) {
                str.append("\n<p class=\"comment\">").append(processComment(struct.comment, infos, info)).append("</p>\n");
            }

            str.append("</td></tr>\n");


            // if there are inhereited members, list them here with links to their source struct
            ArrayList<StructInfo> inheritVector = new ArrayList<StructInfo>();
            String extendsName = struct.extends_name;
            StructInfo tmpStruct = struct;
            while (extendsName.length() != 0) {
                tmpStruct = MDMInfo.getParentType(infos, tmpStruct);
                if (tmpStruct != null) {
                    inheritVector.add(0, tmpStruct);
                    extendsName = tmpStruct.extends_name;
                }
            }

            if (inheritVector.size() > 0) {
                str.append("<tr><td class=\"fieldsection\" colspan=\"3\">");
                for (StructInfo tmp : inheritVector) {
                    str.append("Members inherited from <a href=\"#").append(tmp.seriesName).append("_datatype_").append(tmp.name).append("\">");
                    str.append(tmp.name).append("</a>: ");
                    for (FieldInfo f : tmp.fields) {
                        str.append(f.name).append(", ");
                    }
                    str.append("<br>");
                }

                str.append("</td></tr>\n");
            }

            // headings for field, type, and units
            str.append("<tr><td class=\"header\" width=60%>Field</td>\n");
            str.append("<td class=\"header\" width=20%>Type</td>\n");
            str.append("<td class=\"header\" width=20%>Units</td>\n");
            str.append("</tr>\n");



            for (int j = 0; j < struct.fields.length; j++) {
                str.append("<tr><td class=\"field\"><a name=\"").append(info.seriesName).append("_datatype_").append(struct.name).append("_field_").
                        append(struct.fields[j].name).append("\"><B>").append(struct.fields[j].name).append("</B></a><br>");
                str.append("<p class=\"comment\">").append(processComment(struct.fields[j].comment, infos, info)).append("</p>\n");
                if (struct.fields[j].defaultVal.length() != 0) {
                    str.append("<p class=\"comment\">Default Value = ").append(struct.fields[j].defaultVal).append("</p></td>\n");
                }

                if (struct.fields[j].isStruct) {
                    if (struct.fields[j].type.equals(MDMInfo.LMCP_OBJECT_NAME)) {
                        str.append("<td class=\"field\" align=center>").append(struct.fields[j].type);
                    }
                    else {
                        StructInfo typeInfo = MDMInfo.getStructByName(infos, struct.fields[j]);
                        str.append("<td class=\"field\" align=center><a href=\"#").append(typeInfo.seriesName).append("_datatype_").append(struct.fields[j].type).
                                append("\">").append(struct.fields[j].type).append("</a>");
                    }
                } else if (struct.fields[j].isEnum) {
                    EnumInfo typeInfo = MDMInfo.getEnumByName(infos, struct.fields[j]);
                    str.append("<td class=\"field\" align=center><a href=\"#").append(typeInfo.seriesName).append("_enum_").append(struct.fields[j].type).
                            append("\">").append(struct.fields[j].type).append("</a>");
                } else {
                    str.append("<td class=\"field\" align=center>").append(struct.fields[j].type);
                }
                if (struct.fields[j].isArray) {
                    str.append("[").append((struct.fields[j].length == -1 ? "" : String.valueOf(struct.fields[j].length))).
                            append("]");
                }
                str.append("</td>\n");

                str.append("<td class=\"field\" align=center>").append(struct.fields[j].units).append("</td></tr>\n");
            }

            str.append("</table>\n");
        }
        return str.toString();
    }

    public static String buildEnums(MDMInfo info, String ws, MDMInfo[] infos) throws Exception {
        StringBuilder str = new StringBuilder();
        for (int i = 0; i < info.enums.length; i++) {
            EnumInfo en = info.enums[i];
            str.append(ws).append("<table class=\"st\">\n");
            str.append("<tr><td class=\"name\" colspan=\"2\"><a name=\"").append(info.seriesName).append("_enum_").append(en.name).
                    append("\"</a>").append(en.name).append("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{").append(en.namespace.replace('/', '.')).append(".").append(en.name).append("}</td>");
            str.append("<tr><td class=\"comment\" colspan=\"3\">");

            if (en.comment.length() > 0) {
                str.append(ws).append("\n<p class=\"comment\">").append(processComment(en.comment, infos, info)).append("</p>\n");
            }

            str.append(ws).append("</td></tr>\n");

            for (EnumInfo.EnumEntry entry : en.entries) {
                str.append(ws).append("<tr><td class=\"field\"><a name=\"").append(info.seriesName).append("_enum_").append(en.name).append("_field_").
                        append(entry.name).append("\"><B>").append(entry.name).append("</B></a>");
                str.append(" ( ").append(entry.value).append(" )");
                str.append("<p class=\"comment\">").append(processComment(entry.comment, infos, info)).append("</p>");
                str.append("</td></tr>\n");
            }

            str.append(ws).append("</table>\n");
        }
        return str.toString();
    }

    public static String print_mdm(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return info.mdmString;
    }

    public static String make_data_links(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuilder buf = new StringBuilder();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            buf.append(ws).append("<H2>Series: ").append(i.seriesName).append("</H2>\n");

            buf.append(ws).append("<H3>Enumerations</H3>\n");
            ArrayList<String> enNames = new ArrayList<String>();
            for (int j = 0; j < i.enums.length; j++) {
                enNames.add(i.enums[j].name);
            }
            //sort alphabetically
            Collections.sort(enNames);
            for (String name : enNames) {
                buf.append("<a href=\"detail.html#").append(i.seriesName).append("_enum_").append(name).append("\"" + "target=\"detail\">").append(name).append("</a> </br>\n");
            }

            buf.append(ws).append("<H3>Structs</H3>\n");
            ArrayList<String> dtNames = new ArrayList<String>();
            for (int j = 0; j < i.structs.length; j++) {
                dtNames.add(i.structs[j].name);
            }
            //sort alphabetically
            Collections.sort(dtNames);
            for (String name : dtNames) {
                buf.append(ws).append("<a href=\"detail.html#").append(i.seriesName).append("_datatype_").append(name).append("\"" + "target=\"detail\">").append(name).append("</a> </br>\n");
            }
        }
        return buf.toString();
    }

//    public static String make_dataview_table(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
//        StringBuilder str = new StringBuilder();
//        for (int i = 0; i < info.structs.length; i++) {
//            String linkStr = "<a href=\"detail.html#" + "datatype_" + info.structs[i].name;
//            str.append("<tr>\n<td class=\"field\">").append(linkStr).append("\">").append(info.structs[i].name).
//                    append("</a><br>(").append(info.structs[i].id).append(")");
//            if (info.structs[i].extends_name.length() != 0) {
//                str.append("<br><I>extends <a href=\"detail.html#").append("datatype_").append(info.structs[i].extends_name).append("\">").append(info.structs[i].extends_name).append("</a></I></td>\n");
//            } else {
//                str.append("</td>\n");
//            }
//
//            str.append("<td class=\"field\">");
//            for (int j = 0; j < info.structs[i].fields.length; j++) {
//                str.append(linkStr).append("_field_").append(info.structs[i].fields[j].name).append("\">").append(info.structs[i].fields[j].name).append("</a>,\n");
//            }
//            str.append("</td>\n");
//
//            str.append("<td class=\"field\">").append(processComment(info.structs[i].comment, infos)).append("</td></tr>\n");
//        }
//        return str.toString();
//    }
    public static String series_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return info.seriesName;
    }

    public static String series_name_list(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuilder buf = new StringBuilder();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            buf.append(ws).append(i.seriesName).append(", ");
        }
        return buf.toString();
    }

    public static String namespace(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return info.namespace;
    }

    public static String format_xml(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return info.mdmString;
    }

//    public static String format_xml(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
//        StringBuilder str = new StringBuilder();
//        str.append("<code class=\"element\">&lt;?xml version=\"1.0\" encoding=\"UTF-8\"?&gt;<br>\n");
//        str.append( "&lt;!DOCTYPE MDM SYSTEM 'MDM.DTD'&gt;</code><br>\n");
//        str.append( "<code class=\"comment\">&lt;!-- <br>" ).append( processComment(info.comment) ).append( "<br> --&gt;</pre><br>\n");
//        str.append( "<code class=\"element\">" ).append( "&lt;MDM&gt;<br></code>\n");
//        str.append( "<code class=\"element\">&lt;SeriesName&gt;<code class=\"type\">" ).append( info.seriesName ).
//                append( "</code>&lt;/SeriesName&gt;</code><br>\n");
//        str.append( "<code class=\"element\">&lt;Namespace&gt;<code class=\"type\">" ).append( info.namespace ).
//                append( "</code>&lt;/Namespace&gt;</code><br>\n");
//        str.append( "<code class=\"element\">&lt;StartID&gt;<code class=\"type\">" ).append( info.startId ).
//                append( "</code>&lt;/StartID&gt;</code><br>\n");
//        str.append( "<br>");
//        str.append( "<code class=\"element\">" ).append( "&lt;MessageList&gt;<br></code>\n");
//        str.append( "<blockquote>\n");
//        for (int i = 0; i < info.structs.length; i++) {
//            StructInfo tmp = info.structs[i];
//            if (tmp.comment.length() != 0)
//                str.append( "<code class=\"comment\">&lt;!-- <br>" ).append( processComment(tmp.comment) ).append( "<br> --&gt;<br>\n");
//            str.append( "<code class=\"element\">&lt;StructInfo");
//            str.append( "<code class=\"attrname\"> Name=</code><code class=\"attrval\">\"" ).append( tmp.name ).append( "\" </code>");
//            if (tmp.extends_name.length() != 0)
//                str.append( "<code class=\"attrname\">Extends=</code><code class=\"attrval\">\"" ).append( tmp.extends_name ).append( "\" </code>");
//            str.append( "&gt;</code><br>\n");
//            
//            str.append( "<blockquote>\n");
//            for (int j = 0; j < tmp.fields.length; j++) {
//                FieldInfo f = tmp.fields[j];
//                if (f.comment.length() != 0)
//                    str.append( "<code class=\"comment\">&lt;!-- <br>" ).append( processComment(f.comment) ).append( "<br> --&gt;<br>\n");
//                str.append( "<code class=\"element\">&lt;" ).append( (f.isArray ? "ArrayField" : "FieldInfo"));
//                str.append( "<code class=\"attrname\"> Name=</code><code class=\"attrval\">\"" ).append( f.name ).append( "\" </code>");
//                str.append( "<code class=\"attrname\"> Type=</code><code class=\"attrval\">\"" ).append( f.type ).append( "\" </code>");
//                if (f.units.length() != 0)
//                    str.append( "<code class=\"attrname\"> Units=</code><code class=\"attrval\">\"" ).append( f.units ).append( "\" </code>");
//                if (f.defaultVal.length() != -1)
//                    str.append( "<code class=\"attrname\"> Default=</code><code class=\"attrval\">\"" ).append( f.defaultVal ).append( "\" </code>");
//                if (f.length != 0 && f.isArray) {
//                    str.append( "<code class=\"attrname\"> Length=</code><code class=\"attrval\">\"" ).append( f.length ).append( "\" </code>");
//                }
//                str.append( "/&gt;</code><br>\n");
//            }
//            str.append( "</blockquote>\n");
//            str.append( "<code class=\"element\">&lt;/StructInfo&gt;</code><br><br>\n");
//            
//        }
//        str.append( "</blockquote>\n");
//        str.append( "<code class=\"element\">" + "&lt;/MessageList&gt;<br></code>\n");
//        str.append( "<code class=\"element\">" + "&lt;/MDM&gt;<br></code>\n");
//        
//        return str.toString();
//    }
    public static String getfieldList(MDMInfo[] infos, MDMInfo info, String dt_name) throws Exception {
        String ret = "";
        for (MDMInfo in : infos) {
            for (int i = 0; i < in.structs.length; i++) {
                FieldInfo[] fields = in.structs[i].fields;
                for (int j = 0; j < fields.length; j++) {
                    if (fields[j].type.equals(dt_name)) {
                        ret += "<a href=\"detail.html#" + in.seriesName + "_datatype_" + in.structs[i].name + "\">" + in.structs[i].name + "</a>, ";
                        break;
                    }
                }
            }
        }
        if (ret.length() != 0) {
            ret = "<p class=\"id\">field of: " + ret + "</p>\n";
        }
        return ret;
    }

    public static String getSubclassList(MDMInfo[] infos, MDMInfo info, String dt_name) throws Exception {
        String ret = "";
        for (MDMInfo in : infos) {
            for (int i = 0; i < in.structs.length; i++) {
                if (in.structs[i].extends_name.equals(dt_name)) {
                    ret += "<a href=\"detail.html#" + in.seriesName + "_datatype_" + in.structs[i].name + "\">" + in.structs[i].name + "</a>, ";
                }
            }
        }
        if (ret.length() != 0) {
            ret = "<p class=\"id\">Known Subtypes: " + ret + "</p>\n";
        }
        return ret;
    }

    public static String processComment(String comment, MDMInfo[] infos, MDMInfo info) {

        String[] attrs = getAttributes(comment, "link");
        for (String attr : attrs) {
            String type = attr;
            MDMInfo sourceInfo = info;
            if (attr.contains("/")) {
                String[] splits = attr.split("/");
                sourceInfo = MDMReader.getMDM(splits[0], infos);
                type = splits[1];
            }
            if (sourceInfo == null) {
                return comment;
            }
            if (sourceInfo.isEnum(type)) {
                comment = replaceAttribute(comment, "link", attr, "<a href=\"detail.html#" + sourceInfo.seriesName + "_enum_" + type + "\">" + type + "</a>");
            }
            else if(sourceInfo.isStruct(type)) {
                comment = replaceAttribute(comment, "link", attr, "<a href=\"detail.html#" + sourceInfo.seriesName + "_datatype_" + type + "\">" + type + "</a>");
            }
        }
        return comment;
    }

    public static String[] getAttributes(String comment, String attr) {
        Pattern p = Pattern.compile("\\{@" + attr + "\\s+[^\\}]*\\}");
        Matcher m = p.matcher(comment);
        ArrayList<String> retList = new ArrayList<String>();

        while (m.find()) {
            String s = comment.substring(m.start() + attr.length() + 3, m.end() - 1);
            retList.add(s);
        }

        return retList.toArray(new String[]{});
    }

    public static String replaceAttribute(String comment, String attr, String attr_detail, String replacement) {
        return comment.replaceAll("\\{@" + attr + "\\s+(" + attr_detail + ")\\}", replacement);
    }

    public static boolean isReserved(String type) {
        return type.matches("(bool)|(byte)|(char)|(real64)|(real32)|(int32)|(int16)|(uint16)|(uint32)|(string)");
    }
}
