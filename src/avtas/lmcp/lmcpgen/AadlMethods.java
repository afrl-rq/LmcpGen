// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2018 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

package avtas.lmcp.lmcpgen;

import java.io.File;

public class AadlMethods
{
    public static String series_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        if (info.seriesName.length() > 8) {
            throw new Exception("Error: Series name must be 8 characters or less.\n");
        }
        return ws + info.seriesName;
    }
    
    public static String mdm_inclusion(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (String dep : info.mdmDependencies) {
            for (MDMInfo i : infos) {
                if (i.seriesName.contentEquals(dep))
                {
                    str += ws + "with " + dep + ";\n";
                }
            }
        }
        return str;
    }
    
    public static String list_all_messages(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        
        for (EnumInfo n : info.enums) {
            str += wrap_comment(ws + "--", n.comment);
            str += ws + "data " + n.name + " extends Base_Types::Integer\n";
            str += ws + "\tproperties\n";
            str += ws + "\t\tData_Model::Enumerators => (\n";
            for(EnumInfo.EnumEntry e : n.entries) {
                str += wrap_comment(ws + "\t\t\t--", e.comment);
                str += ws + "\t\t\t\"" + e.name + "\",\n";
            }
            str = str.substring(0, str.length()-2);
            str += ");\n";
            str += ws + "end " + n.name + ";\n\n";
            
            str += ws + "data implementation " + n.name + ".i\n";
            str += ws + "end " + n.name + ".i;\n\n";
        }
        
        for (StructInfo s : info.structs) {
            if(s.extends_name.length() > 0) {
                str += ws + "data " + s.name + " extends " + s.extends_series + "::" + s.extends_name + "\n";
            } else {
                str += ws + "data " + s.name + "\n";
            }
            str += ws + "end " + s.name + ";\n\n";
            
            str += wrap_comment(ws + "--", s.comment);
            if(s.extends_name.length() > 0) {
                str += ws + "data implementation " + s.name + ".i extends " + s.extends_series + "::" + s.extends_name + ".i\n";
            } else {
                str += ws + "data implementation " + s.name + ".i\n";
            }
            
            if(s.fields.length > 0) {
                str += ws + "\tsubcomponents\n";
            }
            
            for (FieldInfo f : s.fields) {
                str += wrap_comment(ws + "\t\t--", f.comment);
                str += ws + "\t\t" + f.name + ": data " + f.type + ".i";
                if(f.isArray) {
                    if(f.length > 0) {
                        str += " {\n\t\t\tData_Model::Data_Representation => Array;\n";
                        str += "\t\t\tData_Model::Dimension => (" + String.valueOf(f.length) + ");\n\t\t}";
                    } else {
                        str += " {Data_Model::Data_Representation => Array;}";
                    }
                }
                str += ";\n";
            }
            
            str += ws + "end " + s.name + ".i;\n\n";
        }
        return str;
    }
    
    private static String wrap_comment(String ws, String comment) {
        //return ws + comment.replaceAll("\\s+", " ").trim() + "\n";
        
        String trimmed = comment.replaceAll("\\s+", " ").trim();
        if(trimmed.isEmpty()) {
            return "";
        }
        
        String [] words = trimmed.split(" ");
        String str = "";
        int w = 0;
        while(w < words.length) {
            // because of the way LmcpGen parses the template files, any -<___>-
            // matches a function call. Since AADL starts comments with '--',
            // there is a danger that a -<___>- style function could be created.
            // avoid this by adding a space if the word starts with '<'
            String addspace = words[w].startsWith("<") ? " " : "";
            String nextline = ws + addspace + words[w];
            w = w + 1;
            while(!nextline.endsWith("<br/>") && nextline.length() < 80 && w < words.length) {
                nextline += " " + words[w];
                w = w + 1;
            }
            str += nextline + "\n";
        }
        return str;
    }
};
