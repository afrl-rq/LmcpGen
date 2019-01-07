// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, 
// Power and Control Division
// 
// Copyright (c) 2019 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States 
// under Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

package avtas.lmcp.lmcpgen;

import java.io.File;
import java.nio.file.Files;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.Vector;

public class AdaMethods {

    enum AdaTypeCategory
    {
        SINGLE_PRIMITIVE,
        SINGLE_ENUM,
        SINGLE_NODE_STRUCT,
        SINGLE_LEAF_STRUCT,
        VECTOR_PRIMITIVE,
        VECTOR_ENUM,
        VECTOR_NODE_STRUCT,
        VECTOR_LEAF_STRUCT,
        FIXED_ARRAY_PRIMITIVE,
        FIXED_ARRAY_ENUM,
        FIXED_ARRAY_NODE_STRUCT,
        FIXED_ARRAY_LEAF_STRUCT
    }

    public static AdaTypeCategory getAdaTypeCategory(MDMInfo[] infos, FieldInfo fieldinfo) throws Exception {
        // single
        if(!fieldinfo.isArray) {
            if(!fieldinfo.isStruct && !fieldinfo.isEnum) {
                return AdaTypeCategory.SINGLE_PRIMITIVE;
            }
            if(fieldinfo.isEnum) {
                return AdaTypeCategory.SINGLE_ENUM;
            }
            if(has_descendants(infos, fieldinfo.type, fieldinfo.seriesName)) {
                return AdaTypeCategory.SINGLE_NODE_STRUCT;
            }
            return AdaTypeCategory.SINGLE_LEAF_STRUCT;
        }
        // array of variable length 
        if(fieldinfo.length == -1) {
            if(!fieldinfo.isStruct && !fieldinfo.isEnum) {
                return AdaTypeCategory.VECTOR_PRIMITIVE;
            }
            if(fieldinfo.isEnum) {
                return AdaTypeCategory.VECTOR_ENUM;
            }
            if(has_descendants(infos, fieldinfo.type, fieldinfo.seriesName)) {
                return AdaTypeCategory.VECTOR_NODE_STRUCT;
            }
            return AdaTypeCategory.VECTOR_LEAF_STRUCT;
        }
        // fixed length array
        if(!fieldinfo.isStruct && !fieldinfo.isEnum) {
            return AdaTypeCategory.FIXED_ARRAY_PRIMITIVE;
        }
        if(fieldinfo.isEnum) {
            return AdaTypeCategory.FIXED_ARRAY_ENUM;
        }
        if(has_descendants(infos, fieldinfo.type, fieldinfo.seriesName)) {
            return AdaTypeCategory.FIXED_ARRAY_NODE_STRUCT;
        }
        return AdaTypeCategory.FIXED_ARRAY_LEAF_STRUCT;
    }

    public static String series_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        if (info.seriesName.length() > 8) {
            throw new Exception("Error: Series name must be 8 characters or less.\n");
        }
        return ws + info.seriesName;
    }

    public static String series_id(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + info.seriesNameAsLong;
    }

    public static String series_version(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + info.version;
    }

    public static String series_dir(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + info.namespace;
    }

    public static String full_series_name_dashes(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + info.namespace.replaceAll("/", "-");
    }

    public static String full_series_name_dots(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + info.namespace.replaceAll("/", ".");
    }

    public static String full_datatype_name_dashes(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return getSeriesNamespaceDashes(infos, st.seriesName) + st.name;
    }

    public static String full_datatype_name_dots(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return getSeriesNamespaceDots(infos, st.seriesName) + st.name;
    }

    public static String datatype_id(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.id;
    }

    public static String datatype_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.name;
    }

    public static String datatype_name_caps(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.name.toUpperCase();
    }

    private static String getSeriesNamespaceDots(MDMInfo[] infos, String series_name) {
        MDMInfo i = MDMReader.getMDM(series_name, infos);
        if (i != null) {
            return i.namespace.replaceAll("/", ".") + ".";
        }
        return "";
    }

    private static String getSeriesNamespaceDashes(MDMInfo[] infos, String series_name) {
        MDMInfo i = MDMReader.getMDM(series_name, infos);
        if (i != null) {
            return i.namespace.replaceAll("/", "-") + "-";
        }
        return "";
    }

    public static String full_parent_datatype(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + (st.extends_name.length() == 0 ? info.namespace.replaceAll("/", ".") + ".object.Object" : getSeriesNamespaceDots(infos, st.extends_series) + st.extends_name);
    }
    
    public static String enum_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + en.name;
    }

    public static String enum_name_caps(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + en.name.toUpperCase();
    }

    public static String list_all_message_enumeration_ids(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        // list names with ids
        for (int i = 0; i < info.structs.length; i++) {
            str += ws + info.structs[i].name.toUpperCase() + "_ENUM => " + info.structs[i].id + ",\n";
        }
        return str.replaceAll(",\n$", "\n");
    }

    public static String list_all_message_enumeration_names(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        // list names as enum
        for (int i = 0; i < info.structs.length; i++) {
            str += ws + info.structs[i].name.toUpperCase() + "_ENUM,\n";
        }
        return str.replaceAll(",\n$", "\n");
    }

    public static String gen_enum_names(EnumInfo en) throws Exception {
        StringBuffer buf = new StringBuffer();
        int len = en.entries.size();
        for (int i = 0; i < len; i++) {
            EnumInfo.EnumEntry entry = en.entries.get(i);
            buf.append(entry.name);
            if (i != len - 1) {
                buf.append(",");
            }
        }
        return buf.toString();
    }

    public static String gen_enum_ids(EnumInfo en) throws Exception {
        StringBuffer buf = new StringBuffer();
        int len = en.entries.size();
        for (int i = 0; i < len; i++) {
            EnumInfo.EnumEntry entry = en.entries.get(i);
            buf.append(entry.name + "=>" + entry.value);
            if (i != len - 1) {
                buf.append(",");
            }
        }
        return buf.toString();
    }

    public static String list_all_enumeration_types(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        
        // loop through all enumerations in the MDM
        for (int i = 0; i < info.enums.length; i++) {
            str += ws + "type " + info.enums[i].name + "Enum is (";
            str += gen_enum_names(info.enums[i]);
            str += ");\n";

            str += ws + "for " + info.enums[i].name + "Enum use (";
            str += gen_enum_ids(info.enums[i]);
            str += ");\n\n";
        }
        return str;
    }

    private static String getResolvedTypeName(MDMInfo[] infos, FieldInfo field) {
        return getSeriesNamespaceDots(infos, field.seriesName) + field.type;
    }

    public static String with_all_field_types(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        
        // for all the types of fields, include 'with' statement
        for (int i = 0; i < st.fields.length; i++) {
            if (st.fields[i].isStruct) {
                String fieldtype = getResolvedTypeName(infos, st.fields[i]);
                str += ws + "with " + fieldtype + "; use " + fieldtype + ";\n";
            }
        }

        // if there are any vectors, include vector package
        for (int i = 0; i < st.fields.length; i++) {
            if (st.fields[i].isArray && st.fields[i].length == -1) {
                str += ws + "with Ada.Containers.Vectors;\n";
                break;
            }
        }

        // if any strings, include string package
        for (int i = 0; i < st.fields.length; i++) {
            if (st.fields[i].type.equalsIgnoreCase("string")) {
                str += ws + "with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;\n";
                break;
            }
        }

        str += "\n";
        return str;
    }

    public static String descending_namespace_spec(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {

        String str = "";
        String[] words = info.namespace.split("/");
        File packageFileDir = outfile.getParentFile();

        // all packages before the lowest-level package, working backward
        for(int i = words.length - 2; i >= 0 ; i--) {
            
            packageFileDir = packageFileDir.getParentFile();
            packageFileDir.mkdirs();

            String packageName = "";
            for(int j = 0; j < i; j++) {
                packageName += words[j] + "."; 
            }
            packageName += words[i];
            str = "package " + packageName + " is\n\nend " + packageName + ";\n"; 

            File packageFile = new File(packageFileDir, packageName + ".ads");
            packageFile.createNewFile();
            Files.write(packageFile.toPath(), str.getBytes());
        }

        // lowest level package, filled in with return str by LmcpGen
        String packageName = "";
        packageName = info.namespace.replaceAll("/", "\\.");  ;
        str = "with avtas.lmcp.object; use avtas.lmcp.object;\n";
        str += "with avtas.lmcp.types; use avtas.lmcp.types;\n\n";
        str += "package " + packageName + " is\n\nend " + packageName + ";\n"; 

        return str;
    }

    private static String getAdaPrimativeType(MDMInfo[] infos, FieldInfo field) {
        String type = field.type;
        if (type.equalsIgnoreCase("byte")) {
            return "Byte";
        }
        if (type.equalsIgnoreCase("char")) {
            return "Character";
        }
        if (type.equalsIgnoreCase("bool")) {
            return "Boolean";
        }
        if (type.equalsIgnoreCase("int16")) {
            return "Int16_t";
        }
        if (type.equalsIgnoreCase("uint16")) {
            return "UInt16_t";
        }
        if (type.equalsIgnoreCase("int32")) {
            return "Int32_t";
        }
        if (type.equalsIgnoreCase("uint32")) {
            return "UInt32_t";
        }
        if (type.equalsIgnoreCase("int64")) {
            return "Int64_t";
        }
        if (type.equalsIgnoreCase("real32")) {
            return "Float_t";
        }
        if (type.equalsIgnoreCase("real64")) {
            return "Double_t";
        }
        if (type.equalsIgnoreCase("string")) {
            return "Unbounded_String";
        }
        return type;
    }

    public static String vector_package_import(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        // if there are any vectors, include vector package
        for (int i = 0; i < st.fields.length; i++) {
            switch (getAdaTypeCategory(infos,st.fields[i])) {
                case SINGLE_PRIMITIVE:
                case SINGLE_ENUM:
                case SINGLE_NODE_STRUCT:
                case SINGLE_LEAF_STRUCT:
                    break;
                case VECTOR_PRIMITIVE:
                    String typename = getAdaPrimativeType(infos, st.fields[i]);
                    str += ws + "package Vect_" + typename + " is new Ada.Containers.Vectors\n";
                    str += ws + "  (Index_Type   => Natural,\n";
                    str += ws + "  Element_Type => " + typename + ");\n";
                    str += ws + "type Vect_" + typename + "_Acc is access all Vect_" + typename + ".Vector;\n";
                    break;
                case VECTOR_ENUM:
                    str += ws + "package Vect_" + st.fields[i].type + "Enum is new Ada.Containers.Vectors\n";
                    str += ws + "  (Index_Type   => Natural,\n";
                    str += ws + "  Element_Type => " + st.fields[i].type + "Enum);\n";
                    str += ws + "type Vect_" + st.fields[i].type + "Enum_Acc is access all Vect_" + st.fields[i].type + "Enum.Vector;\n";
                    break;
                case VECTOR_NODE_STRUCT:
                    str += ws + "package Vect_" + st.fields[i].type + "_Any is new Ada.Containers.Vectors\n";
                    str += ws + "  (Index_Type   => Natural,\n";
                    str += ws + "  Element_Type => " + st.fields[i].type + "_Any);\n";
                    str += ws + "type Vect_" + st.fields[i].type + "_Any_Acc is access all Vect_" + st.fields[i].type + "_Any.Vector;\n";
                    break;
                case VECTOR_LEAF_STRUCT:
                    str += ws + "package Vect_" + st.fields[i].type + "_Acc is new Ada.Containers.Vectors\n";
                    str += ws + "  (Index_Type   => Natural,\n";
                    str += ws + "  Element_Type => " + st.fields[i].type + "_Acc);\n";
                    str += ws + "type Vect_" + st.fields[i].type + "_Acc_Acc is access all Vect_" + st.fields[i].type + "_Acc.Vector;\n";                
                    break;
                case FIXED_ARRAY_PRIMITIVE:
                case FIXED_ARRAY_ENUM:
                case FIXED_ARRAY_NODE_STRUCT:
                case FIXED_ARRAY_LEAF_STRUCT:
                default:
                    break;
            }
        }
        return str;
    }

    public static String get_and_set_methods_spec(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        String thisRecordName = st.name + (has_descendants(infos, st.name, st.seriesName) ? "'Class" : "");
        for (int i = 0; i < st.fields.length; i++) {
            switch (getAdaTypeCategory(infos,st.fields[i])) {
                case SINGLE_PRIMITIVE:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return " + getAdaPrimativeType(infos, st.fields[i]) + ";\n";
                    str += ws + "procedure set" + st.fields[i].name + "(this : out " + thisRecordName + "; " + st.fields[i].name + " : in " + getAdaPrimativeType(infos, st.fields[i]) + ");\n";
                    break;  
                case SINGLE_ENUM:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return " + st.fields[i].type + "Enum;\n";
                    str += ws + "procedure set" + st.fields[i].name + "(this : out " + thisRecordName + "; " + st.fields[i].name + " : in " + st.fields[i].type + "Enum);\n";
                    break;
                case SINGLE_NODE_STRUCT:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return " + st.fields[i].type + "_Any;\n";
                    str += ws + "procedure set" + st.fields[i].name + "(this : out " + thisRecordName + "; " + st.fields[i].name + " : in " + st.fields[i].type + "_Any);\n";
                    break;
                case SINGLE_LEAF_STRUCT:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return " + st.fields[i].type + "_Acc;\n";
                    str += ws + "procedure set" + st.fields[i].name + "(this : out " + thisRecordName + "; " + st.fields[i].name + " : in " + st.fields[i].type + "_Acc);\n";        
                    break;
                case VECTOR_PRIMITIVE:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return Vect_" + getAdaPrimativeType(infos, st.fields[i]) + "_Acc;\n";
                    break;
                case VECTOR_ENUM:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return Vect_" + st.fields[i].type + "Enum_Acc;\n";
                    break;
                case VECTOR_NODE_STRUCT:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return Vect_" + st.fields[i].type + "_Any_Acc;\n";
                    break;
                case VECTOR_LEAF_STRUCT:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return Vect_" + st.fields[i].type + "_Acc_Acc;\n";
                    break;    
                case FIXED_ARRAY_PRIMITIVE:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + getAdaPrimativeType(infos, st.fields[i]) + ";\n";
                    break;
                case FIXED_ARRAY_ENUM:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + st.fields[i].type + "Enum;\n";
                    break;
                case FIXED_ARRAY_NODE_STRUCT:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + st.fields[i].type + "_Any;\n";
                    break;
                case FIXED_ARRAY_LEAF_STRUCT:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + st.fields[i].type + "_Acc;\n";
                    break;
                default:
                    break;
            }
        }
        return str;
    }

    public static String get_and_set_methods_body(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        String thisRecordName = st.name + (has_descendants(infos, st.name, st.seriesName) ? "'Class" : "");
        for (int i = 0; i < st.fields.length; i++) {
            switch (getAdaTypeCategory(infos,st.fields[i])) {
                case SINGLE_PRIMITIVE:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return " + getAdaPrimativeType(infos, st.fields[i]) + " is (this." + st.fields[i].name + ");\n";
                    str += ws + "procedure set" + st.fields[i].name + "(this : out " + thisRecordName + "; " + st.fields[i].name + " : in " + getAdaPrimativeType(infos, st.fields[i]) + ") is\n";
                    str += ws + "   this." + st.fields[i].name + " := " + st.fields[i].name + ";\n";
                    str += ws + "end get" + st.fields[i].name + ";\n\n";
                    break;  
                case SINGLE_ENUM:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return " + st.fields[i].type + "Enum is (this." + st.fields[i].name + ");\n";
                    str += ws + "procedure set" + st.fields[i].name + "(this : out " + thisRecordName + "; " + st.fields[i].name + " : in " + st.fields[i].type + "Enum) is\n";
                    str += ws + "   this." + st.fields[i].name + " := " + st.fields[i].name + ";\n";
                    str += ws + "end get" + st.fields[i].name + ";\n\n";
                    break;
                case SINGLE_NODE_STRUCT:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return " + st.fields[i].type + "_Any is (this." + st.fields[i].name + ");\n";
                    str += ws + "procedure set" + st.fields[i].name + "(this : out " + thisRecordName + "; " + st.fields[i].name + " : in " + st.fields[i].type + "_Any) is\n";
                    str += ws + "   this." + st.fields[i].name + " := " + st.fields[i].name + ";\n";
                    str += ws + "end get" + st.fields[i].name + ";\n\n";
                    break;
                case SINGLE_LEAF_STRUCT:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return " + st.fields[i].type + "_Acc is (this." + st.fields[i].name + ");\n";
                    str += ws + "procedure set" + st.fields[i].name + "(this : out " + thisRecordName + "; " + st.fields[i].name + " : in " + st.fields[i].type + "_Acc) is\n";
                    str += ws + "   this." + st.fields[i].name + " := " + st.fields[i].name + ";\n";
                    str += ws + "end get" + st.fields[i].name + ";\n\n";
                    break;
                case VECTOR_PRIMITIVE:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return Vect_" + getAdaPrimativeType(infos, st.fields[i]) + "_Acc is (this." + st.fields[i].name + ");\n";
                    break;
                case VECTOR_ENUM:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return Vect_" + st.fields[i].type + "Enum_Acc;\n";
                    break;
                case VECTOR_NODE_STRUCT:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return Vect_" + st.fields[i].type + "_Any_Acc;\n";
                    break;
                case VECTOR_LEAF_STRUCT:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return Vect_" + st.fields[i].type + "_Acc_Acc;\n";
                    break;    
                case FIXED_ARRAY_PRIMITIVE:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + getAdaPrimativeType(infos, st.fields[i]) + " is (this." + st.fields[i].name +");\n";
                    break;
                case FIXED_ARRAY_ENUM:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + st.fields[i].type + "Enum is (this." + st.fields[i].name + ");\n";
                    break;
                case FIXED_ARRAY_NODE_STRUCT:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + st.fields[i].type + "_Any is (this." + st.fields[i].name + ");\n";
                    break;
                case FIXED_ARRAY_LEAF_STRUCT:
                    str += ws + "function get" + st.fields[i].name + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + st.fields[i].type + "_Acc is (this." + st.fields[i].name + ");\n";
                    break;
                default:
                    break;
            }
        }
        return str;
    }

    public static String record_fields(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        //return str;
        for (int i = 0; i < st.fields.length; i++) {
            String name = st.fields[i].name;
            String type = getResolvedTypeName(infos, st.fields[i]);
            // Add field comment
            str += ws + "--" + st.fields[i].comment.replaceAll("\\s+", " ").replaceAll("<br", "\n" + ws + "*<br") + "\n";
            // Scalars
            if (!st.fields[i].isArray) {
                // Primatives and enums
                if(!st.fields[i].isStruct) {
                    str += ws + name + " : " + getAdaPrimativeType(infos, st.fields[i]) + " := " + getAdaDefaultVal(infos, st.fields[i]) + ";\n";
                }
                else {
                    // Struct with descendents
                    if(has_descendants(infos, st.fields[i].type, st.fields[i].seriesName)) {
                        str += ws + name + " : " + getResolvedTypeName(infos, st.fields[i]) + "_Any" + " := " + getAdaDefaultVal(infos, st.fields[i]) + ";\n";
                    }
                    // Struct without descendents
                    else {
                        str += ws + name + " : " + getResolvedTypeName(infos, st.fields[i]) + "_Acc" + " := " + getAdaDefaultVal(infos, st.fields[i]) + ";\n";;
                    }
                }
            } 
            // Arrays
            else {
                if(st.fields[i].length != -1) {
                    if(!st.fields[i].isStruct && !st.fields[i].isEnum) {
                        str += ws + name + " : array (Integer range 1 .. " + st.fields[i].length + ") of " + getAdaPrimativeType(infos, st.fields[i]) + " := (others => " + getAdaDefaultVal(infos, st.fields[i]) + ");\n";
                    }
                    else if(st.fields[i].isEnum) {
                        str += ws + name + " : array (Integer range 1 .. " + st.fields[i].length + ") of " + getResolvedTypeName(infos, st.fields[i]) + "Enum := (others => " + getAdaDefaultVal(infos, st.fields[i]) + ");\n";
                    }
                    else {
                        if(has_descendants(infos, st.fields[i].type, st.fields[i].seriesName)) {
                            str += ws + name + " : access all array (Integer range 1 .. " + st.fields[i].length + ") of " + getResolvedTypeName(infos, st.fields[i]) + "_Any := (others => " + getAdaDefaultVal(infos, st.fields[i]) + ");\n";
                        }
                        else {
                            str += ws + name + " : access all array (Integer range 1 .. " + st.fields[i].length + ") of " + getResolvedTypeName(infos, st.fields[i]) + "_Acc := (others => " + getAdaDefaultVal(infos, st.fields[i]) + ");\n";
                        }
                    }
                }
                else {
                    // Primatives
                    if(!st.fields[i].isStruct && !st.fields[i].isEnum) {
                        str += ws + name + " : Vect_" + getAdaPrimativeType(infos, st.fields[i]) + "_Acc := new Vect_" + getAdaPrimativeType(infos, st.fields[i]) + ".Vector;\n";
                    } 
                    // Enumerations
                    else if(st.fields[i].isEnum) {
                        str += ws + name + " : Vect_" + st.fields[i].type + "Enum_Acc" + " := new Vect_" + st.fields[i].type + "Enum.Vector;\n";
                    }
                    else {
                        // Structs with descendents
                        if(has_descendants(infos, st.fields[i].type, st.fields[i].seriesName)) {
                            str += ws + name + " : Vect_" + st.fields[i].type + "_Any_Acc" + " := new Vect_" + st.fields[i].type + "_Any.Vector;\n";
                        }
                        // Structs without descendents
                        else {
                            str += ws + name + " : Vect_" + st.fields[i].type + "_Acc_Acc := new Vect_" + st.fields[i].type + "_Acc.Vector;\n";
                        }
                    }
                }
            }
        }
        return str;
    }

    public static String getAdaDefaultVal(MDMInfo[] infos, FieldInfo field) {
        String type = field.type;
        if (!field.defaultVal.isEmpty()) {
            if (type.equalsIgnoreCase("string")) {
                return "To_Unbounded_String(\"" + field.defaultVal + "\")";
            }
            else if (type.equals("char")) {
                return "'" + field.defaultVal + "'";
            }
            else if (type.equals("int64")) {
                return field.defaultVal;
            }
            else if (type.equals("real32")) {
                if (field.defaultVal.contains("."))
                    return field.defaultVal;
                else 
                    return field.defaultVal + ".0";
            }
            else if (field.isStruct) {
                if (field.defaultVal.equalsIgnoreCase("null")) {
                    return "null";
                }
            }
            else if (field.isEnum) {
                return field.defaultVal;
            }
            else {
                return field.defaultVal;
            }
        }
        if (type.equalsIgnoreCase("byte")) {
            return "0";
        }
        if (type.equalsIgnoreCase("char")) {
            return "0";
        }
        if (type.equalsIgnoreCase("bool")) {
            return "False";
        }
        if (type.equalsIgnoreCase("int16")) {
            return "0";
        }
        if (type.equalsIgnoreCase("uint16")) {
            return "0";
        }
        if (type.equalsIgnoreCase("int32")) {
            return "0";
        }
        if (type.equalsIgnoreCase("uint32")) {
            return "0";
        }
        if (type.equalsIgnoreCase("int64")) {
            return "0";
        }
        if (type.equalsIgnoreCase("real32")) {
            return "0.0";
        }
        if (type.equalsIgnoreCase("real64")) {
            return "0.0";
        }
        if (type.equalsIgnoreCase("string")) {
            return "To_Unbounded_String(\"\")";
        }
        if (field.isEnum) {
            return MDMInfo.getEnumByName(infos, field).entries.get(0).name;
        }
        return "new " + getResolvedTypeName(infos, field); // for objects
    }

    public static boolean has_descendants(MDMInfo[] infos, String stname, String stseries) throws Exception {
        List<String> descendants = new ArrayList<String>();
        add_descendants(infos, stname, stseries, descendants);
        if( descendants.size() > 0) {
            return true;
        }
        return false;
    }
    
    public static void add_descendants(MDMInfo[] infos, String typename, String seriesname, List<String> descendants) {
        for (MDMInfo in : infos) {
            for (int i = 0; i < in.structs.length; i++) {
                if (in.structs[i].extends_name.equals(typename) && in.structs[i].extends_series.equals(seriesname)) {
                    String child = in.structs[i].namespace.replace('/', '.') + "." + in.structs[i].name;
                    if(!descendants.contains(child)) {
                        descendants.add(child);
                        add_descendants(infos, in.structs[i].name, in.structs[i].seriesName, descendants);
                    }
                }
            }
        }
    }

};