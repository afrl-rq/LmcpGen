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
import java.util.HashSet;
import java.util.Set;

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
        // single variables
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
        // vectors (variable length) 
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
        // arrays (fixed length)
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

    public static final Set<String> adaReservedWords = new HashSet<String>(Arrays.asList("loop", "task"));

    private static String getDeconflictedName(String name) {
        return (adaReservedWords.contains(name.toLowerCase()) ? "Lmcp" + name : name );
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
        return getSeriesNamespaceDashes(infos, st.seriesName) + getDeconflictedName(st.name);
    }

    public static String full_datatype_name_dashes_lowercase(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return getSeriesNamespaceDashes(infos, st.seriesName).toLowerCase() + getDeconflictedName(st.name).toLowerCase();
    }

    public static String full_datatype_name_dots(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return getSeriesNamespaceDots(infos, st.seriesName) + getDeconflictedName(st.name);
    }

    public static String datatype_id(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.id;
    }

    public static String datatype_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + getDeconflictedName(st.name);
    }

    public static String datatype_name_caps(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + getDeconflictedName(st.name).toUpperCase();
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

    public static String full_parent_datatype_package(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + (st.extends_name.length() == 0 ? info.namespace.replaceAll("/", ".") + ".object" : getSeriesNamespaceDots(infos, st.extends_series) + getDeconflictedName(st.extends_name));
    }

    public static String full_parent_datatype(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + (st.extends_name.length() == 0 ? info.namespace.replaceAll("/", ".") + ".object.Object" : ws + getSeriesNamespaceDots(infos, st.extends_series) + getDeconflictedName(st.extends_name) + "." + getDeconflictedName(st.extends_name));
    }

    public static String getFullParentDatatype(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return (st.extends_name.length() == 0 ? info.namespace.replaceAll("/", ".") + ".object.Object" : ws + getSeriesNamespaceDots(infos, st.extends_series) + getDeconflictedName(st.extends_name) + "." + getDeconflictedName(st.extends_name));
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
            str += ws + getDeconflictedName(info.structs[i].name).toUpperCase() + "_ENUM => " + info.structs[i].id + ",\n";
        }
        return str.replaceAll(",\n$", "\n");
    }

    public static String list_all_message_enumeration_names(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        // list names as enum
        for (int i = 0; i < info.structs.length; i++) {
            str += ws + getDeconflictedName(info.structs[i].name).toUpperCase() + "_ENUM,\n";
        }
        return str.replaceAll(",\n$", "\n");
    }

    public static String gen_enum_names(EnumInfo en) throws Exception {
        StringBuffer buf = new StringBuffer();
        int len = en.entries.size();
        for (int i = 0; i < len; i++) {
            EnumInfo.EnumEntry entry = en.entries.get(i);
            buf.append(getDeconflictedName(entry.name));
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
            buf.append(getDeconflictedName(entry.name) + "=>" + entry.value);
            if (i != len - 1) {
                buf.append(",");
            }
        }
        return buf.toString();
    }

    public static String gen_enum_to_int(EnumInfo en) throws Exception {
        StringBuffer buf = new StringBuffer();
        int len = en.entries.size();
        for (int i = 0; i < len; i++) {
            EnumInfo.EnumEntry entry = en.entries.get(i);
            buf.append("when " + getDeconflictedName(entry.name) + " => " + entry.value);
            if (i != len - 1) {
                buf.append(", ");
            }
        }
        return buf.toString();
    }

    public static String gen_int_to_enum(EnumInfo en) throws Exception {
        StringBuffer buf = new StringBuffer();
        int len = en.entries.size();
        for (int i = 0; i < len; i++) {
            EnumInfo.EnumEntry entry = en.entries.get(i);
            buf.append("when " + entry.value + " => " + getDeconflictedName(entry.name) + ", ");
        }
        buf.append("when others => raise Constraint_Error");
        return buf.toString();
    }

    public static String list_all_enumeration_types(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        
        // loop through all enumerations in the MDM
        for (int i = 0; i < info.enums.length; i++) {
            str += "\n" + ws + "type " + getDeconflictedName(info.enums[i].name) + "Enum is (";
            str += gen_enum_names(info.enums[i]);
            str += ");\n";

            str += ws + "function toInt32(enum : " + getDeconflictedName(info.enums[i].name) + "Enum) return Int32_t is\n";
            str += ws + "   (case enum is " + gen_enum_to_int(info.enums[i]) + ");\n";

            str += ws + "function toEnum(val : Int32_t) return " + getDeconflictedName(info.enums[i].name) + "Enum is\n";
            str += ws + "   (case val is " + gen_int_to_enum(info.enums[i]) + ");\n";

            /* Old implementation that explicitly numbers the enumeration, but it requires doing an 
               Unchecked_Conversion in the resulting Ada code to go from Enum <-> Int32 */
            /* str += ws + "for " + getDeconflictedName(info.enums[i].name) + "Enum use (";
            str += gen_enum_ids(info.enums[i]);
            str += ");"; */
        }
        return str;
    }

    private static String getResolvedTypeName(MDMInfo[] infos, FieldInfo field) {
        return getSeriesNamespaceDots(infos, field.seriesName) + getDeconflictedName(field.type);
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

        // For <namespace_1>/<namespace_2>/ ... <namespace_n>, create files with empty package definitions for 
        // <namespace_1>, <namespace_1>.<namespace_2>, through <namespace_1>.<namespace_2> ... .<namespace_n-1>
        // and return basic package definition for <namespace_1>.<namespace_2> ... .<namespace_n>

        String str = "";
        String[] words = info.namespace.split("/");
        File packageFileDir = outfile.getParentFile();

        // Packages before the lowest-level package, starting at n-1
        for(int i = words.length - 2; i >= 0 ; i--) {
            
            String packageName = "";
            for(int j = 0; j < i; j++) {
                packageName += words[j] + "."; 
            }
            packageName += words[i];
            str = "package " + packageName + " is\n\nend " + packageName + ";\n"; 

            packageFileDir = packageFileDir.getParentFile();
            packageFileDir.mkdirs();
            File packageFile = new File(packageFileDir, packageName + ".ads");
            packageFile.createNewFile();
            Files.write(packageFile.toPath(), str.getBytes());
        }

        // Lowest level package definition text, returned as str
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
        // if there are any vectors, include vector package, keeping track to avoid duplicates
        Set<String> vectTypes = new HashSet<String>();
        for (int i = 0; i < st.fields.length; i++) {
            switch (getAdaTypeCategory(infos,st.fields[i])) {
                case SINGLE_PRIMITIVE:
                case SINGLE_ENUM:
                case SINGLE_NODE_STRUCT:
                case SINGLE_LEAF_STRUCT:
                    break;
                case VECTOR_PRIMITIVE:
                    String typename = getAdaPrimativeType(infos, st.fields[i]);
                    if(!vectTypes.contains(typename)) {
                        vectTypes.add(typename);
                        str += ws + "package Vect_" + typename + " is new Ada.Containers.Vectors\n";
                        str += ws + "  (Index_Type   => Natural,\n";
                        str += ws + "  Element_Type => " + typename + ");\n";
                        str += ws + "type Vect_" + typename + "_Acc is access all Vect_" + typename + ".Vector;\n";
                    }
                    break;
                case VECTOR_ENUM:
                    if(!vectTypes.contains(getDeconflictedName(st.fields[i].type))) {
                        vectTypes.add(getDeconflictedName(st.fields[i].type));
                        str += ws + "package Vect_" + getDeconflictedName(st.fields[i].type) + "Enum is new Ada.Containers.Vectors\n";
                        str += ws + "  (Index_Type   => Natural,\n";
                        str += ws + "  Element_Type => " + getDeconflictedName(st.fields[i].type) + "Enum);\n";
                        str += ws + "type Vect_" + getDeconflictedName(st.fields[i].type) + "Enum_Acc is access all Vect_" + getDeconflictedName(st.fields[i].type) + "Enum.Vector;\n";
                    }
                    break;
                case VECTOR_NODE_STRUCT:
                    if(!vectTypes.contains(getDeconflictedName(st.fields[i].type))) {
                        vectTypes.add(getDeconflictedName(st.fields[i].type));
                        str += ws + "package Vect_" + getDeconflictedName(st.fields[i].type) + "_Any is new Ada.Containers.Vectors\n";
                        str += ws + "  (Index_Type   => Natural,\n";
                        str += ws + "  Element_Type => " + getDeconflictedName(st.fields[i].type) + "_Any);\n";
                        str += ws + "type Vect_" + getDeconflictedName(st.fields[i].type) + "_Any_Acc is access all Vect_" + getDeconflictedName(st.fields[i].type) + "_Any.Vector;\n";
                    }
                    break;
                case VECTOR_LEAF_STRUCT:
                    if(!vectTypes.contains(getDeconflictedName(st.fields[i].type))) {
                        vectTypes.add(getDeconflictedName(st.fields[i].type));
                        str += ws + "package Vect_" + getDeconflictedName(st.fields[i].type) + "_Acc is new Ada.Containers.Vectors\n";
                        str += ws + "  (Index_Type   => Natural,\n";
                        str += ws + "  Element_Type => " + getDeconflictedName(st.fields[i].type) + "_Acc);\n";
                        str += ws + "type Vect_" + getDeconflictedName(st.fields[i].type) + "_Acc_Acc is access all Vect_" + getDeconflictedName(st.fields[i].type) + "_Acc.Vector;\n";                
                    }
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
        // String thisRecordName = getDeconflictedName(st.name) + (has_descendants(infos, st.name, st.seriesName) ? "'Class" : "");
        String thisRecordName = getDeconflictedName(st.name);
        for (int i = 0; i < st.fields.length; i++) {
            String fieldname = getDeconflictedName(st.fields[i].name);
            String type = getDeconflictedName(st.fields[i].type);
            switch (getAdaTypeCategory(infos,st.fields[i])) {
                case SINGLE_PRIMITIVE:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return " + getAdaPrimativeType(infos, st.fields[i]) + ";\n";
                    str += ws + "procedure set" + fieldname + "(this : out " + thisRecordName + "; " + fieldname + " : in " + getAdaPrimativeType(infos, st.fields[i]) + ");\n";
                    break;  
                case SINGLE_ENUM:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return " + type + "Enum;\n";
                    str += ws + "procedure set" + fieldname + "(this : out " + thisRecordName + "; " + fieldname + " : in " + type + "Enum);\n";
                    break;
                case SINGLE_NODE_STRUCT:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return " + type + "_Any;\n";
                    str += ws + "procedure set" + fieldname + "(this : out " + thisRecordName + "; " + fieldname + " : in " + type + "_Any);\n";
                    break;
                case SINGLE_LEAF_STRUCT:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return " + type + "_Acc;\n";
                    str += ws + "procedure set" + fieldname + "(this : out " + thisRecordName + "; " + fieldname + " : in " + type + "_Acc);\n";        
                    break;
                case VECTOR_PRIMITIVE:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return Vect_" + getAdaPrimativeType(infos, st.fields[i]) + "_Acc;\n";
                    break;
                case VECTOR_ENUM:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return Vect_" + type + "Enum_Acc;\n";
                    break;
                case VECTOR_NODE_STRUCT:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return Vect_" + type + "_Any_Acc;\n";
                    break;
                case VECTOR_LEAF_STRUCT:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return Vect_" + type + "_Acc_Acc;\n";
                    break;    
                case FIXED_ARRAY_PRIMITIVE:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + getAdaPrimativeType(infos, st.fields[i]) + ";\n";
                    break;
                case FIXED_ARRAY_ENUM:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + type + "Enum;\n";
                    break;
                case FIXED_ARRAY_NODE_STRUCT:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + type + "_Any;\n";
                    break;
                case FIXED_ARRAY_LEAF_STRUCT:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + type + "_Acc;\n";
                    break;
                default:
                    break;
            }
        }
        return str;
    }

    public static String get_and_set_methods_body(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        // String thisRecordName = getDeconflictedName(st.name); + (has_descendants(infos, st.name, st.seriesName) ? "'Class" : "");
        String thisRecordName = getDeconflictedName(st.name);
        for (int i = 0; i < st.fields.length; i++) {
            String fieldname = getDeconflictedName(st.fields[i].name);
            String type = getDeconflictedName(st.fields[i].type);
            switch (getAdaTypeCategory(infos,st.fields[i])) {
                case SINGLE_PRIMITIVE:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return " + getAdaPrimativeType(infos, st.fields[i]) + " is (this." + fieldname + ");\n";
                    str += ws + "procedure set" + fieldname + "(this : out " + thisRecordName + "; " + fieldname + " : in " + getAdaPrimativeType(infos, st.fields[i]) + ") is\n" + ws + "begin\n";
                    str += ws + "   this." + fieldname + " := " + fieldname + ";\n";
                    str += ws + "end set" + fieldname + ";\n\n";
                    break;  
                case SINGLE_ENUM:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return " + type + "Enum is (this." + fieldname + ");\n";
                    str += ws + "procedure set" + fieldname + "(this : out " + thisRecordName + "; " + fieldname + " : in " + type + "Enum) is\n" + ws + "begin\n";
                    str += ws + "   this." + fieldname + " := " + fieldname + ";\n";
                    str += ws + "end set" + fieldname + ";\n\n";
                    break;
                case SINGLE_NODE_STRUCT:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return " + type + "_Any is (this." + fieldname + ");\n";
                    str += ws + "procedure set" + fieldname + "(this : out " + thisRecordName + "; " + fieldname + " : in " + type + "_Any) is\n" + ws + "begin\n";
                    str += ws + "   this." + fieldname + " := " + fieldname + ";\n";
                    str += ws + "end set" + fieldname + ";\n\n";
                    break;
                case SINGLE_LEAF_STRUCT:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return " + type + "_Acc is (this." + fieldname + ");\n";
                    str += ws + "procedure set" + fieldname + "(this : out " + thisRecordName + "; " + fieldname + " : in " + type + "_Acc) is\n" + ws + "begin\n";
                    str += ws + "   this." + fieldname + " := " + fieldname + ";\n";
                    str += ws + "end set" + fieldname + ";\n\n";
                    break;
                case VECTOR_PRIMITIVE:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return Vect_" + getAdaPrimativeType(infos, st.fields[i]) + "_Acc is (this." + fieldname + ");\n";
                    break;
                case VECTOR_ENUM:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return Vect_" + type + "Enum_Acc is (this." + fieldname + ");\n";
                    break;
                case VECTOR_NODE_STRUCT:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return Vect_" + type + "_Any_Acc is (this." + fieldname + ");\n";
                    break;
                case VECTOR_LEAF_STRUCT:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return Vect_" + type + "_Acc_Acc is (this." + fieldname + ");\n";
                    break;    
                case FIXED_ARRAY_PRIMITIVE:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + getAdaPrimativeType(infos, st.fields[i]) + " is (this." + fieldname +");\n";
                    break;
                case FIXED_ARRAY_ENUM:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + type + "Enum is (this." + fieldname + ");\n";
                    break;
                case FIXED_ARRAY_NODE_STRUCT:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + type + "_Any is (this." + fieldname + ");\n";
                    break;
                case FIXED_ARRAY_LEAF_STRUCT:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + type + "_Acc is (this." + fieldname + ");\n";
                    break;
                default:
                    break;
            }
        }
        return str;
    }

    public static String record_fields(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        if (st.fields.length == 0) {
            return ws + "null;";
        }
        String str = "";
        for (int i = 0; i < st.fields.length; i++) {
            String fieldname = getDeconflictedName(st.fields[i].name);
            String type = getDeconflictedName(st.fields[i].type);
            // Add field comment
            str += ws + "--" + st.fields[i].comment.replaceAll("\\s+", " ").replaceAll("<br/>", "\n" + ws + "--") + "\n";
            switch (getAdaTypeCategory(infos,st.fields[i])) {
                case SINGLE_PRIMITIVE:
                    str += ws + fieldname + " : " + getAdaPrimativeType(infos, st.fields[i]) + " := " + getAdaDefaultVal(infos, st.fields[i]) + ";\n";
                    break;  
                case SINGLE_ENUM:
                    str += ws + fieldname + " : " + getSeriesNamespaceDots(infos, st.fields[i].seriesName) + "enumerations." + type + "Enum := " + getAdaDefaultVal(infos, st.fields[i]) + ";\n";
                    break;
                case SINGLE_NODE_STRUCT:
                    str += ws + fieldname + " : " + getResolvedTypeName(infos, st.fields[i]) + "." + type + "_Any" + " := " + getAdaDefaultVal(infos, st.fields[i]) + ";\n";
                    break;
                case SINGLE_LEAF_STRUCT:
                    str += ws + fieldname + " : " + getResolvedTypeName(infos, st.fields[i]) + "." + type + "_Acc" + " := " + getAdaDefaultVal(infos, st.fields[i]) + ";\n";
                    break;
                case VECTOR_PRIMITIVE:
                    str += ws + fieldname + " : Vect_" + getAdaPrimativeType(infos, st.fields[i]) + "_Acc := new Vect_" + getAdaPrimativeType(infos, st.fields[i]) + ".Vector;\n";
                    break;
                case VECTOR_ENUM:
                    str += ws + fieldname + " : Vect_" + type + "Enum_Acc" + " := new Vect_" + type + "Enum.Vector;\n";
                    break;
                case VECTOR_NODE_STRUCT:
                    str += ws + fieldname + " : Vect_" + type + "_Any_Acc" + " := new Vect_" + type + "_Any.Vector;\n";
                    break;
                case VECTOR_LEAF_STRUCT:
                    str += ws + fieldname + " : Vect_" + type + "_Acc_Acc := new Vect_" + type + "_Acc.Vector;\n";
                    break;    
                case FIXED_ARRAY_PRIMITIVE:
                    str += ws + fieldname + " : access all array (Integer range 1 .. " + st.fields[i].length + ") of " + getAdaPrimativeType(infos, st.fields[i]) + " := (others => " + getAdaDefaultVal(infos, st.fields[i]) + ");\n";
                    break;
                case FIXED_ARRAY_ENUM:
                    str += ws + fieldname + " : access all array (Integer range 1 .. " + st.fields[i].length + ") of " + getResolvedTypeName(infos, st.fields[i]) + "Enum := (others => " + getAdaDefaultVal(infos, st.fields[i]) + ");\n";
                    break;
                case FIXED_ARRAY_NODE_STRUCT:
                    str += ws + fieldname + " : access all array (Integer range 1 .. " + st.fields[i].length + ") of " + getResolvedTypeName(infos, st.fields[i]) + "_Any := (others => " + getAdaDefaultVal(infos, st.fields[i]) + ");\n";
                    break;
                case FIXED_ARRAY_LEAF_STRUCT:
                    str += ws + fieldname + " : access all array (Integer range 1 .. " + st.fields[i].length + ") of " + getResolvedTypeName(infos, st.fields[i]) + "_Acc := (others => " + getAdaDefaultVal(infos, st.fields[i]) + ");\n";
                    break;
                default:
                    break;
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
            else if (type.equals("real64")) {
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
        return "new " + getResolvedTypeName(infos, field) + "." + getDeconflictedName(type); // for objects
    }

    public static boolean has_descendants(MDMInfo[] infos, String stname, String stseries) throws Exception {
        List<String> descendants = new ArrayList<String>();
        add_descendants(infos, stname, stseries, descendants);
        if(descendants.size() > 0) {
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

    public static String global_factory_switch(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        buf.append(ws + "case seriesId is\n");
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            buf.append(ws + "   when " + i.seriesNameAsLong + " => return " + i.namespace.replaceAll("/", ".") + ".factory.createObject(seriesId, msgType, version);\n");
        }
        buf.append(ws + "   when others => return null;\n");
        buf.append(ws + "end case;");
        return buf.toString();
    }

    public static String series_factory_switch(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        
        buf.append(ws + "if seriesId = " + info.seriesNameAsLong + " and then version = " + info.version + " then\n");
        buf.append(ws + "   case msgType is\n");
        for (int j = 0; j < info.structs.length; j++) {
            buf.append(ws + "      when " + info.structs[j].id + " => return new " + info.namespace.replaceAll("/", ".") + "."
                    + getDeconflictedName(info.structs[j].name) + "." + getDeconflictedName(info.structs[j].name) + "; \n");
        }
        buf.append(ws + "      when others => return null;\n");
        buf.append(ws + "   end case;\n");
        buf.append(ws + "else\n");
        buf.append(ws + "   return null;\n");
        buf.append(ws + "end if;");
        
        return buf.toString();
    }

    public static String include_all_factories(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            buf.append(ws + "with " + i.namespace.replaceAll("/", ".") + "." + "factory;\n");
        }
        return buf.toString();
    }

    public static String include_all_series_headers(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        str += ws + "with " + info.namespace.replaceAll("/", ".") + ".enumerations;\n";
        str += ws + "with " + info.namespace.replaceAll("/", ".") + ".object;\n";
        for (int i = 0; i < info.structs.length; i++) {
            str += ws + "with " + info.namespace.replaceAll("/", ".") + "." + getDeconflictedName(info.structs[i].name) + ";\n";
        }
        return str;
    }

    public static String pack_body(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        String parentDatatype = getFullParentDatatype(infos, info, outfile, st, en, ws);
        // TODO: Change spec and body to be more precise with Acc and Any
        if(has_descendants(infos, st.name, st.seriesName)) {
            str += ws + "procedure pack(this : in " + getDeconflictedName(st.name) + "_Any; buf : in out ByteBuffer) is\n";
        }
        else {
            str += ws + "procedure pack(this : in " + getDeconflictedName(st.name) + "_Any; buf : in out ByteBuffer) is\n";
        }
        str += ws + "begin\n";
        str += ws + "   pack(" + parentDatatype +"_Any(this), buf);\n";
        for (int i = 0; i < st.fields.length; i++) {
            String fieldname = getDeconflictedName(st.fields[i].name);
            switch (getAdaTypeCategory(infos,st.fields[i])) {
                case SINGLE_PRIMITIVE:
                        str += ws + "   Put_" + getAdaPrimativeType(infos, st.fields[i]) + "(this." + fieldname + ", buf);\n";
                    break;
                case SINGLE_ENUM:
                    str += ws + "   Put_Int32_t(toInt32(this." + fieldname + "), buf);\n";
                    break; 
                case SINGLE_NODE_STRUCT:
                case SINGLE_LEAF_STRUCT:
                    str += ws + "   avtas.lmcp.factory.putObject(avtas.lmcp.object.Object_Any(this." + fieldname + "), buf);\n";
                    break;
                case VECTOR_PRIMITIVE:
                    if (st.fields[i].isLargeArray) {
                        str += ws + "   Put_UInt64_t(UInt64_t(this." + fieldname + ".Length), buf);\n";
                    }
                    else {
                        str += ws + "   Put_UInt32_t(UInt32_t(this." + fieldname + ".Length), buf);\n";
                    }
                    str += ws + "   for i of this." + fieldname + ".all loop\n";
                    str += ws + "      Put_" + getAdaPrimativeType(infos, st.fields[i]) + "(i, buf);\n";
                    str += ws + "   end loop;\n";
                    break;
                case VECTOR_ENUM:
                    if (st.fields[i].isLargeArray) {
                        str += ws + "   Put_UInt64_t(UInt64_t(this." + fieldname + ".Length), buf);\n";
                    }
                    else {
                        str += ws + "   Put_UInt32_t(UInt32_t(this." + fieldname + ".Length), buf);\n";
                    }
                    str += ws + "   for i of this." + fieldname + ".all loop\n";
                    str += ws + "      Put_Int32_t(toInt32(i), buf);\n";
                    str += ws + "   end loop;\n";
                    break;
                case VECTOR_NODE_STRUCT:
                case VECTOR_LEAF_STRUCT:
                    if (st.fields[i].isLargeArray) {
                        str += ws + "   Put_UInt64_t(UInt_64_t(this." + fieldname + ".Length), buf);\n";
                    }
                    else {
                        str += ws + "   Put_UInt32_t(UInt32_t(this." + fieldname + ".Length), buf);\n";
                    }
                    str += ws + "   for i of this." + fieldname + ".all loop\n";
                    str += ws + "      avtas.lmcp.factory.putObject(avtas.lmcp.object.Object_Any(i), buf);\n";
                    str += ws + "   end loop;\n";
                    break;
                case FIXED_ARRAY_PRIMITIVE:
                    str += ws + "   Put_UInt32_t(UInt32_t(this." + fieldname + "'Length), buf);\n";
                    str += ws + "   for i of this." + fieldname + ".all loop\n";
                    str += ws + "      Put_" + getAdaPrimativeType(infos, st.fields[i]) + "(i, buf);\n";
                    str += ws + "   end loop;\n";
                    break;
                case FIXED_ARRAY_ENUM:
                    str += ws + "   Put_UInt32_t(UInt32_t(this." + fieldname + "'Length), buf);\n";
                    str += ws + "   for i of this." + fieldname + ".all loop\n";
                    str += ws + "      Put_Int32_t(toInt32(i), buf);\n";
                    str += ws + "   end loop;\n";
                    break;
                case FIXED_ARRAY_NODE_STRUCT:
                case FIXED_ARRAY_LEAF_STRUCT:
                    str += ws + "   Put_UInt32_t(UInt32_t(this." + fieldname + "'Length), buf);\n";
                    str += ws + "   for i of this." + fieldname + ".all loop\n";
                    str += ws + "      avtas.lmcp.factory.putObject(avtas.lmcp.object.Object_Any(i), buf);\n";
                    str += ws + "   end loop;\n";
                    break;
                default:
                    break;
            }
        
        }   
        str += ws + "end pack;";
        return str;
    };

    public static String unpack_body(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        // TODO : be precise between Acc and Any in body and spec
        str += ws + "procedure unpack(this : in out " + getDeconflictedName(st.name) + "_Any; buf : in out ByteBuffer) is\n";
        str += ws + "begin\n";
        str += ws + "   null;\n";
        str += ws + "end unpack;";
        return str;
    };

    public static String calculate_packed_size_body(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        str += ws + "function calculatePackedSize(this : " + getDeconflictedName(st.name) + ") return UInt32_t is\n";
        str += ws + "  size : UInt32_t := 0;\n";
        str += ws + "begin\n";
        for (int i = 0; i < st.fields.length; i++) {
             switch (getAdaTypeCategory(infos,st.fields[i])) {
                case SINGLE_PRIMITIVE:
                    if(st.fields[i].type.equalsIgnoreCase("string")) {
                        str += ws + "   size := size + 2 + UInt32_t(Length(this." + getDeconflictedName(st.fields[i].name) + "))*Character\'Size/8;\n";
                    }
                    else {
                        str += ws + "   size := size + " + getAdaPrimativeType(infos, st.fields[i]) + "\'Size/8;\n";
                    }
                    break;
                case SINGLE_ENUM:
                    str += ws + "   size := size + Int32_t\'Size/8;\n";
                    break; 
                case SINGLE_NODE_STRUCT:
                case SINGLE_LEAF_STRUCT:
                    str += "   if this." + getDeconflictedName(st.fields[i].name) + " = null then\n";
                    str += "      size := size + 1;\n";
                    str += "   else\n";
                    str += "      size := size + 15 + calculatePackedSize(this." + getDeconflictedName(st.fields[i].name) + ".all);\n";
                    str += "   end if;\n";
                    break;
                case VECTOR_PRIMITIVE:
                    if (st.fields[i].isLargeArray) {
                        str += ws + "   size := size + 4;\n";
                    }
                    else {
                        str += ws + "   size := size + 2;\n";
                    }
                    if(st.fields[i].type.equalsIgnoreCase("string")) {
                        str += ws + "   for i of this." + getDeconflictedName(st.fields[i].name) + " loop\n";
                        str += ws + "      size := size + 2 + UInt32_t(Length(i))*Character\'Size/8;\n";
                        str += ws + "   end loop;\n";
                    }
                    else {
                        str += ws + "   size := size + UInt32_t(this." + getDeconflictedName(st.fields[i].name) + ".Length)*" + getAdaPrimativeType(infos, st.fields[i]) + "\'Size/8;\n";
                    }
                    break;
                case VECTOR_ENUM:
                    if (st.fields[i].isLargeArray) {
                        str += ws + "   size := size + 4;\n";
                    }
                    else {
                        str += ws + "   size := size + 2;\n";
                    }
                    str += ws + "   size := size + UInt32_t(this." + getDeconflictedName(st.fields[i].name) + ".Length)*Int32_t\'Size/8;\n";
                    break;
                case VECTOR_NODE_STRUCT:
                case VECTOR_LEAF_STRUCT:
                    if (st.fields[i].isLargeArray) {
                    str += ws + "   size := size + 4;\n";
                    }
                    else {
                        str += ws + "   size := size + 2;\n";
                    }
                    str += ws + "   for i of this." + getDeconflictedName(st.fields[i].name) + ".all loop\n";
                    str += ws + "      if i = null then\n";
                    str += ws + "         size := size + 1;\n";
                    str += ws + "      else\n";
                    str += ws + "         size := size + 15 + calculatePackedSize(i.all);\n";
                    str += ws + "      end if;\n";
                    str += ws + "   end loop;\n";
                    break;
                case FIXED_ARRAY_PRIMITIVE:
                    str += ws + "   size := size + 2;\n";
                    if(st.fields[i].type.equalsIgnoreCase("string")) {
                        str += ws + "   for i of this." + getDeconflictedName(st.fields[i].name) + ".all loop\n";
                        str += ws + "      size := size + 2 + UInt32_t(Length(i))*Character\'Size/8;\n";
                        str += ws + "   end loop;\n";
                    }
                    else {
                        str += ws + "   size := size + this." + getDeconflictedName(st.fields[i].name) + ".all'Length)*" + getAdaPrimativeType(infos, st.fields[i]) + "'Size/8;\n";
                    }
                    break;
                case FIXED_ARRAY_ENUM:
                    str += ws + "   size := size + 2;\n";
                    str += ws + "   size := size + this." + getDeconflictedName(st.fields[i].name) + ".all'Length)*UInt32_t'Size/8;\n";
                    break;
                case FIXED_ARRAY_NODE_STRUCT:
                case FIXED_ARRAY_LEAF_STRUCT:
                    str += ws + "   size := size + 2;\n";
                    str += ws + "   for i of this." + getDeconflictedName(st.fields[i].name) + ".all loop\n";
                    str += ws + "      if i = null then\n";
                    str += ws + "         size := size + 1;\n";
                    str += ws + "      else\n";
                    str += ws + "         size := size + 15 + calculatePackedSize(i);\n";
                    str += ws + "      end if;\n";
                    str += ws + "   end loop;\n";
                break;
                default:
                    break;
            }
        }
        str += ws + "   return size;\n";
        str += ws + "end calculatePackedSize;";
        return str;
    };
};