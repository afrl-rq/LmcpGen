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
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
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

    /**
     * A partial set of Ada reserved words, to be used in identifier renaming to
     * ensure Ada compatibility.
     */
    public static final Set<String> ADA_RESERVED_WORDS = new HashSet<String>(
            Arrays.asList("loop", "record", "task", "range"));

    /**
     * A {@link Map} from String to String where the key is the all-lowercase name
     * of the identifier and the value is the preferred casing for the identifier.
     *
     * <p>
     * The idea here is to provider a general means to get from the package-name
     * representation of an identifier to the preferred variable-name representation
     * of the identifier. For example, for UxAS, we would like the package name to
     * be <code>uxas</code> but we would like the variable name to be
     * <code>UxAS</code>.
     * </p>
     *
     * <p>
     * This is important for allowing us to gracefully support target-language
     * naming conventions in a uniform and easy-to-maintain way. For now, the map is
     * statically defined here in the class. A possible future enhancement is to
     * make the map data-driven based on some sort of simple configuration file that
     * would be loaded during generation.
     * </p>
     */
    private static final Map<String, String> SPECIAL_CASING;

    /*
     * Static initialization of the specialCasing map. This could be replaced by a
     * method that initializes the map from the contents of a configuration file.
     */
    static {
        SPECIAL_CASING = new HashMap<String, String>();
        SPECIAL_CASING.put("afrl",     "AFRL");
        SPECIAL_CASING.put("cmasi",    "CMASI");
        SPECIAL_CASING.put("vehicles", "Vehicles");
        SPECIAL_CASING.put("uxas",     "UxAS");
        SPECIAL_CASING.put("messages", "Messages");
        SPECIAL_CASING.put("route",    "Route");
        SPECIAL_CASING.put("uxnative", "UxNative");
        SPECIAL_CASING.put("impact",   "Impact");
     }

    /**
     * <p>
     * Given an identifier, return a new identifier that:
     * </p>
     *
     * <ul>
     * <li>has proper casing for an identifier, using the {@link #SPECIAL_CASING} map
     * (e.g., <code>uxas</code> becomes <code>UxAS</code>); and</li>
     * <li>includes the prefix "lmcp" if the identifier is an Ada reserved word,
     * using the {@link #ADA_RESERVED_WORDS} set (e.g., <code>task</code> becomes
     * <code>lmcptask</code>).</li>
     * </ul>
     *
     * <p>
     * The identifer can be provided with any casing: the method will downcase the
     * identifier before testing against the map or set.
     * </p>
     *
     * @param name the identifier to deconflict
     * @return the deconflicted identifier
     */
    private static String getDeconflictedName(String name) {
        final String downcased = name.toLowerCase();

        if (SPECIAL_CASING.containsKey(downcased)) {
           return SPECIAL_CASING.get(downcased);
        } else {         
           return (ADA_RESERVED_WORDS.contains(downcased) ? "lmcp" + name : name );
        }
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
        return ws + getDeconflictedNamespace(info.namespace).toLowerCase().replaceAll("/", "-");
    }

    public static String full_series_name_dots(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + getDeconflictedNamespace(info.namespace).replaceAll("/", ".");
    }

    public static String full_datatype_name_dashes(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return getSeriesNamespaceDashes(infos, st.seriesName) + getDeconflictedName(st.name);
    }

    public static String full_datatype_name_dashes_lowercase(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return getSeriesNamespaceDashes(infos, st.seriesName).toLowerCase() + getDeconflictedName(st.name).toLowerCase();
    }

    //  used ONLY to get the subscription value in the series package spec template, so it MUST NOT provide deconflicted values
    public static String full_datatype_name_dots(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        MDMInfo i = MDMReader.getMDM(st.seriesName, infos);
        return i.namespace.replaceAll("/", ".") + "." + st.name;
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

    public static String access_suffix(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + (has_descendants(infos, st.name, st.seriesName) ? "Any" : "Acc");
    }

    private static String getDeconflictedNamespace (String input) {
        String [] Parts = input.split ("/");
        StringBuffer buffer = new StringBuffer();
        for (int k = 0; k < Parts.length; k++) {
           buffer.append(getDeconflictedName(Parts[k]));

           if (k < Parts.length - 1) {
              buffer.append("/");  // put the delimiter back in; we only change the names
           }
        }
        return buffer.toString();
    }

    private static String getSeriesNamespaceDots(MDMInfo[] infos, String series_name) {
        MDMInfo i = MDMReader.getMDM(series_name, infos);
        if (i != null) {
            return getDeconflictedNamespace(i.namespace).replaceAll("/", ".") + ".";
        }
        return "";
    }

    private static String getSeriesNamespaceDashes(MDMInfo[] infos, String series_name) {
        MDMInfo i = MDMReader.getMDM(series_name, infos);
        if (i != null) {
            return getDeconflictedNamespace(i.namespace).replaceAll("/", "-") + "-";
        }
        return "";
    }

    public static String full_parent_datatype_package(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + (st.extends_name.length() == 0 ? getDeconflictedNamespace (info.namespace).replaceAll("/", ".") + ".Object" : getSeriesNamespaceDots(infos, st.extends_series) + getDeconflictedName(st.extends_name));
    }

    public static String full_parent_datatype(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + (st.extends_name.length() == 0 ? getDeconflictedNamespace (info.namespace).replaceAll("/", ".") + ".Object.Object" : ws + getSeriesNamespaceDots(infos, st.extends_series) + getDeconflictedName(st.extends_name) + "." + getDeconflictedName(st.extends_name));
    }

    public static String getFullParentDatatype(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return (st.extends_name.length() == 0 ? getDeconflictedNamespace (info.namespace).replaceAll("/", ".") + ".Object.Object" : getSeriesNamespaceDots(infos, st.extends_series) + getDeconflictedName(st.extends_name) + "." + getDeconflictedName(st.extends_name));
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

            str += ws + "function toInt32(enum : " + getDeconflictedName(info.enums[i].name) + "Enum) return Int32 is\n";
            str += ws + "   (case enum is " + gen_enum_to_int(info.enums[i]) + ");\n";

            str += ws + "function toEnum(val : Int32) return " + getDeconflictedName(info.enums[i].name) + "Enum is\n";
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
            } else if (st.fields[i].isEnum) {
                String series = getSeriesNamespaceDots(infos, st.fields[i].seriesName);
                str += ws + "with " + series + "enumerations; use " + series + "enumerations;\n";
            }
        }

        // if there are any vectors, include vector package
        for (int i = 0; i < st.fields.length; i++) {
            if (st.fields[i].isArray && st.fields[i].length == -1) {
                str += ws + "with Ada.Containers; use Ada.Containers;\n";
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

    /**
     * Recursively build packages and directories for a namespace.
     *
     * @param infos
     * @param info
     * @param outfile
     * @param st
     * @param en
     * @param ws
     * @return
     * @throws Exception
     */
    public static String descending_namespace_spec(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        // For <namespace_1>/<namespace_2>/ ... <namespace_n>, create files with empty package definitions for
        // <namespace_1>, <namespace_1>.<namespace_2>, through <namespace_1>.<namespace_2> ... .<namespace_n-1>
        // and return basic package definition for <namespace_1>.<namespace_2> ... .<namespace_n>

        final String namespace = getDeconflictedNamespace(info.namespace);
        final String[] namespaceParts = namespace.split("/");
        
        // We treat this a bit like a stack, grabbing the parent at each pass through the loop below.
        File packageFileDir = outfile.getParentFile();

        // Packages before the lowest-level package, starting at n-1
        for(int i = namespaceParts.length - 2; i >= 0 ; i--) {

            final StringBuilder packageNameBuilder = new StringBuilder();
            for(int j = 0; j < i; j++) {
                packageNameBuilder.append(namespaceParts[j]);
                packageNameBuilder.append(".");
            }
            packageNameBuilder.append(namespaceParts[i]);

            final String packageName = packageNameBuilder.toString();
            final String packageFileName = packageName.replaceAll("\\.", "-");
            packageFileDir = packageFileDir.getParentFile();
            packageFileDir.mkdirs();

            final File packageFile = new File(packageFileDir, packageFileName.toLowerCase() + ".ads");

            // We do not want to write the package file if it already exists,
            // because we may have previously written it, with necessary withs,
            // on an earlier mdm. (This is particularly true for CMASI MDMs.)
            if (!packageFile.exists()) {
                packageFile.createNewFile();

                final String packageDeclaration = MessageFormat.format(
                          "package {0} is\n"
                        + "\n"
                        + "end {0};\n", packageName);

                Files.write(packageFile.toPath(), packageDeclaration.getBytes());
            }
        }

        return MessageFormat.format(
                  "with AVTAS.LMCP.Object; use AVTAS.LMCP.Object;\n"
                + "with AVTAS.LMCP.types;  use AVTAS.LMCP.types;\n"
                + "\n"
                + "package {0} is\n"
                + "\n"
                + "end {0};\n", namespace.replaceAll("/", "."));
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
            return "Int16";
        }
        if (type.equalsIgnoreCase("uint16")) {
            return "UInt16";
        }
        if (type.equalsIgnoreCase("int32")) {
            return "Int32";
        }
        if (type.equalsIgnoreCase("uint32")) {
            return "UInt32";
        }
        if (type.equalsIgnoreCase("int64")) {
            return "Int64";
        }
        if (type.equalsIgnoreCase("real32")) {
            return "Real32";
        }
        if (type.equalsIgnoreCase("real64")) {
            return "Real64";
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
                case VECTOR_PRIMITIVE: {
                    String typename = getAdaPrimativeType(infos, st.fields[i]);
                    if(!vectTypes.contains(typename)) {
                        vectTypes.add(typename);
                        str += ws + "package Vect_" + typename + " is new Ada.Containers.Vectors\n";
                        str += ws + "  (Index_Type   => Natural,\n";
                        str += ws + "   Element_Type => " + typename + ");\n";
                        str += ws + "type Vect_" + typename + "_Acc is access all Vect_" + typename + ".Vector;\n";
                    }
                    break;
                }
                case VECTOR_ENUM:
                    if(!vectTypes.contains(getDeconflictedName(st.fields[i].type))) {
                        vectTypes.add(getDeconflictedName(st.fields[i].type));
                        str += ws + "package Vect_" + getDeconflictedName(st.fields[i].type) + "Enum is new Ada.Containers.Vectors\n";
                        str += ws + "  (Index_Type   => Natural,\n";
                        str += ws + "   Element_Type => " + getDeconflictedName(st.fields[i].type) + "Enum);\n";
                        str += ws + "type Vect_" + getDeconflictedName(st.fields[i].type) + "Enum_Acc is access all Vect_" + getDeconflictedName(st.fields[i].type) + "Enum.Vector;\n";
                    }
                    break;
                case VECTOR_NODE_STRUCT:
                    if(!vectTypes.contains(getDeconflictedName(st.fields[i].type))) {
                        vectTypes.add(getDeconflictedName(st.fields[i].type));
                        str += ws + "package Vect_" + getDeconflictedName(st.fields[i].type) + "_Any is new Ada.Containers.Vectors\n";
                        str += ws + "  (Index_Type   => Natural,\n";
                        str += ws + "   Element_Type => " + getDeconflictedName(st.fields[i].type) + "_Any);\n";
                        str += ws + "type Vect_" + getDeconflictedName(st.fields[i].type) + "_Any_Acc is access all Vect_" + getDeconflictedName(st.fields[i].type) + "_Any.Vector;\n";
                    }
                    break;
                case VECTOR_LEAF_STRUCT:
                    if(!vectTypes.contains(getDeconflictedName(st.fields[i].type))) {
                        vectTypes.add(getDeconflictedName(st.fields[i].type));
                        str += ws + "package Vect_" + getDeconflictedName(st.fields[i].type) + "_Acc is new Ada.Containers.Vectors\n";
                        str += ws + "  (Index_Type   => Natural,\n";
                        str += ws + "   Element_Type => " + getDeconflictedName(st.fields[i].type) + "_Acc);\n";
                        str += ws + "type Vect_" + getDeconflictedName(st.fields[i].type) + "_Acc_Acc is access all Vect_" + getDeconflictedName(st.fields[i].type) + "_Acc.Vector;\n";
                    }
                    break;
                case FIXED_ARRAY_PRIMITIVE: {
                    String typename = get_array_type_for_fixed_array_primitive(infos, st.fields[i]);
                    if (!vectTypes.contains(typename)) {
                        vectTypes.add(typename);
                        str += ws + "type " + typename + " is array (1 .. " + st.fields[i].length + ") of " + getAdaPrimativeType(infos, st.fields[i]) + ";\n";
                        str += ws + "type " + typename + "_Acc is access all " + typename + ";\n";
                    }
                    break;
                }
                case FIXED_ARRAY_ENUM:
                case FIXED_ARRAY_NODE_STRUCT:
                case FIXED_ARRAY_LEAF_STRUCT:
                    throw new IllegalArgumentException("ENUM, NODE_STRUCT, and LEAF_STRUCT uses of FIXED_ARRAY are unimplemented");

                    // These will need to be implemented as in FIXED_ARRAY_PRIMITIVE.
                    // break;
                default:
                    break;
            }
        }
        return str;
    }

    private static String get_array_type_for_fixed_array_primitive(MDMInfo[] infos, FieldInfo field) {
        return getAdaPrimativeType(infos, field) + "_" + field.length + "D";
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
                    str += ws + "procedure set" + fieldname + "(this : in out " + thisRecordName + "; " + fieldname + " : in " + getAdaPrimativeType(infos, st.fields[i]) + ");\n";
                    break;
                case SINGLE_ENUM:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return " + type + "Enum;\n";
                    str += ws + "procedure set" + fieldname + "(this : in out " + thisRecordName + "; " + fieldname + " : in " + type + "Enum);\n";
                    break;
                case SINGLE_NODE_STRUCT:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return " + type + "_Any;\n";
                    str += ws + "procedure set" + fieldname + "(this : in out " + thisRecordName + "; " + fieldname + " : in " + type + "_Any);\n";
                    break;
                case SINGLE_LEAF_STRUCT:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return " + type + "_Acc;\n";
                    str += ws + "procedure set" + fieldname + "(this : in out " + thisRecordName + "; " + fieldname + " : in " + type + "_Acc);\n";
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
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return " + get_array_type_for_fixed_array_primitive(infos, st.fields[i]) + "_Acc;\n";
                    break;
                case FIXED_ARRAY_ENUM:
                    throw new IllegalArgumentException("FIXED_ARRAY_ENUM unimplemented in `get_and_set_methods_body`");

                    // This is illegal in Ada. It would need to be handled like the FIXED_ARRAY_PRIMITIVE.
                    // str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + type + "Enum;\n";
                    // break;
                case FIXED_ARRAY_NODE_STRUCT:
                    throw new IllegalArgumentException("FIXED_ARRAY_NODE_STRUCT unimplemented in `get_and_set_methods_body`");

                    // This is illegal in Ada. It would need to be handled like the FIXED_ARRAY_PRIMITIVE.
                    // str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + type + "_Any;\n";
                    // break;
                case FIXED_ARRAY_LEAF_STRUCT:
                    throw new IllegalArgumentException("FIXED_ARRAY_LEAF_STRUCT unimplemented in `get_and_set_methods_body`");

                    // This is illegal in Ada. It would need to be handled like the FIXED_ARRAY_PRIMITIVE.
                    // str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + type + "_Acc;\n";
                    // break;
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
                    str += ws + "procedure set" + fieldname + "(this : in out " + thisRecordName + "; " + fieldname + " : in " + getAdaPrimativeType(infos, st.fields[i]) + ") is\n" + ws + "begin\n";
                    str += ws + "   this." + fieldname + " := " + fieldname + ";\n";
                    str += ws + "end set" + fieldname + ";\n\n";
                    break;
                case SINGLE_ENUM:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return " + type + "Enum is (this." + fieldname + ");\n";
                    str += ws + "procedure set" + fieldname + "(this : in out " + thisRecordName + "; " + fieldname + " : in " + type + "Enum) is\n" + ws + "begin\n";
                    str += ws + "   this." + fieldname + " := " + fieldname + ";\n";
                    str += ws + "end set" + fieldname + ";\n\n";
                    break;
                case SINGLE_NODE_STRUCT:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return " + type + "_Any is (this." + fieldname + ");\n";
                    str += ws + "procedure set" + fieldname + "(this : in out " + thisRecordName + "; " + fieldname + " : in " + type + "_Any) is\n" + ws + "begin\n";
                    str += ws + "   this." + fieldname + " := " + fieldname + ";\n";
                    str += ws + "end set" + fieldname + ";\n\n";
                    break;
                case SINGLE_LEAF_STRUCT:
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return " + type + "_Acc is (this." + fieldname + ");\n";
                    str += ws + "procedure set" + fieldname + "(this : in out " + thisRecordName + "; " + fieldname + " : in " + type + "_Acc) is\n" + ws + "begin\n";
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
                    str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return " + get_array_type_for_fixed_array_primitive(infos, st.fields[i]) + "_Acc is (this." + fieldname + ");\n";
                    break;
                case FIXED_ARRAY_ENUM:
                    throw new IllegalArgumentException("FIXED_ARRAY_ENUM unimplemented in `get_and_set_methods_body`");

                    // This is illegal in Ada. It would need to be handled like the FIXED_ARRAY_PRIMITIVE.
                    // str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + type + "Enum is (this." + fieldname + ");\n";
                    // break;
                case FIXED_ARRAY_NODE_STRUCT:
                    throw new IllegalArgumentException("FIXED_ARRAY_NODE_STRUCT unimplemented in `get_and_set_methods_body`");

                    // This is illegal in Ada. It would need to be handled like the FIXED_ARRAY_PRIMITIVE.
                    // str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + type + "_Any is (this." + fieldname + ");\n";
                    // break;
                case FIXED_ARRAY_LEAF_STRUCT:
                    throw new IllegalArgumentException("FIXED_ARRAY_LEAF_STRUCT unimplemented in `get_and_set_methods_body`");

                    // This is illegal in Ada. It would need to be handled like the FIXED_ARRAY_PRIMITIVE.
                    // str += ws + "function get" + fieldname + "(this : " + thisRecordName + ") return access all array (Integer range 1 .. " + st.fields[i].length + ") of " + type + "_Acc is (this." + fieldname + ");\n";
                    // break;
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
                    str += ws + fieldname + " : " + get_array_type_for_fixed_array_primitive(infos, st.fields[i]) + "_Acc := new " + get_array_type_for_fixed_array_primitive(infos, st.fields[i]) + ";\n";
                    break;
                case FIXED_ARRAY_ENUM:
                    throw new IllegalArgumentException("FIXED_ARRAY_ENUM unimplemented in `record_fields`");

                    // This is illegal in Ada. It would need to be handled like the FIXED_ARRAY_PRIMITIVE.
                    // str += ws + fieldname + " : access all array (Integer range 1 .. " + st.fields[i].length + ") of " + getResolvedTypeName(infos, st.fields[i]) + "Enum := (others => " + getAdaDefaultVal(infos, st.fields[i]) + ");\n";
                    // break;
                case FIXED_ARRAY_NODE_STRUCT:
                    throw new IllegalArgumentException("FIXED_ARRAY_NODE_STRUCT unimplemented in `record_fields`");

                    // This is illegal in Ada. It would need to be handled like the FIXED_ARRAY_PRIMITIVE.
                    // str += ws + fieldname + " : access all array (Integer range 1 .. " + st.fields[i].length + ") of " + getResolvedTypeName(infos, st.fields[i]) + "_Any := (others => " + getAdaDefaultVal(infos, st.fields[i]) + ");\n";
                    // break;
                case FIXED_ARRAY_LEAF_STRUCT:
                    throw new IllegalArgumentException("FIXED_ARRAY_LEAF_STRUCT unimplemented in `record_fields`");

                    // This is illegal in Ada. It would need to be handled like the FIXED_ARRAY_PRIMITIVE.
                    // str += ws + fieldname + " : access all array (Integer range 1 .. " + st.fields[i].length + ") of " + getResolvedTypeName(infos, st.fields[i]) + "_Acc := (others => " + getAdaDefaultVal(infos, st.fields[i]) + ");\n";
                    // break;
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

    public static String all_descendants(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String ret = "";
        List<String> descendants = new ArrayList<String>();
        add_descendants(infos, st.name, st.seriesName, descendants);
        for(String child : descendants) {
            ret += ws + "Descendants.Append (\"" + child + "\");\n";
        }
        return ret;
    }
  
    public static String global_factory_switch(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        buf.append(ws + "case seriesId is\n");
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            buf.append(ws + "   when " + i.seriesNameAsLong + " => return " + getDeconflictedNamespace(i.namespace).replaceAll("/", ".") + ".Factory.createObject(seriesId, msgType, version);\n");
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
            buf.append(ws + "      when " + info.structs[j].id + " => return new " + getDeconflictedNamespace(info.namespace).replaceAll("/", ".") + "."
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
            buf.append(ws + "with " + getDeconflictedNamespace(i.namespace).replaceAll("/", ".") + "." + "Factory;\n");
        }
        return buf.toString();
    }

    public static String include_all_series_headers(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        str += ws + "with " + getDeconflictedNamespace (info.namespace).replaceAll("/", ".") + ".Enumerations;\n";
        str += ws + "with " + getDeconflictedNamespace (info.namespace).replaceAll("/", ".") + ".Object;\n";
        for (int i = 0; i < info.structs.length; i++) {
            str += ws + "with " + getDeconflictedNamespace (info.namespace).replaceAll("/", ".") + "." + getDeconflictedName(info.structs[i].name) + ";\n";
        }
        return str;
    }

    public static String pack_body(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        String parentDatatype = getFullParentDatatype(infos, info, outfile, st, en, ws);
        str += ws + "overriding\n";
        str += ws + "procedure Pack (This : " + getDeconflictedName(st.name) + "; Buffer : in out ByteBuffer) is\n";
        str += ws + "begin\n";
        str += ws + "   Pack (" + parentDatatype +"(This), Buffer);   --  call parent version statically\n";
        for (int i = 0; i < st.fields.length; i++) {
            String fieldname = getDeconflictedName(st.fields[i].name);
            switch (getAdaTypeCategory(infos,st.fields[i])) {
                case SINGLE_PRIMITIVE:
                        str += ws + "   Buffer.Put_" + getAdaPrimativeType(infos, st.fields[i]) + "(This." + fieldname + ");\n";
                    break;
                case SINGLE_ENUM:
                    str += ws + "   Buffer.Put_Int32(toInt32(This." + fieldname + "));\n";
                    break;
                case SINGLE_NODE_STRUCT:
                case SINGLE_LEAF_STRUCT:
                    str += ws + "   AVTAS.LMCP.Factory.putObject(AVTAS.LMCP.Object.Object_Any(This." + fieldname + "), Buffer);\n";
                    break;
                case VECTOR_PRIMITIVE:
                    if (st.fields[i].isLargeArray) {
                        str += ws + "   Buffer.Put_UInt32(UInt32(This." + fieldname + ".Length));\n";
                    }
                    else {
                        str += ws + "   Buffer.Put_UInt16(UInt16(This." + fieldname + ".Length));\n";
                    }
                    str += ws + "   for i of This." + fieldname + ".all loop\n";
                    str += ws + "      Buffer.Put_" + getAdaPrimativeType(infos, st.fields[i]) + "(i);\n";
                    str += ws + "   end loop;\n";
                    break;
                case VECTOR_ENUM:
                    if (st.fields[i].isLargeArray) {
                        str += ws + "   Buffer.Put_UInt32(UInt32(This." + fieldname + ".Length));\n";
                    }
                    else {
                        str += ws + "   Buffer.Put_UInt16(UInt16(This." + fieldname + ".Length));\n";
                    }
                    str += ws + "   for i of This." + fieldname + ".all loop\n";
                    str += ws + "      Buffer.Put_Int32(toInt32(i));\n";
                    str += ws + "   end loop;\n";
                    break;
                case VECTOR_NODE_STRUCT:
                case VECTOR_LEAF_STRUCT:
                    if (st.fields[i].isLargeArray) {
                        str += ws + "   Buffer.Put_UInt32(UInt32(This." + fieldname + ".Length));\n";
                    }
                    else {
                        str += ws + "   Buffer.Put_UInt16(UInt16(This." + fieldname + ".Length));\n";
                    }
                    str += ws + "   for i of This." + fieldname + ".all loop\n";
                    str += ws + "      AVTAS.LMCP.Factory.putObject(AVTAS.LMCP.Object.Object_Any(i), Buffer);\n";
                    str += ws + "   end loop;\n";
                    break;
                case FIXED_ARRAY_PRIMITIVE:
                    //str += ws + "   Put_UInt32(UInt32(This." + fieldname + "'Length), Buffer);\n";
                    str += ws + "   for i of This." + fieldname + ".all loop\n";
                    str += ws + "      Buffer.Put_" + getAdaPrimativeType(infos, st.fields[i]) + "(i);\n";
                    str += ws + "   end loop;\n";
                    break;
                case FIXED_ARRAY_ENUM:
                    //str += ws + "   Put_UInt32(UInt32(This." + fieldname + "'Length), Buffer);\n";
                    str += ws + "   for i of This." + fieldname + ".all loop\n";
                    str += ws + "      Buffer.Put_Int32(toInt32(i));\n";
                    str += ws + "   end loop;\n";
                    break;
                case FIXED_ARRAY_NODE_STRUCT:
                case FIXED_ARRAY_LEAF_STRUCT:
                    // str += ws + "   Put_UInt32(UInt32(This." + fieldname + "'Length), Buffer);\n";
                    str += ws + "   for i of This." + fieldname + ".all loop\n";
                    str += ws + "      AVTAS.LMCP.Factory.putObject(AVTAS.LMCP.Object.Object_Any(i), Buffer);\n";
                    str += ws + "   end loop;\n";
                    break;
                default:
                    break;
            }

        }
        str += ws + "end Pack;";
        return str;
    };

    public static String unpack_body(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        String parentDatatype = getFullParentDatatype(infos, info, outfile, st, en, ws);
        str += ws + "overriding\n";
        str += ws + "procedure Unpack (This : out " + getDeconflictedName(st.name) + "; Buffer : in out ByteBuffer) is\n";
        str += ws + "begin\n";
        str += ws + "   Unpack ("+ parentDatatype +" (This), Buffer);  --  call parent version statically\n";
        for (int i = 0; i < st.fields.length; i++) {
            String fieldname = getDeconflictedName(st.fields[i].name);
            switch (getAdaTypeCategory(infos,st.fields[i])) {
                case SINGLE_PRIMITIVE:
                        str += ws + "   Buffer.Get_" + getAdaPrimativeType(infos, st.fields[i]) + "(This." + fieldname + ");\n";
                    break;
                case SINGLE_ENUM:
                    str += ws + "   declare\n";
                    str += ws + "      i32 : Int32;\n";
                    str += ws + "   begin\n";
                    str += ws + "      Buffer.Get_Int32(i32);\n";
                    str += ws + "      This." + fieldname + " := toEnum(i32);\n";
                    str += ws + "   end;\n";
                    break;
                case SINGLE_NODE_STRUCT:
                case SINGLE_LEAF_STRUCT:
                    String accessSuffix = (has_descendants(infos, st.fields[i].type, st.fields[i].seriesName) ? "_Any" : "_Acc");
                    String component_name = "This." + st.fields[i].name;
                    String fieldtype = getSeriesNamespaceDots(infos, st.fields[i].seriesName) + getDeconflictedName(st.fields[i].type) + "." + getDeconflictedName(st.fields[i].type) + accessSuffix;
                    str += ws + "   declare\n";
                    str += ws + "      fieldExists : Boolean;\n";
                    str += ws + "      seriesId : Int64;\n";
                    str += ws + "      msgType : UInt32;\n";
                    str += ws + "      version : UInt16;\n";
                    str += ws + "   begin\n";
                    str += ws + "      Buffer.Get_Boolean(fieldExists);\n";
                    str += ws + "      if fieldExists then\n";
                    str += ws + "         Buffer.Get_Int64(seriesId);\n";
                    str += ws + "         Buffer.Get_UInt32(msgType);\n";
                    str += ws + "         Buffer.Get_UInt16(version);\n";
                    str += ws + "         " + component_name + " := " + fieldtype + "(AVTAS.LMCP.Factory.createObject(seriesId, msgType, version));\n";
                    str += ws + "         " + component_name + ".Unpack (Buffer);\n";
                    str += ws + "      end if;\n";
                    str += ws + "   end;\n";
                    break;
                case VECTOR_PRIMITIVE:
                    str += ws + "   declare\n";
                    str += ws + "      item : " + getAdaPrimativeType(infos, st.fields[i]) + ";\n";
                    if (st.fields[i].isLargeArray) {
                        str += ws + "      length : UInt32;\n";
                        str += ws + "   begin\n";
                        str += ws + "      Buffer.Get_UInt32(length);\n";
                    }
                    else {
                        str += ws + "      length : UInt16;\n";
                        str += ws + "   begin\n";
                        str += ws + "      Buffer.Get_UInt16(length);\n";
                    }
                    str += ws + "      This.get" + fieldname + ".Clear;  -- delete old content\n";
                    str += ws + "      for i in 1 .. length loop\n";
                    str += ws + "         Buffer.Get_" + getAdaPrimativeType(infos, st.fields[i]) + "(item);\n";
                    str += ws + "         This.get" + fieldname + ".Append(item);\n";
                    str += ws + "      end loop;\n";
                    str += ws + "   end;\n";
                    break;
                case VECTOR_ENUM:
                    str += ws + "   declare\n";
                    str += ws + "      item : Int32;\n";
                    if (st.fields[i].isLargeArray) {
                        str += ws + "      length : UInt32;\n";
                        str += ws + "   begin\n";
                        str += ws + "      Buffer.Get_UInt32(length);\n";
                    }
                    else {
                        str += ws + "      length : UInt16;\n";
                        str += ws + "   begin\n";
                        str += ws + "      Buffer.Get_UInt16(length);\n";
                    }
                    str += ws + "      This.get" + fieldname + ".Clear;  -- delete old content\n";
                    str += ws + "      for i in 1 .. length loop\n";
                    str += ws + "         Buffer.Get_Int32(item);\n";
                    str += ws + "         This.get" + fieldname + ".Append(ToEnum(item));\n";
                    str += ws + "      end loop;\n";
                    str += ws + "   end;\n";
                    break;
                case VECTOR_NODE_STRUCT:
                case VECTOR_LEAF_STRUCT:
                    accessSuffix = (has_descendants(infos, st.fields[i].type, st.fields[i].seriesName) ? "_Any" : "_Acc");
                    String fieldType = getSeriesNamespaceDots(infos, st.fields[i].seriesName) + getDeconflictedName(st.fields[i].type) + "." + getDeconflictedName(st.fields[i].type) + accessSuffix;
                    String lengthType = (st.fields[i].isLargeArray ? "UInt32" : "UInt16");
                    str += ws + "   declare\n";
                    str += ws + "      fieldExists : Boolean;\n";
                    str += ws + "      seriesId : Int64;\n";
                    str += ws + "      msgType : UInt32;\n";
                    str += ws + "      version : UInt16;\n";
                    str += ws + "      item : " + fieldType + ";\n";
                    str += ws + "      length : " + lengthType + ";\n";
                    str += ws + "   begin\n";
                    str += ws + "      Buffer.Get_" + lengthType + "(length);\n";
                    str += ws + "      This.get" + fieldname + ".Clear;  -- delete old content\n";
                    str += ws + "      for i in 1 .. length loop\n";
                    str += ws + "         Buffer.Get_Boolean(fieldExists);\n";
                    str += ws + "         if fieldExists then\n";
                    str += ws + "            Buffer.Get_Int64(seriesId);\n";
                    str += ws + "            Buffer.Get_UInt32(msgType);\n";
                    str += ws + "            Buffer.Get_UInt16(version);\n";
                    str += ws + "            item := " + fieldType + "(AVTAS.LMCP.Factory.createObject(seriesId, msgType, version));\n";
                    str += ws + "            item.unpack(Buffer);\n";
                    str += ws + "         end if;\n";
                    str += ws + "         This.get" + fieldname + ".Append(item);\n";
                    str += ws + "      end loop;\n";
                    str += ws + "   end;\n";
                    break;
                case FIXED_ARRAY_PRIMITIVE:
                    str += ws + "   declare\n";
                    str += ws + "      item : " + getAdaPrimativeType(infos, st.fields[i]) + ";\n";
                    str += ws + "   begin\n";
                    str += ws + "      for i in This.get" + fieldname + "\'Range loop\n";
                    str += ws + "         Buffer.Get_" + getAdaPrimativeType(infos, st.fields[i]) + "(item);\n";
                    str += ws + "         This.get" + fieldname + "(i) := item;\n";
                    str += ws + "      end loop;\n";
                    str += ws + "   end;\n";
                    break;
                case FIXED_ARRAY_ENUM:
                    str += ws + "   declare\n";
                    str += ws + "      item : Int32;\n";
                    str += ws + "   begin\n";
                    str += ws + "      for i in This.get" + fieldname + "\'Range loop\n";
                    str += ws + "         Buffer.Get_Int32(item);\n";
                    str += ws + "         This.get" + fieldname + "(i) := toEnum(item);\n";
                    str += ws + "      end loop;\n";
                    str += ws + "   end;\n";
                    break;
                case FIXED_ARRAY_NODE_STRUCT:
                case FIXED_ARRAY_LEAF_STRUCT:
                    //accessSuffix = (has_descendants(infos, st.fields[i].type, st.fields[i].seriesName) ? "_Any" : "_Acc");
                    accessSuffix = "_Any";
                    fieldType = getSeriesNamespaceDots(infos, st.fields[i].seriesName) + getDeconflictedName(st.fields[i].type) + "." + getDeconflictedName(st.fields[i].type) + accessSuffix;
                    str += ws + "   declare\n";
                    str += ws + "      fieldExists : Boolean;\n";
                    str += ws + "      seriesId : Int64;\n";
                    str += ws + "      msgType : UInt32;\n";
                    str += ws + "      version : UInt16;\n";
                    str += ws + "      item : " + fieldType + ";\n";
                    str += ws + "   begin\n";
                    str += ws + "      for i in This.get" + fieldname + "\'Range loop\n";
                    str += ws + "         Buffer.Get_Boolean(fieldExists);\n";
                    str += ws + "         if fieldExists then\n";
                    str += ws + "            Buffer.Get_Int64(seriesId);\n";
                    str += ws + "            Buffer.Get_UInt32(msgType);\n";
                    str += ws + "            Buffer.Get_UInt16(version);\n";
                    str += ws + "            item := " + fieldType + "(AVTAS.LMCP.Factory.createObject(seriesId, msgType, version));\n";
                    str += ws + "            item.unpack(Buffer);\n";
                    str += ws + "         else\n";
                    str += ws + "            item := null;\n";
                    str += ws + "         end if;\n";
                    str += ws + "         This.get" + fieldname + "(i) := item;\n";
                    str += ws + "      end loop;\n";
                    str += ws + "   end;\n";
                    break;
                default:
                    break;
            }

        }
        str += ws + "end Unpack;";
        return str;
    };

    public static String calculate_packed_size_body(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String parentDataType = getFullParentDatatype(infos, info, outfile, st, en, ws);
        String str = "";
        str += ws + "overriding\n";
        str += ws + "function calculatePackedSize(this : " + getDeconflictedName(st.name) + ") return UInt32 is\n";
        str += ws + "  size : UInt32 := 0;\n";
        str += ws + "begin\n";

        if (parentDataType != null) {
           str += ws + "   -- call parent version statically\n";
           str += ws + "   Size := Size + calculatePackedSize (" + parentDataType + " (this));\n";
        }

        for (int i = 0; i < st.fields.length; i++) {
             switch (getAdaTypeCategory(infos,st.fields[i])) {
                case SINGLE_PRIMITIVE:
                    if(st.fields[i].type.equalsIgnoreCase("string")) {
                        str += ws + "   size := size + 2 + UInt32(Length(this." + getDeconflictedName(st.fields[i].name) + "))*Character\'Object_Size/8;\n";
                    }
                    else {
                        str += ws + "   size := size + " + getAdaPrimativeType(infos, st.fields[i]) + "\'Object_Size/8;\n";
                    }
                    break;
                case SINGLE_ENUM:
                    str += ws + "   size := size + Int32\'Object_Size/8;\n";
                    break;
                case SINGLE_NODE_STRUCT:
                case SINGLE_LEAF_STRUCT:
                    str += ws + "   if this." + getDeconflictedName(st.fields[i].name) + " = null then\n";
                    str += ws + "      size := size + 1;\n";
                    str += ws + "   else\n";
                    str += ws + "      size := size + 15 + calculatePackedSize(this." + getDeconflictedName(st.fields[i].name) + ".all);\n";
                    str += ws + "   end if;\n";
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
                        str += ws + "      size := size + 2 + UInt32(Length(i))*Character\'Object_Size/8;\n";
                        str += ws + "   end loop;\n";
                    }
                    else {
                        str += ws + "   size := size + UInt32(this." + getDeconflictedName(st.fields[i].name) + ".Length)*" + getAdaPrimativeType(infos, st.fields[i]) + "\'Object_Size/8;\n";
                    }
                    break;
                case VECTOR_ENUM:
                    if (st.fields[i].isLargeArray) {
                        str += ws + "   size := size + 4;\n";
                    }
                    else {
                        str += ws + "   size := size + 2;\n";
                    }
                    str += ws + "   size := size + UInt32(this." + getDeconflictedName(st.fields[i].name) + ".Length)*Int32\'Object_Size/8;\n";
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
                        str += ws + "      size := size + 2 + UInt32(Length(i))*Character\'Object_Size/8;\n";
                        str += ws + "   end loop;\n";
                    }
                    else {
                        str += ws + "   size := size + UInt32(this." + getDeconflictedName(st.fields[i].name) + ".all'Length)*" + getAdaPrimativeType(infos, st.fields[i]) + "'Object_Size/8;\n";
                    }
                    break;
                case FIXED_ARRAY_ENUM:
                    str += ws + "   size := size + 2;\n";
                    str += ws + "   size := size + UInt32(this." + getDeconflictedName(st.fields[i].name) + ".all'Length)*UInt32'Object_Size/8;\n";
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

    public static String xml_write_body(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = ws + "overriding\n";
        str += ws + "procedure XML_Write (this  : " + getDeconflictedName(st.name) + ";\n";
        str += ws + "                     S     : access Ada.Streams.Root_Stream_Type'Class;\n";
        str += ws + "                     Level : Natural) is\n";
        str += ws + "begin\n";
        str += ws + "   XML_Write (" + getFullParentDatatype(infos, info, outfile, st, en, ws) + "(this), S, Level);  --  call parent version statically\n";

        // TODO: mixed-case To_String subprogram for verbatim printing of enum values?
        //       - LMCP standard doesn't explicitly mention case-sensitivity
        //       - Ada is case insensitive so 'Image can only be used to produce all lower/upper case
        // TODO: consider shorter tags (or absent) when payload is empty or default value
        for (int i = 0; i < st.fields.length; i++) {
            switch (getAdaTypeCategory(infos, st.fields[i])) {
                case SINGLE_PRIMITIVE:
                    str += ws + "   String'Write (S, LeftPad (\"<" + st.fields[i].name + ">\" & ";

                    if (st.fields[i].type.equalsIgnoreCase("string")) {
                        str += "To_String (";
                    }
                    else {
                        str += getAdaPrimativeType(infos, st.fields[i]) + "'Image (";
                    }

                    str += "this." + getDeconflictedName(st.fields[i].name) + ") & \"</" + st.fields[i].name
                            + ">\" & ASCII.LF, Level));\n";
                    break;
                case SINGLE_ENUM:
                    str += ws + "   String'Write (S, LeftPad (\"<" + st.fields[i].name + ">\" & this."
                            + st.fields[i].name + "'Image & \"</" + st.fields[i].name + ">\" & ASCII.LF, Level));\n";
                    break;
                case SINGLE_NODE_STRUCT:
                case SINGLE_LEAF_STRUCT:
                    str += "\n";
                    str += ws + "   String'Write (S, LeftPad (\"<" + st.fields[i].name + ">\" & ASCII.LF, Level));\n";
                    str += ws + "   XML_Output (this." + getDeconflictedName(st.fields[i].name) + ".all, S, Level + 1);\n";
                    str += ws + "   String'Write (S, LeftPad (\"</" + st.fields[i].name + ">\" & ASCII.LF, Level));\n\n";
                    break;
                case VECTOR_PRIMITIVE:
                case FIXED_ARRAY_PRIMITIVE:
                    str += "\n";
                    str += ws + "   String'Write (S, LeftPad (\"<" + st.fields[i].name + ">\" & ASCII.LF, Level));\n";
                    str += ws + "   for element of this." + getDeconflictedName(st.fields[i].name) + ".all loop\n";
                    str += ws + "      String'Write (S, LeftPad (\"<" + st.fields[i].type + ">\" & "
                            + getAdaPrimativeType(infos, st.fields[i]) + "'Image (element) & \"</" + st.fields[i].type
                            + ">\" & ASCII.LF, Level + 1));\n";
                    str += ws + "   end loop;\n";
                    str += ws + "   String'Write (S, LeftPad (\"</" + st.fields[i].name + ">\" & ASCII.LF, Level));\n\n";
                    break;
                case VECTOR_ENUM:
                case FIXED_ARRAY_ENUM:
                    str += "\n";
                    str += ws + "   String'Write (S, LeftPad (\"<" + st.fields[i].name + ">\" & ASCII.LF, Level));\n";
                    str += ws + "   for element of this." + getDeconflictedName(st.fields[i].name) + ".all loop\n";
                    str += ws + "      String'Write (S, LeftPad (\"<" + st.fields[i].type + ">\" & element'Image & \"</"
                            + st.fields[i].type + ">\" & ASCII.LF, Level + 1));\n";
                    str += ws + "   end loop;\n";
                    str += ws + "   String'Write (S, LeftPad (\"</" + st.fields[i].name + ">\" & ASCII.LF, Level));\n\n";
                    break;
                case VECTOR_NODE_STRUCT:
                case VECTOR_LEAF_STRUCT:
                case FIXED_ARRAY_NODE_STRUCT:
                case FIXED_ARRAY_LEAF_STRUCT:
                    str += "\n";
                    str += ws + "   String'Write (S, LeftPad (\"<" + st.fields[i].name + ">\" & ASCII.LF, Level));\n";
                    str += ws + "   for element of this." + getDeconflictedName(st.fields[i].name) + ".all loop\n";
                    str += ws + "      XML_Output (element.all, S, Level + 1);\n";
                    str += ws + "   end loop;\n";
                    str += ws + "   String'Write (S, LeftPad (\"</" + st.fields[i].name + ">\" & ASCII.LF, Level));\n\n";
                    break;
                default:
                    str += "pragma Compile_Time_Warning (Standard.True, \"XML write unimplemented for type\");\n";
                    break;
            }
        }

        str += ws + "end XML_Write;\n";
        return str;
    }
};

