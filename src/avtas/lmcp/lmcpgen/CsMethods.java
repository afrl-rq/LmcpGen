// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

package avtas.lmcp.lmcpgen;

import avtas.lmcp.lmcpgen.EnumInfo.EnumEntry;
import java.io.File;
import java.rmi.server.UID;
import java.text.DateFormat;
import java.util.Set;
import java.util.HashSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.UUID;

class CsMethods {

    public static String block_comment = "\n*";
    private static String libraryGuid = null;
    private static String coreLibraryGuid = null;
    private static String testServerGuid = null;
    private static String testClientGuid = null;
    private static String packageName = null;

    public static String series_namespace(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String[] splits = info.namespace.split("/");
        String ret = "";
        for (String tmp : splits) {
            if (tmp.length() > 0) {
                ret = ret + toPascalCase(tmp) + ".";
            }
        }
        return ws + "namespace " + ret.substring(0, ret.length() - 1);
    }

    public static String direct_series_namespace(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String[] splits = info.namespace.split("/");
        String ret = "";
        for (String tmp : splits) {
            if (tmp.length() > 0) {
                ret = ret + toPascalCase(tmp) + ".";
            }
        }
        return ws + ret.substring(0, ret.length() - 1);
    }

    public static String series_version(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + String.valueOf(info.version);
    }

    public static String series_id(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + String.valueOf(info.seriesNameAsLong);
    }

    public static String namespace(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String[] splits = info.namespace.split("/");
        String ret = "";
        for (String tmp : splits) {
            if (tmp.length() > 0) {
                ret = ret + toPascalCase(tmp) + "/";
            }
        }
        return ws + ret.substring(0, ret.length() - 1);
    }

    public static String dot_namespace(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String[] splits = info.namespace.split("/");
        String ret = "";
        for (String tmp : splits) {
            if (tmp.length() > 0) {
                ret = ret + toPascalCase(tmp) + ".";
            }
        }
        return ws + ret.substring(0, ret.length() - 1);
    }

    public static String series_root_namespace(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String[] splits = info.namespace.split("/");
        String ret = "";
        for (String tmp : splits) {
            if (tmp.length() > 0) {
                ret = toPascalCase(tmp);
                break;
            }
        }
        return ws + ret;
    }

    public static String import_series(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        String[] splits = info.namespace.split("/");
        String ret = "";
        for (String tmp : splits) {
            if (tmp.length() > 0) {
                ret = ret + toPascalCase(tmp) + ".";
            }
        }
        buf.append(ws + "using " + ret.substring(0, ret.length() - 1) + ";\n");
        return buf.toString();
    }
    
    public static String import_all_series(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            if (info == null || !info.seriesName.equals(i.seriesName)) {
                String[] splits = i.namespace.split("/");
                String ret = "";
                for (String tmp : splits) {
                    if (tmp.length() > 0) {
                        ret = ret + toPascalCase(tmp) + ".";
                    }
                }
                buf.append(ws + "using " + ret.substring(0, ret.length() - 1) + ";\n");
            }
        }
        return buf.toString();
    }

    public static String datatype_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + toPascalCase(st.name);
    }

    public static String enum_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + toPascalCase(en.name);
    }

    public static String enum_comment(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + cleanComment(en.comment);
    }

    // Returns the date that the package was made
    public static String creation_date(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + DateFormat.getDateInstance(DateFormat.FULL).format(new Date());
    }

    public static String enum_list_types(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String enStr = "";
        for (int i = 0; i < info.structs.length; i++) {
            st = info.structs[i];
            String name = toPascalCase(st.name);
            enStr += ws + "/// <Summary>\n";
            enStr += ws + "/// " + name + " (" + st.id + ")\n";
            enStr += ws + "/// </Summary>\n";
            enStr += ws + name + " = " + st.id + ",\n";
        }
        return enStr;
    }

    public static String factory_list_series_ids(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            buf.append(i.seriesNameAsLong + ", ");
        }
        return buf.toString();
    }

    public static String factory_auto_register(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo in : infos) {            
            if(in.seriesNameAsLong == 0)
            {
                continue;
            }
            String[] splits = in.namespace.split("/");
            String ret = "";
            for (String tmp : splits) {
                if (tmp.length() > 0) {
                    tmp = tmp.substring(0, 1).toUpperCase() + tmp.substring(1);
                    ret = ret + tmp + ".";
                }
            }
            String seriesListClass = ret + "SeriesList";
            buf.append(ws + "RegisterSeries( new " + seriesListClass + "() );\n");
        }
        return buf.toString();
    }

    public static String server_register_all_series(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo in : infos) {            
            if(in.seriesNameAsLong == 0)
            {
                continue;
            }
            String[] splits = in.namespace.split("/");
            String ret = "";
            for (String tmp : splits) {
                if (tmp.length() > 0) {
                    tmp = tmp.substring(0, 1).toUpperCase() + tmp.substring(1);
                    ret = ret + tmp + ".";
                }
            }
            String seriesListClass = ret + "SeriesList";
            buf.append(ws + "LmcpFactory.RegisterSeries(new " + seriesListClass + "());\n");
        }
        return buf.toString();
    }

    public static String series_factory_object_type(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        
        String[] splits = info.namespace.split("/");
        String ret = "";
        for (String tmp : splits) {
            if (tmp.length() > 0) {
                tmp = tmp.substring(0, 1).toUpperCase() + tmp.substring(1);
                ret = ret + tmp + ".";
            }
        }
        String seriesListClass = ret + "SeriesList";
        buf.append(ws + "if ( series_id == " + seriesListClass + ".SERIES_ID )\n");
        buf.append(ws + "   if ( series_version == " + seriesListClass + ".SERIES_VERSION )\n");
        buf.append(ws + "      return " + seriesListClass + ".GetInstance(object_type);\n");
        buf.append(ws + "   else throw new InvalidOperationException(\"" + ret + "SeriesFactory Exception.  Bad Version Number.\");\n");

        return buf.toString();
    }

    public static String factory_get_type_names(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {

        String str = "";
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            for (StructInfo d : i.structs) {
                str += ws + "\"" + d.name + "\",";
            }
        }
        return str;
    }

    public static String gen_enum_fields(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        int len = en.entries.size();
        for (int i = 0; i < len; i++) {
            EnumEntry entry = en.entries.get(i);
            buf.append(ws + "/// <Summary>" + entry.comment + "</Summary>\n");
            buf.append(ws + entry.name);
            buf.append(" = " + entry.value);
            buf.append(",\n");
        }
        return buf.toString();
    }

    public static String enum_name_for_type(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String ret = "";
        for (int i = 0; i < info.structs.length; i++) {
            ret += ws + "case " + info.structs[i].id + ": return \"" + info.structs[i].name + "\";\n";
        }
        return ret;
    }

    public static String enum_type_for_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String ret = "";
        for (int i = 0; i < info.structs.length; i++) {
            ret += ws + "if ( name.Equals(\"" + info.structs[i].name + "\") ) return " + info.structs[i].id + ";\n";
        }
        return ret;
    }

    public static String list_instance_for_type(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < info.structs.length; i++) {
            buf.append(ws + "case " + info.structs[i].id + ": return new " + info.structs[i].name + "();\n");
        }
        return buf.toString();
    }

    public static String list_name_for_type(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < info.structs.length; i++) {
            buf.append(ws + "case " + info.structs[i].id + ": return \"" + info.structs[i].name + "\";\n");
        }
        return buf.toString();
    }

    public static String list_type_for_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String ret = "";
        for (StructInfo s : info.structs) {
            ret += ws + "if ( name == \"" + s.name + "\" )  return " + s.id + ";\n";
        }
        return ret;
    }

    public static String series_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + info.seriesName;
    }
    
    public static String cs_series_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = info.seriesName.substring(0, 1).toUpperCase() + info.seriesName.substring(1).toLowerCase();
        return ws + str;
    }
    
    public static String series_name_setup(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + "\"" + info.seriesName + "\"";
    }

    public static String object_comment(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return cleanComment(st.comment);
    }

    public static String member_gets_and_sets(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        FieldInfo[] list = st.fields;
        for (int i = 0; i < list.length; i++) {
            String type = getCsType(infos, list[i]);
            String comment = list[i].comment.replaceAll("\\s+", " ");
            comment = comment.replaceAll("<br", "\n" + ws + "/// <br") + "(Units: " + list[i].units + ")";
            String name = "_" + toCamelCase(list[i].name);
            String name2 = toPascalCase(list[i].name);
            if (name2.equals(toPascalCase(st.name)))
                name2 += "Value";
            str += ws + "/// <summary>\n" + ws + "/// " + comment + "\n";
            if (list[i].isArray) {
                str += ws + "/// [Get Only]\n";
            } else {
                str += ws + "/// [Get and Set]\n";
            }
            str += ws + "/// </summary>\n";
            // scalar types
            if (!list[i].isArray) {
                str += ws + "public " + type + " " + name2 + "\n" + ws + "{\n";
                str += ws + "   get { return " + name + "; }\n";
                str += ws + "   set { " + name + " = value; }\n";
                str += ws + "}\n";
                //array types
            } else {
                if (list[i].length == -1) {
                    str += ws + "public List<" + getCsType(infos, list[i]) + "> " + name2 + "\n" + ws + "{\n";
                    str += ws + "    get { return " + name + "; }\n";
                    str += ws + "}\n\n";

                    // don't allow a set method with lists or arrays (get the list and then perform operations)
                    // fixed-length arrays
                } else {
                    str += ws + "public " + type + "[] " + name2 + "\n" + ws + "{\n";
                    str += ws + "    get { return " + name + "; }\n";
                    str += ws + "}\n\n";
                }
            }
        }
        return str;
    }

    public static String member_declaration(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        FieldInfo[] list = st.fields;
        for (int i = 0; i < list.length; i++) {
            String name = "_" + toCamelCase(list[i].name);
            String defaultVal = list[i].defaultVal;
            String type = list[i].type;

            if (!list[i].isArray) {
                str += ws + "private " + getCsType(infos, list[i]) + " " + name;
                if (list[i].isStruct) {
                    if (defaultVal.equalsIgnoreCase("null")) {
                        str += " = null;\n";
                    } else {
                        str += " = new " + getCsType(infos, list[i]) + "();\n";
                    }
                } else if (list[i].isEnum) {
                    str += " = " + getCsType(infos, list[i]) + "." + defaultVal + ";\n";
                } else if (defaultVal.length() == 0 && !type.equals("string")) {
                    str += ";\n";
                    continue;
                } else if (type.equals("bool")) {
                    str += " = " + Boolean.valueOf(defaultVal) + ";\n";
                } else if (type.equals("char")) {
                    str += " = '" + defaultVal + "';\n";
                } else if (type.equals("real32")) {
                    str += " = (float)" + defaultVal + ";\n";
                } else if (type.equals("string")) {
                    str += " = \"" + defaultVal + "\";\n";
                } else {
                    str += " = " + defaultVal + ";\n";
                }
                // arrays
            } else {
                String typeName = getCsType(infos, list[i]);

                if (list[i].length == -1) {
                    str += ws + "private List<" + typeName + "> " + name
                            + " = new List<" + typeName + ">();\n";
                } else {
                    // for primitive arrays, use the primitive type name
                    //if (!list[i].isStruct) typeName = typeName.toLowerCase();
                    if (defaultVal.isEmpty()) {
                        // if no default val specified, use the natural defaults (0 for numbers, null for objects) and continue
                        str += ws + "private " + typeName + "[] " + name + " = new " + typeName + "[" + list[i].length + "];\n";
                    } else {
                        str += ws + "private " + typeName + "[] " + name + " = new " + typeName + "[]{ ";
                        for (int j = 0; j < list[i].length; j++) {
                            if (list[i].isStruct) {
                                if (defaultVal.equalsIgnoreCase("null")) {
                                    str += "null, ";
                                } else {
                                    str += "new " + typeName + "(), ";
                                }
                            } else if (type.equals("bool")) {
                                str += (defaultVal.equalsIgnoreCase("TRUE") ? "true" : "false") + ", ";
                            } else if (type.equals("char")) {
                                str += "'" + defaultVal + "', ";
                            } else {
                                str += defaultVal + ", ";
                            }
                        }
                        str += "};\n";
                    }
                }
            }
        }
        return str;
    }

    public static String calc_size(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        int size = 15;  // length of the series name (8), the type (4), the version (2) and the null byte (1)
        String str = "";
        FieldInfo[] list = st.fields;

        for (int i = 0; i < list.length; i++) {
            String name = "_" + toCamelCase(list[i].name);
            //scalar
            if (!list[i].isArray) {
                if (list[i].isStruct) {
                    // if the object is not null, add type and object size, otherwise, add zero for null
                    str += ws + "size += ( " + name + " != null ) ? " + name + ".CalculateSize() : 1;\n";
                } else if (list[i].type.equalsIgnoreCase("string")) {
                    str += ws + "if (" + name + " != null) size += " + name + ".Length;\n";
                    size += 2; // add 2 bytes for the ushort before the string array
                } else {
                    size += sizeOf(info, list[i]);
                }
            } else {
                // arrays
                if (list[i].length == -1) {
                    size += list[i].isLargeArray ? 4 : 2; // add the ushort or uint that sits ahead of a variable array
                    if (list[i].isStruct) {
                        // if not null, add the calculated size, otherwise just add 1 bytes for null byte
                        str += ws + "for(int i=0; i<" + name + ".Count; i++) {\n";
                        str += ws + "    if (" + name + "[i] != null) size += " + name + "[i].CalculateSize();\n";
                        str += ws + "    else size += 1;\n";
                        str += ws + "}\n";

                    } else if (list[i].type.equalsIgnoreCase("string")) {
                        str += ws + "size += " + name + ".Count * 2;\n";
                        str += ws + "for(int i=0; i<" + name + ".Count; i++) {\n";
                        str += ws + "    if (" + name + "[i] != null) size += " + name
                                + "[i].Length;\n";
                        str += ws + "}\n";

                    } else {
                        str += ws + "\n" + ws + "size += " + String.valueOf(sizeOf(info, list[i])) + " * "
                                + name + ".Count;\n";
                    }
                } else {  // fixed length arrays
                    if (list[i].isStruct) {
                        // if not null, add the calculated size and 4 bytes for type and 1 for null byte.
                        // add 1 byte if null
                        str += ws + "for(int i=0; i<" + name + ".Length; i++) {\n";
                        str += ws + "    if (" + name + "[i] != null) size += 5 + " + name
                                + "[i].CalculateSize();\n";
                        str += ws + "    else size += 1;\n";
                        str += ws + "}\n";
                        size += list[i].length;  // adds the bool for every element

                    } else if (list[i].type.equalsIgnoreCase("string")) {
                        str += ws + "for(int i=0; i<" + name + ".Length; i++) {\n";
                        str += ws + "    if (" + name + "[i] != null) size += "
                                + name + "[i].Length;\n";
                        str += ws + "}\n";

                    } else {
                        size += list[i].length * sizeOf(info, list[i]);
                    }
                }
            }
        }

        String retStr = ws + "int size = " + String.valueOf(size);
        retStr += "; // accounts for primitive types, object null bytes, array leader bytes, some string leader bytes\n";
        retStr += str + "\n" + ws + "return ";
        if (st.extends_name.length() != 0) {
            retStr += "base.CalculateSize() + ";
        }
        retStr += "size;";

        return retStr;
    }

    public static String member_unpack(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        if (st.extends_name.length() != 0) {
            str += ws + "base.Unpack(buf);\n";
        }
        FieldInfo[] list = st.fields;

        for (int i = 0; i < st.fields.length; i++) {
            String type = getCsObjectType(infos, list[i]);
            String name = "_" + toCamelCase(list[i].name);
            //type = type.substring(0, 1).toUpperCase() + type.substring(1);

            // Scalar Types
            if (list[i].isScalar) {
                if (list[i].isStruct) {
                    str += ws + name + " = (" + type + ") br.ReadObject();\n";
                } else if (list[i].isEnum) {
                    str += ws + name + " = (" + type + ") br.ReadInt32();\n\n";
                } else {
                    str += ws + name + " = br.Read" + type + "();\n\n";
                }
                // Array Types
            } else if (list[i].isArray) {
                // variable length arrays
                if (list[i].length == -1) {
                    str += ws + name + ".Clear();\n";
                    
                    if (list[i].isLargeArray) {
                        str += ws + "uint " + name + "_len = br.ReadUInt32();\n";
                    } else {
                        str += ws + "ushort " + name + "_len = br.ReadUInt16();\n";
                    }                  

                    str += ws + "for(int i=0; i<" + name + "_len; i++){\n";
                    if (list[i].isStruct) {
                        str += ws + name + ".Add( (" + type + ") br.ReadObject() );\n";
                    } else if (list[i].isEnum) {
                        str += ws + "    " + name + ".Add( (" + type + ") br.ReadInt32());\n";
                    } else {
                        str += ws + "    " + name + ".Add(" + "br.Read" + type + "());\n";
                    }
                    str += ws + "}\n";

                } else {  // fixed length arrays
                    str += ws + "for(int i=0; i<" + list[i].length + "; i++){\n";
                    if (list[i].isStruct) {
                        str += ws + "    " + name + "[i] = (" + type + ") br.ReadObject() );\n";
                    } else if (list[i].isEnum) {
                        str += ws + "    " + name + "[i] = (" + type + ") br.ReadInt32();\n";
                    } else {
                        str += ws + "    " + name + "[i] = " + "br.Read" + type + "();\n";
                    }
                    str += ws + "}\n";
                }
            }
        }

        return str;
    }

    public static String member_pack(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        FieldInfo[] list = st.fields;
        if (st.extends_name.length() != 0) {
            str += ws + "base.Pack(buf);\n";
        }

        // for each field in the message
        for (int i = 0; i < list.length; i++) {
            String name = "_" + toCamelCase(list[i].name);

            //scalar types
            if (list[i].isScalar) {
                if (list[i].isStruct) {
                    str += ws + "bw.Write(" + name + ");\n";
                } else if (list[i].isEnum) {
                    str += ws + "bw.Write( (int) " + name + ");\n";
                } else {
                    str += ws + "bw.Write(" + name + ");\n";
                }
                // array types
            } else {
                int length = list[i].length;
                //variable length arrays
                if (length == -1) {
                    if (list[i].isLargeArray) {
                        str += ws + "bw.Write((uint) " + name + ".Count);\n";
                    }
                    else {
                        str += ws + "bw.Write((ushort) " + name + ".Count);\n";
                    }
                    
                    str += ws + "for(int i=0; i<" + name + ".Count; i++){\n";
                    if (list[i].isStruct) {
                        str += ws + "    bw.Write(" + name + "[i]);\n";
                    } else if (list[i].isEnum) {
                        str += ws + "bw.Write( (int) " + name + "[i]);\n";
                    } else {
                        str += ws + "    bw.Write(" + name + "[i]);\n";
                    }
                    str += ws + "}\n";
                } else {
                    //fixed length arrays
                    str += ws + "for(int i=0; i<" + list[i].length + "; i++){\n";
                    if (list[i].isStruct) {
                        str += ws + "    bw.Write(" + name + "[i]);\n";
                    } else if (list[i].isEnum) {
                        str += ws + "    bw.Write( (int) " + name + "[i]);\n";
                    } else {
                        str += ws + "    bw.Write(" + name + "[i]);\n";
                    }
                    str += ws + "}\n";
                }
            }
        }
        return str;
    }

    public static String object_type_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.name;
    }

    public static String object_type_number(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.id;
    }

    public static String parent_object_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        if (st.extends_name.length() == 0) {
            return ws + "ILmcpObject";
        } else {
            return ws + getSeriesNamespace(infos, st.extends_series) + st.extends_name;
        }
    }
    
    private static String getSeriesNamespace(MDMInfo[] infos, String series_name) {
        MDMInfo i = MDMReader.getMDM(series_name, infos);
        if (i != null) {
            String[] splits = i.namespace.split("/");
            String ret = "";
            for (String tmp : splits) {
                if (tmp.length() > 0) {
                    ret = ret + toPascalCase(tmp) + ".";
                }
            }
        
            return ret;
        }
        return "";
    }

    public static String _override(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        if (st.extends_name.length() == 0) {
            return ws + "virtual";
        } else {
            return ws + "override";
        }
    }

    public static String member_print(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        FieldInfo[] list = st.fields;
        if (st.extends_name.length() != 0) {
            str += ws + member_print(infos, info, outfile, MDMInfo.getParentType(infos, st), en, ws) + "\n";
        }
        for (int i = 0; i < list.length; i++) {
            String type = list[i].type;
            String varname = toPascalCase(list[i].name);
            if (varname.equals(toPascalCase(st.name)))
                varname += "Value";
            if (!list[i].isArray) {
                if (list[i].isStruct) {
                    str += ws + "buf.Append(\"  " + list[i].name + "\"  + (" + varname + " == null ? \" == null \" : \"\") + \"(" + type + ")\\n\");\n";
                } else {
                    str += ws + "buf.Append(\"  " + list[i].name + " = \" + Convert.ToString(" + varname + ")" + " + \" (" + type + ")\\n\");\n";
                }
            } else {
                if (list[i].length == -1) {
                    str += ws + "buf.Append(\"  " + list[i].name + " = " + type + "[\" + " + varname + ".Count + \", var]\\n\");\n";
                } else {
                    str += ws + "buf.Append(\"  " + list[i].name + " = " + type + "[\" + " + varname + ".Length + \"] \\n\");\n";
                }
            }

        }

        return str;
    }

    public static String tester_create_objects(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < info.structs.length; i++) {
            buf.append(ws).append(info.structs[i].name).append(" o").append(i + 1).append(" = new ");
            buf.append(info.structs[i].name).append("();\n");
            buf.append(ws).append("v.Add(o").append(i + 1).append(");\n");
        }
        return buf.toString();
    }

    public static String xml_seriesname(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return info.seriesName;
    }

    public static String xml_add_series_readers(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            buf.append(ws + "RegisterXmlReader( new ").append(getCsNamespace(i.namespace)).append(".SeriesXmlReader());\n");
        }
        return buf.toString();
    }

    public static String xml_create_visits(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";

        for (int i = 0; i < info.structs.length; i++) {
            StructInfo dt_tmp = info.structs[i];
            str += ws + "if (type.Equals(\"" + dt_tmp.name + "\")){\n";
            str += ws + dt_tmp.name + " o = new " + dt_tmp.name + "();\n";
            str += ws + "    XmlNodeList list = el.ChildNodes;\n";
            str += ws + "    for (int i=0; i<list.Count; i++) {\n";
            str += ws + "        string name = list.Item(i).Name;\n";

            ArrayList<FieldInfo> fields = new ArrayList<FieldInfo>();
            fields.addAll(Arrays.asList(dt_tmp.fields));
            while (dt_tmp.extends_name.length() != 0) {
                dt_tmp = MDMInfo.getParentType(infos, dt_tmp);
                fields.addAll(Arrays.asList(dt_tmp.fields));
            }

            for (int j = 0; j < fields.size(); j++) {
                FieldInfo f = fields.get(j);
                String setname = f.name.substring(0, 1).toUpperCase() + f.name.substring(1);
                if (setname.equals(toPascalCase(dt_tmp.name)))
                    setname += "Value";
                String defaultVal = f.defaultVal;
                String cstype = getCsType(infos, f);
                str += ws + "        if(name.Equals(\"" + f.name + "\")){\n";
                if (f.isArray) {
                    str += ws + "            XmlNodeList tmp = list.Item(i).ChildNodes;\n";
                    str += ws + "            for (int j=0; j<tmp.Count; j++) {\n";
                    str += ws + "                if ( tmp.Item(j) is XmlElement)\n";
                    if (f.length == -1) {
                        if (f.isStruct) {
                            str += ws + "                    o." + setname + ".Add( (" + cstype + ") ReadXML( (XmlElement) tmp.Item(j)));\n";
                        } else if (f.isEnum) {
                            str += ws + "                    o." + setname + ".Add( (" + cstype + ") Enum.Parse( typeof(" + cstype + "), get_string( (XmlElement) tmp.Item(j), \"" + defaultVal + "\")));\n";
                        } else {
                            str += ws + "                    o." + setname + ".Add( get_" + f.type + "( (XmlElement) tmp.Item(j), \"" + defaultVal + "\"));\n";
                        }
                    } else {
                        if (f.isStruct) {
                            str += ws + "                    o." + setname + "[j] = (" + cstype + ") ReadXML( (XmlElement) tmp.Item(j));\n";
                        } else if (f.isEnum) {
                            str += ws + "                    o." + setname + "[j] = (" + cstype + ") Enum.Parse( typeof(" + cstype + "), get_string( (XmlElement) tmp.Item(j), \"" + defaultVal + "\"));\n";
                        } else {
                            str += ws + "                    o." + setname + "[j] = get_" + f.type + "( (XmlElement) tmp.Item(j), \"" + defaultVal + "\");\n";
                        }
                    }
                    str += ws + "            }\n";
                } else {
                    if (f.isStruct) {
                        str += ws + "            XmlNode tmp = list.Item(i).FirstChild;\n";
                        str += ws + "            while ( !(tmp is XmlElement) && tmp != null ) { tmp = tmp.NextSibling; }\n";
                        str += ws + "            if (tmp != null) \n";
                        str += ws + "                o." + setname + " = ((" + cstype + ") ReadXML( (XmlElement) tmp ));\n";
                    } else if (f.isEnum) {
                        str += ws + "            o." + setname + " =  (" + cstype + ") Enum.Parse( typeof(" + cstype + "), get_string( (XmlElement) list.Item(i), \"" + defaultVal + "\"));\n";
                    } else {
                        str += ws + "            o." + setname + " = ( get_" + f.type + "( (XmlElement) list.Item(i), \"" + defaultVal + "\" ));\n";
                    }
                }
                str += ws + "            continue;\n";
                str += ws + "        }\n";
            }

            str += ws + "    }\n";
            str += ws + "    return o;\n";
            str += ws + "}\n";

        }
        return str;
    }

    public static String xml_write_object(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        StructInfo dt_tmp = st;
        ArrayList<FieldInfo> fields = new ArrayList<FieldInfo>();
        fields.addAll(Arrays.asList(dt_tmp.fields));
        while (dt_tmp.extends_name.length() != 0) {
            dt_tmp = MDMInfo.getParentType(infos, dt_tmp);
            fields.addAll(Arrays.asList(dt_tmp.fields));
        }

        str += (ws + "buf.Append( ws + \"<" + st.name + " Series=\\\"" + st.seriesName + "\\\">\\n\");\n");

        for (FieldInfo f : fields) {
            String name = toPascalCase(f.name);
            if (name.equals(toPascalCase(st.name)))
                name += "Value";
            if (f.isArray) {
                str += ws + "buf.Append( ws + \"  <" + f.name + ">\\n\");\n";
                if (f.length == -1) {
                    str += ws + "for (int i=0; i<" + name + ".Count; i++) {\n";
                    if (f.isStruct) {
                        str += ws + "    buf.Append( " + name + "[i] == null ? ( ws + \"    <null/>\\n\") : (" + name + "[i].ToXml(ws + \"    \")) + \"\\n\");\n";
                    } else {
                        str += ws + "buf.Append( ws + \"  <" + f.type + ">\" + Convert.ToString(" + name + "[i]) + \"</" + f.type + ">\\n\");\n";
                    }
                } else {
                    str += ws + "for (int i=0; i<" + name + ".Length; i++) {\n";
                    if (f.isStruct) {
                        str += ws + "    buf.Append( " + name + "[i] == null ? ( ws + \"    <null/>\\n\") : (" + name + "[i].ToXml(ws + \"    \")) + \"\\n\");\n";
                    } else {
                        str += ws + "buf.Append( ws + \"  <" + f.type + ">\" + Convert.ToString(" + name + "[i]) + \"</" + f.type + ">\\n\");\n";
                    }
                }
                str += ws + "}\n";
                str += ws + "buf.Append( ws + \"  </" + f.name + ">\\n\");\n";
            } else if (f.isStruct) {
                str += ws + "buf.Append( ws + \"  <" + f.name + ">\");\n";
                str += ws + "buf.Append( " + name + " == null ? (\"null\") : ( \"\\n\" + " + name + ".ToXml(ws + \"    \")) + \"\\n\" + ws + \"  \");\n";
                str += ws + "buf.Append( \"  </" + f.name + ">\\n\");\n";
            } else {
                str += ws + "buf.Append( ws + \"  <" + f.name + ">\" + Convert.ToString(" + name + ") + \"</" + f.name + ">\\n\");\n";
            }
        }

        str += ws + "buf.Append( ws + \"</" + st.name + ">\");\n";

        return str;
    }

    public static String equals_compare_object(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {

        String code = ws + st.name + " other = (" + st.name + ")obj;\n\n";

        if (st.extends_name.length() != 0)
            code += ws + "if (!base.Equals(obj)) return false;\n\n";

        ArrayList<FieldInfo> fields = new ArrayList<FieldInfo>();
        fields.addAll(Arrays.asList(st.fields));

        for (FieldInfo f : fields) {

            String name = toPascalCase(f.name);
            if (name.equals(toPascalCase(st.name)))
                name += "Value";

            code += ws + "// " + name + "\n";
            if (f.isStruct)
                code += String.format("%1$sif ((%2$s == null && other.%2$s != null) || (other.%2$s == null && %2$s != null)) return false;\n", ws, name);

            if (f.isArray) {
                String sizePropName = "Length";
                if (f.length == -1)
                    sizePropName = "Count";

                code += String.format("%1$sif (%2$s != null)\n%1$s{\n", ws, name);
                code += String.format("%1$s    if (%2$s.%3$s != other.%2$s.%3$s) return false;\n", ws, name, sizePropName);
                code += String.format("%1$s    for (int i = 0; i < %2$s.%3$s; ++i)\n", ws, name, sizePropName);
                code += String.format("%1$s      if (!%2$s[i].Equals(other.%2$s[i])) return false;\n", ws, name);
                code += String.format("%1$s}\n\n", ws, name);
            }
            else{
                code += String.format("%1$sif (", ws);
                if (f.isStruct && f.isOptional) {
                    code += String.format("%1$s != null && ", name);
                }
                code += String.format("!%1$s.Equals(other.%1$s)) return false;\n\n", name);
            }
        }

        code += "\n" + ws + "return true;";

        return code;
    }

    public static String send_all_messages(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            String ns = getCsNamespace(i.namespace);
            for (StructInfo s : i.structs ) {
                buf.append(ws + "Console.WriteLine(\"Sending " + s.name + "...\");\n");
                buf.append(ws + "o = new " + ns + "." + s.name + "();\n");
                buf.append(ws + "writer.Write( Avtas.Lmcp.LmcpFactory.PackMessage(o, true));\n\n");
            }
        }
        return buf.toString();
    }

    public static String project_sources(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            String winDir = getCsNamespace(i.namespace).replaceAll("\\.", "\\\\");
            for (StructInfo s : i.structs ) {
                str += ws + "<Compile Include=\"" + winDir + "\\" + s.name + ".cs\"/>\n";
            }
            for (EnumInfo s : i.enums ) {
                str += ws + "<Compile Include=\"" + winDir + "\\" + s.name + ".cs\"/>\n";
            }
            str += ws + "<Compile Include=\"" + winDir + "\\"  + "SeriesEnum.cs\"/>\n";
            str += ws + "<Compile Include=\"" + winDir + "\\"  + "SeriesList.cs\"/>\n";
            str += ws + "<Compile Include=\"" + winDir + "\\"  + "SeriesXmlReader.cs\"/>\n";
        }
        return str;
    }
    
    public static String series_csproj_sources(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        String winDir = getCsNamespace(info.namespace).replaceAll("\\.", "\\\\");
        for (StructInfo s : info.structs ) {
            str += ws + "<Compile Include=\"" + winDir + "\\" + s.name + ".cs\"/>\n";
        }
        for (EnumInfo s : info.enums ) {
            str += ws + "<Compile Include=\"" + winDir + "\\" + s.name + ".cs\"/>\n";
        }
        str += ws + "<Compile Include=\"" + winDir + "\\"  + "SeriesEnum.cs\"/>\n";
        str += ws + "<Compile Include=\"" + winDir + "\\"  + "SeriesList.cs\"/>\n";
        str += ws + "<Compile Include=\"" + winDir + "\\"  + "SeriesXmlReader.cs\"/>\n";
        return str;
    }

    public static String series_csproj_references(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        
        String str = "";
        for (String dep : info.mdmDependencies) {
            for (MDMInfo i : infos) {
                if (i.seriesName.contentEquals(dep))
                {
                    String libName = dep.substring(0, 1).toUpperCase() + dep.substring(1).toLowerCase();
                    str += ws + "<ProjectReference Include=\"Lmcp" + libName + ".csproj\">\n";
                    str += ws + "  <Project>{" + i.guid + "}</Project>\n";
                    str += ws + "  <Name>Lmcp" + libName + "</Name>\n";
                    str += ws + "</ProjectReference>\n";
                }
            }
        }
        return str;
    }
    
    public static String all_series_csproj_references(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (MDMInfo i : infos) {
            if (i.seriesNameAsLong == 0) { continue; }
            String libName = i.seriesName.substring(0, 1).toUpperCase() + i.seriesName.substring(1).toLowerCase();
            str += ws + "<ProjectReference Include=\"Lmcp" + libName + ".csproj\">\n";
            str += ws + "  <Project>{" + i.guid + "}</Project>\n";
            str += ws + "  <Name>Lmcp" + libName + "</Name>\n";
            str += ws + "</ProjectReference>\n";
        }
        return str;
    }
	
	public static String get_nuget_mdm_dependencies(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "    <dependencies>\n";
        if ( infos.length == 0 )
            return str + "    </dependencies>\n";
        
        for (String dep : info.mdmDependencies) {
            for (MDMInfo i : infos) {
                if (i.seriesName.contentEquals(dep))
                {
                    String currentVersion = String.valueOf(i.version);
                    String nextVersion = String.valueOf(i.version + 1);
                    
                    String nugetPkg = dep.substring(0, 1).toUpperCase() + dep.substring(1).toLowerCase();
                    str += "      <dependency id=\""+nugetPkg+"\" version=\"["+currentVersion+","+nextVersion+")\"/>\n";
                }
            }
        }
        
        str += "    </dependencies>";
        return str;
    }
    
    public static String all_series_projects(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for(MDMInfo i : infos) {
            String libName = i.seriesName.substring(0, 1).toUpperCase() + i.seriesName.substring(1).toLowerCase();
            str += ws + "Project(\"{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}\") = \"Lmcp" + libName + "\", \"Library\\Lmcp" + libName + ".csproj\", \"{" + i.guid + "}\"\n";
            str += ws + "EndProject\n";
        }
        return str;
    }
    
    public static String all_series_configurations(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for(MDMInfo i : infos) {
            str += ws + "{" + i.guid + "}.Debug|Any CPU.ActiveCfg = Debug|Any CPU\n";
            str += ws + "{" + i.guid + "}.Debug|Any CPU.Build.0 = Debug|Any CPU\n";
            str += ws + "{" + i.guid + "}.Release|Any CPU.ActiveCfg = Release|Any CPU\n";
            str += ws + "{" + i.guid + "}.Release|Any CPU.Build.0 = Release|Any CPU\n";
        }
        return str;
    }
    
    public static String library_make_guid(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        if (libraryGuid == null)
            libraryGuid = makeGUID("LMCP");
        return libraryGuid;
    }
    
    public static String core_library_make_guid(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        if (coreLibraryGuid == null)
            coreLibraryGuid = makeGUID("LmcpCore");
        return coreLibraryGuid;
    }
    
    public static String test_client_make_guid(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        if (testClientGuid == null)
            testClientGuid = makeGUID("TestClient");
        return testClientGuid;
    }

   public static String test_server_make_guid(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        if (testServerGuid == null)
            testServerGuid = makeGUID("TestServer");
        return testServerGuid;
    }

   public static String series_guid(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return info.guid;
    }

   public static String major_version(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
       return String.valueOf(infos[0].version);
   }

    /******************* Utility Methods ******************************/

    public static String toCamelCase(String input) {
        if (input.length() == 0)
            return input;
        else
            return input.substring(0, 1).toLowerCase() + input.substring(1);
    }

    public static String toPascalCase(String input) {
        if (input.length() == 0)
            return input;
        else
            return input.substring(0, 1).toUpperCase() + input.substring(1);
    }

    public static String cleanComment(String comment){
        return comment.replaceAll("\\s+", " ");
    }

    /** this returns the java type that is associated with the LMCP type */
    public static String getCsType(MDMInfo[] mdms, FieldInfo f) {
        String type = f.type;
        if (type.equalsIgnoreCase("uint16")) {
            return "ushort";
        }
        if (type.equalsIgnoreCase("uint32")) {
            return "uint";
        }
        if (type.equalsIgnoreCase("byte")) {
            return "byte";
        }
        if (type.equalsIgnoreCase("bool")) {
            return "bool";
        }
        if (type.equalsIgnoreCase("int16")) {
            return "short";
        }
        if (type.equalsIgnoreCase("int32")) {
            return "int";
        }
        if (type.equalsIgnoreCase("int64")) {
            return "long";
        }
        if (type.equalsIgnoreCase("real32")) {
            return "float";
        }
        if (type.equalsIgnoreCase("real64")) {
            return "double";
        }
        if (type.equalsIgnoreCase("char")) {
            return "char";
        }
        if (type.equalsIgnoreCase("string")) {
            return "string";
        }
        if (type.equalsIgnoreCase(MDMInfo.LMCP_OBJECT_NAME)) {
            return "Avtas.Lmcp.ILmcpObject";
        }
        return getCsNamespace(mdms, f.seriesName) + "." + f.type;
    }

    /** returns the Object type associated with the LMCP type passed */
    public static String getCsObjectType(MDMInfo[] mdms, FieldInfo f) {
        String type = f.type;
        if (type.equalsIgnoreCase("uint16")) {
            return "UInt16";
        }
        if (type.equalsIgnoreCase("uint32")) {
            return "UInt32";
        }
        if (type.equalsIgnoreCase("byte")) {
            return "Byte";
        }
        if (type.equalsIgnoreCase("bool")) {
            return "Boolean";
        }
        if (type.equalsIgnoreCase("char")) {
            return "Char";
        }
        if (type.equalsIgnoreCase("int16")) {
            return "Int16";
        }
        if (type.equalsIgnoreCase("int32")) {
            return "Int32";
        }
        if (type.equalsIgnoreCase("int64")) {
            return "Int64";
        }
        if (type.equalsIgnoreCase("real32")) {
            return "Single";
        }
        if (type.equalsIgnoreCase("real64")) {
            return "Double";
        }
        if (type.equalsIgnoreCase("string")) {
            return "String";
        }
        if (type.equalsIgnoreCase(MDMInfo.LMCP_OBJECT_NAME)) {
            return "Avtas.Lmcp.ILmcpObject";
        }
        return getCsNamespace(mdms, f.seriesName) + "." + f.type;
    }

    /** returns the size in bytes of the LMCP type */
    public static int sizeOf(MDMInfo mdm, FieldInfo field) throws Exception {
        if (field.isEnum) {
            return 4;
        }
        String type = field.type;
        if (type.toLowerCase().matches("(byte)|(char)|(bool)")) {
            return 1;
        }

        if (type.toLowerCase().matches("(int16)|(uint16)")) {
            return 2;
        }

        if (type.toLowerCase().matches("(int32)|(uint32)|(real32)")) {
            return 4;
        }

        if (type.toLowerCase().matches("(int64)|(real64)")) {
            return 8;
        }

        throw new Exception("Invalid struct");
    }

    public static String getCsNamespace(MDMInfo[] infos, String seriesName) {
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            if (i.seriesName.equals(seriesName)) {
                return getCsNamespace(i.namespace);
            }
        }
        return "";
    }

    /** Generates a Globally Unique ID based on RFC 4122.  
     * @return a Globally unique ID with the format: 8-4-4-4-12 digits
     */
    private static String makeGUID(String startName) {
        
        return UUID.nameUUIDFromBytes(startName.getBytes()).toString().toUpperCase(); //  UUID.randomUUID().toString().toUpperCase();

    }

    public static String getCsNamespace(String namespace) {
        String[] splits = namespace.split("/");
        String ret = "";
        for (String tmp : splits) {
            if (tmp.length() > 0) {
                tmp = tmp.substring(0, 1).toUpperCase() + tmp.substring(1);
                ret = ret + tmp + ".";
            }
        }
        return ret.substring(0, ret.length() - 1);
    }
    
    public static String getWinNamespace(String namespace) {
        String[] splits = namespace.split("/");
        String ret = "";
        for (String tmp : splits) {
            if (tmp.length() > 0) {
                ret = ret + tmp + "\\";
            }
        }
        return ret;
    }
}
