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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class JavaMethods {

    public static final String block_comment = "\n*";
    public static final String NUMBER_MATCHER = "(byte)|(int16)|(uint16)|(int32)|(uint32)|(real32)|(real64)";

    public static String package_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + "package " + info.namespace.replace('/', '.') + ";\n";
    }

    public static String namespace(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + info.namespace;
    }

    public static String package_path(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + info.namespace.replace('/', '.');
    }

    public static String datatype_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.name;
    }
    
    public static String class_comment(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + "/**\n" + st.comment + "\n*/";
    }

    public static String make_field_constructor(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        if (st.fields.length == 0) {
            return "";
        }
        List<FieldInfo> fieldList = new ArrayList<FieldInfo>();
        StructInfo tmp = st;
        int total = 0;
        do {
            List<FieldInfo> tmpList = new ArrayList<FieldInfo>();
            for (int i = 0; i < tmp.fields.length; i++) {
                if (tmp.fields[i].isScalar) {
                    total ++;
                    tmpList.add(tmp.fields[i]);
                }
            }
            fieldList.addAll(0, tmpList);
            tmp = MDMInfo.getParentType(infos, tmp);
        } while(tmp != null);

        if (fieldList.size() == 0) {
            return "";
        }

        buf.append(ws).append("public ").append(st.name).append("(");
        for (FieldInfo f : fieldList) {
            buf.append(getJavaType(f, infos)).append(" ").append(f.name);
            if (fieldList.indexOf(f) != fieldList.size() - 1) {
                buf.append(", ");
            } 
        }
        buf.append("){\n");
        for (FieldInfo f : fieldList) {
            buf.append(ws).append("    this.").append(f.name).append(" = ").append(f.name).append(";\n");
        }
        buf.append(ws).append("}\n");

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
                buf.append(ws + "import " + i.namespace.replace("/", ".") + ".*;\n");
            }
        }
        return buf.toString();
    }

    public static String enum_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + en.name;
    }

    public static String enum_comment(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + "/** " + en.comment + "*/";
    }

    public static String gen_enum_fields(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        int len = en.entries.size();
        for (int i = 0; i < len; i++) {
            EnumEntry entry = en.entries.get(i);
            buf.append(ws + "/** " + entry.comment + " */\n");
            buf.append(ws + entry.name).append("(").append(entry.value).append(")");
            if (i != len - 1) {
                buf.append(",\n");
            } else {
                buf.append(";\n");
            }
        }
        return buf.toString();
    }

    public static String enum_from_int(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < en.entries.size(); i++) {
            buf.append(ws + "case " + en.entries.get(i).value + " : return " + en.entries.get(i).name + ";\n");
        }
        buf.append(ws + "default: return " + en.entries.get(0).name + ";\n");
        return buf.toString();
    }

    public static String factory_add_series(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo in : infos) {
            if(in.seriesNameAsLong == 0)
            {
                continue;
            }
            String seriesListClass = in.namespace.replaceAll("/", ".") + ".SeriesEnum";
            buf.append(ws + "{ LMCPEnum e = new " + seriesListClass + "();\n");
            buf.append(ws + "addSeries(e); } \n");
        }
        return buf.toString();
    }

    public static String list_all_types(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuilder buf = new StringBuilder();
        for (int i = 0; i < info.structs.length; i++) {
            buf.append(ws).append("\"").append(info.structs[i].name).append("\"");
            if (i != info.structs.length - 1)
                buf.append(",\n");
        }
        return buf.toString();
    }

    public static String extends_object_type(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        if (st.extends_name.length() == 0) {
            return ws + "extends avtas.lmcp.LMCPObject";
        } else {
            return ws + "extends " + getJavaClassName(infos, st.extends_name, st.extends_series);
        }
    }

    public static String series_name_setup(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {

        StringBuffer buf = new StringBuffer();
        buf.append(ws + "public static final String SERIES_NAME = \"" + info.seriesName + "\";\n");
        buf.append(ws + "/** Series Name turned into a long for quick comparisons. */\n");
        buf.append(ws + "public static final long SERIES_NAME_ID = " + info.seriesNameAsLong + "L;\n");
        buf.append(ws + "public static final int SERIES_VERSION = " + info.version + ";\n");
        return buf.toString();
    }

    public static String member_gets_and_sets(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        FieldInfo[] list = st.fields;
        for (int i = 0; i < list.length; i++) {
            String type = getJavaType(list[i], infos);
            String comment = list[i].comment.replaceAll("\\s+", " ");
            comment = comment.replaceAll("<br", "\n" + ws + "*<br") + "(Units: " + list[i].units + ")";
            String name = list[i].name;
            String name2 = list[i].name.substring(0, 1).toUpperCase() + list[i].name.substring(1);
            // scalar types
            if (list[i].isScalar) {
                str += ws + "/** " + comment + "*/\n";
                str += ws + "public " + type + " get" + name2 + "() { return " + name + "; }\n\n";

                str += ws + "/** " + comment + "*/\n";
                str += ws + "public " + st.name + " set" + name2 + "( " + type + " val ) {\n";
                str += ws + "    " + name + " = val;\n";
                str += ws + "    return this;\n";
                str += ws + "}\n\n";
                //array types
            } else if (list[i].isArray) {
                if (list[i].length == -1) {
                    str += ws + "public java.util.ArrayList<" + getJavaObjectType(list[i], infos) + "> get" + name2 + "() {\n";
                    str += ws + "    return " + name + ";\n";
                    str += ws + "}\n\n";

                    // don't allow a set method with array lists or arrays (get the list and then perform operations)
                    // fixed-length arrays
                } else {
                    str += ws + "public " + type + "[] get" + name2 + "() {\n";
                    str += ws + "    return " + name + ";\n";
                    str += ws + "}\n\n";
                }
            } else if (list[i].isMap) {
                str += ws + "public java.util.HashMap<" + getJavaObjectType(list[i], infos) + "> get" + name2 + "() {\n";
                str += ws + "    return " + name + ";\n";
                str += ws + "}\n\n";
            }
        }
        return str;
    }

    public static String member_declaration(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        FieldInfo[] list = st.fields;
        for (int i = 0; i < list.length; i++) {
            String name = list[i].name;
            String defaultVal = list[i].defaultVal;
            String type = list[i].type;

            String comment = list[i].comment.replaceAll("\\s+", " ");
            comment = comment.replaceAll("<br", "\n" + ws + "*<br") + "(Units: " + list[i].units + ")";
            str += ws + "/** " + comment + "*/\n";
            str += ws + "@LmcpType(\"" + type + "\")\n";

            if (list[i].isEnum) {
                EnumInfo enumInfo = MDMInfo.getEnumByName(infos, list[i]);
                type = enumInfo.namespace.replaceAll("/", ".") + "." + type;
            }

            if (list[i].isScalar) {
                str += ws + "protected " + getJavaType(list[i], infos) + " " + name;
                if (list[i].isStruct) {
                    if (defaultVal.equalsIgnoreCase("null")) {
                        str += " = null;\n";
                    } else {
                        str += " = new " + getJavaType(list[i], infos) + "();\n";
                    }
                } else if (list[i].isEnum) {
                    if (defaultVal.isEmpty()) {
                        str += " = " + type + ".getEnum(0);\n";
                    } else {
                        str += " = " + type + "." + list[i].defaultVal + ";\n";
                    }
                } else if (defaultVal.length() == 0 && !type.equals("string")) {
                    str += ";\n";
                    continue;
                } else if (type.equals("bool")) {
                    str += " = " + Boolean.valueOf(defaultVal) + ";\n";
                } else if (type.equals("byte")) {
                    str += " = (byte)" + defaultVal + ";\n";
                } else if (type.equals("char")) {
                    str += " = '" + defaultVal + "';\n";
                } else if (type.equals("int16")) {
                    str += " = (short)" + defaultVal + ";\n";
                } else if (type.equals("real32")) {
                    str += " = (float)" + defaultVal + ";\n";
                } else if (type.equals("string")) {
                    str += " = \"" + defaultVal + "\";\n";
                } else if (type.equals("int64")) {
                    str += " = " + defaultVal + "L;\n";
                } else if (type.equals("uint32")) {
                    str += " = " + defaultVal + "L;\n";
                } else {
                    str += " = " + defaultVal + ";\n";
                }
                // arrays
            } else if (list[i].isArray) {
                String typeName = getJavaObjectType(list[i], infos);

                if (list[i].length == -1) {
                    str += ws + "protected java.util.ArrayList<" + typeName + "> " + name
                            + " = new java.util.ArrayList<" + typeName + ">();\n";
                } else {
                    // for primitive arrays, use the primitive type name
                    if (!list[i].isStruct) {
                        typeName = getJavaType(list[i], infos);
                    }
                    if (defaultVal.length() == 0) {
                        // if no default val specified, use the natural defaults (0 for numbers, null for objects) and continue
                        str += ws + "protected " + typeName + "[] " + name + " = new " + typeName + "[" + list[i].length + "];\n";
                        continue;
                    }
                    str += ws + "protected " + typeName + "[] " + name + " = new " + typeName + "[]{ ";
                    for (int j = 0; j < list[i].length; j++) {
                        if (list[i].isStruct) {
                            if (defaultVal.equalsIgnoreCase("null") || type.equalsIgnoreCase(MDMInfo.LMCP_OBJECT_NAME)) {
                                str += "null, ";
                            } else {
                                str += "new " + typeName + "(), ";
                            }
                        } else if (type.equals("bool")) {
                            str += (defaultVal.equalsIgnoreCase("TRUE") ? "true" : "false") + ", ";
                        } else if (type.equals("byte")) {
                            str += "(byte)" + defaultVal + ", ";
                        } else if (type.equals("char")) {
                            str += "'" + defaultVal + "', ";
                        } else if (type.equals("int16")) {
                            str += "(short) " + defaultVal + ", ";
                        } else if (type.equals("real32")) {
                            str += "(float) " + defaultVal + ", ";
                        } else {
                            str += defaultVal + ", ";
                        }
                    }
                    str += "};\n";
                }
            }
        }
        return str;
    }

    public static String member_equals(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (FieldInfo f : st.fields) {
            if (f.isScalar) {
                if (f.isStruct || f.type.equals("string")) {
                    buf.append(ws + "if (" + f.name + " == null && o." + f.name + " != null) return false;\n");
                    buf.append(ws + "if ( " + f.name + "!= null && !" + f.name + ".equals(o." + f.name + ")) return false;\n");
                } else {
                    buf.append(ws + "if (" + f.name + " != o." + f.name + ") return false;\n");
                }
            } else {
                if (f.length == -1) {
                    buf.append(ws + " if (!" + f.name + ".equals( o." + f.name + ")) return false;\n");
                } else {
                    buf.append(ws + " if (!java.util.Arrays.equals(" + f.name + ", o." + f.name + ")) return false;\n");
                }
            }
        }
        return buf.toString();
    }

    public static String member_hashcode(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        int counter = 0;
        for (FieldInfo f : st.fields) {
            counter++;
            if (f.isScalar) {
                if (f.type.matches(NUMBER_MATCHER)) {
                    buf.append(ws + "hash += 31 * (int)" + f.name + ";\n");
                }
            }
            if (counter > 4) {
                break;
            }
        }
        return buf.toString();
    }

    public static String calc_size(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        int size = 0;
        String str = "";
        FieldInfo[] list = st.fields;

        for (int i = 0; i < list.length; i++) {
            String name = list[i].name;
            //scalar
            if (list[i].isScalar) {
                if (list[i].isStruct) {
                    str += ws + "size += LMCPUtil.sizeOf(" + name + ");\n";
                } else if (list[i].type.equals("string")) {
                    str += ws + "size += LMCPUtil.sizeOfString(" + name + ");\n";
                } else {
                    size += sizeOf(info, list[i]);
                }
            } else if (list[i].isArray) {
                // arrays
                if (list[i].length == -1) {
                    if (list[i].isStruct) {
                        str += ws + "size += " + (list[i].isLargeArray ? "4;\n" : "2;\n"); 
                        str += ws + "size += LMCPUtil.sizeOfList(" + name + ");\n";

                    } else if (list[i].type.equals("string")) {
                        str += ws + "size += 2;\n";
                        String tmpName = name + "_tmp";
                        str += ws + "for (String " + tmpName + " : " + name + ") { size+= LMCPUtil.sizeOfString(" + tmpName + "); }\n";
                    } else {
                        int len_size = list[i].isLargeArray ? 4 : 2;
                        str += ws + "\n" + ws + "size += " + len_size + " + " + String.valueOf(sizeOf(info, list[i])) + " * " + name + ".size();\n";
                    }
                } else {  // fixed length arrays
                    if (list[i].isStruct) {
                        str += ws + "size += LMCPUtil.sizeOfArray(" + name + ");\n";
                    } else if (list[i].type.equals("string")) {
                        String tmpName = name + "_tmp";
                        str += ws + "for (String " + tmpName + " : " + name + ") { size+= LMCPUtil.sizeOfString(" + tmpName + "); }\n";
                    } else {
                        size += list[i].length * sizeOf(info, list[i]);
                    }
                }
            }
        }

        String retStr = ws + "size += " + String.valueOf(size);
        retStr += "; // accounts for primitive types\n";
        retStr += str + "\n" + ws + "return ";
        retStr += "size;";

        return retStr;
    }

    public static String member_unpack(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        if (st.extends_name.length() != 0) {
            str += ws + "super.unpack(in);\n";
        }
        FieldInfo[] list = st.fields;

        for (int i = 0; i < st.fields.length; i++) {
            String type = getJavaType(list[i], infos);
            String name = list[i].name;
            
            if (!list[i].isStruct && !list[i].isEnum) {
                type = list[i].type.substring(0,1).toUpperCase() + list[i].type.substring(1);
            }

            // Scalar Types
            if (list[i].isScalar) {
                if (list[i].isStruct) {
                    str += ws + "    " + name + " = (" + type + ") LMCPUtil.getObject(in);\n";
                } else if (list[i].isEnum) {
                    str += ws + name + " = " + type + ".unpack( in );\n\n";
                } else {
                    str += ws + name + " = LMCPUtil.get" + type + "(in);\n\n";
                }
                // Array Types
            } else if (list[i].isArray) {
                // variable length arrays
                if (list[i].length == -1) {
                    str += ws + name + ".clear();\n";
                    if (list[i].isLargeArray) {
                        str += ws + "long " + name + "_len = LMCPUtil.getUint32(in);\n";
                    } else {
                        str += ws + "int " + name + "_len = LMCPUtil.getUint16(in);\n";
                    }
                    str += ws + "for(int i=0; i<" + name + "_len; i++){\n";
                    if (list[i].isStruct) {
                        str += ws + name + ".add( (" + type + ") LMCPUtil.getObject(in));\n";
                    } else if (list[i].isEnum) {
                        str += ws + name + ".add(" + type + ".unpack( in ));\n\n";
                    } else {
                        str += ws + "    " + name + ".add(" + "LMCPUtil.get" + type + "(in));\n";
                    }
                    str += ws + "}\n";

                } else {  // fixed length arrays
                    str += ws + "for(int i=0; i<" + name + ".length; i++){\n";
                    if (list[i].isStruct) {
                        str += ws + name + "[i] = (" + type + ") LMCPUtil.getObject(in);\n";
                    } else if (list[i].isEnum) {
                        str += ws + name + "[i] = " + type + ".unpack(in);\n\n";
                    } else {
                        str += ws + "    " + name + "[i] = " + "LMCPUtil.get" + type + "(in);\n";
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
            str += ws + "super.pack(out);\n";
        }

        // for each field in the message
        for (int i = 0; i < list.length; i++) {
            String type = getJavaType(list[i], infos);
            String name = list[i].name;
            
            if (!list[i].isStruct && !list[i].isEnum) {
                type = list[i].type.substring(0,1).toUpperCase() + list[i].type.substring(1);
            }

            //scalar types
            if (list[i].isScalar) {
                if (list[i].isStruct) {
                    str += ws + "LMCPUtil.putObject(out, " + name + ");\n";
                } else if (list[i].isEnum) {
                    str += ws + name + ".pack(out);\n";
                } else {
                    str += ws + "LMCPUtil.put" + type + "(out, " + name + ");\n";
                }
                // array types
            } else if (list[i].isArray) {
                int length = list[i].length;
                //variable length arrays
                if (length == -1) {
                    if ( list[i].isLargeArray) {
                        str += ws + "LMCPUtil.putUint32(out, " + name + ".size());\n";
                    } else {
                        str += ws + "LMCPUtil.putUint16(out, " + name + ".size());\n";
                    }
                    
                    str += ws + "for(int i=0; i<" + name + ".size(); i++){\n";
                    if (list[i].isStruct) {
                        str += ws + "    LMCPUtil.putObject(out, " + name + ".get(i));\n";
                    } else if (list[i].isEnum) {
                        str += ws + "    " + name + ".get(i).pack(out);\n";
                    } else {
                        
                        str += ws + "    LMCPUtil.put" + type + "(out, " + name + ".get(i));\n";
                    }
                    str += ws + "}\n";
                } else {
                    //fixed length arrays
                    str += ws + "for(int i=0; i<" + name + ".length; i++){\n";
                    if (list[i].isStruct) {
                        str += ws + "    LMCPUtil.putObject(out, " + name + "[i]);\n";
                    } else if (list[i].isEnum) {
                        str += ws + "    " + name + "[i].pack(out);\n";
                    } else {
                        str += ws + "    LMCPUtil.put" + type + "(out, " + name + "[i]);\n";
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
    
    public static String full_object_type_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.namespace.replaceAll("/", ".") + "." + st.name;
    }

    public static String object_type_number(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.id;
    }

    public static String to_xml(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        StructInfo dt_tmp = st;
        ArrayList<FieldInfo> fields = new ArrayList<FieldInfo>();
        fields.addAll(Arrays.asList(dt_tmp.fields));
        while (dt_tmp.extends_name.length() != 0) {
            dt_tmp = MDMInfo.getParentType(infos, dt_tmp);
            fields.addAll(Arrays.asList(dt_tmp.fields));
        }

        buf.append(ws + "buf.append( ws + \"<" + st.name + " Series=\\\"" + st.seriesName + "\\\">\\n\");\n");
        for (FieldInfo f : fields) {
            String name = f.name;
            if (f.isArray) {
                buf.append(ws + "buf.append( ws + \"  <" + name + ">\\n\");\n");
                if (f.length == -1) {
                    buf.append(ws + "for (int i=0; i<" + name + ".size(); i++) {\n");
                    if (f.isStruct) {
                        buf.append(ws + "    buf.append( " + name + ".get(i) == null ? ( ws + \"    <null/>\\n\") : (" + name + ".get(i).toXML(ws + \"    \")) + \"\\n\");\n");
                    } else {
                        buf.append(ws + "buf.append( ws + \"  <" + f.type + ">\" + String.valueOf(" + name + ".get(i)) + \"</" + f.type + ">\\n\");\n");
                    }
                } else {
                    buf.append(ws + "for (int i=0; i<" + name + ".length; i++) {\n");
                    if (f.isStruct) {
                        buf.append(ws + "    buf.append( " + name + "[i] == null ? ( ws + \"  <null/>\\n\") : (" + name + "[i].toXML(ws + \"  \")) + \"\\n\");\n");
                    } else {
                        buf.append(ws + "buf.append( ws + \"  <" + f.type + ">\" + String.valueOf(" + name + "[i]) + \"</" + f.type + ">\\n\");\n");
                    }
                }
                buf.append(ws + "}\n");
                buf.append(ws + "buf.append( ws + \"  </" + f.name + ">\\n\");\n");
            } else if (f.isStruct) {
                buf.append(ws + "if (" + name + "!= null){\n");
                buf.append(ws + "   buf.append( ws + \"  <" + f.name + ">\\n\");\n");
                buf.append(ws + "   buf.append( ( " + name + ".toXML(ws + \"    \")) + \"\\n\");\n");
                buf.append(ws + "   buf.append( ws + \"  </" + f.name + ">\\n\");\n");
                buf.append(ws + "}\n");
            } else {
                buf.append(ws + "buf.append( ws + \"  <" + f.name + ">\" + String.valueOf(" + name + ") + \"</" + f.name + ">\\n\");\n");
            }
        }

        buf.append(ws + "buf.append( ws + \"</" + st.name + ">\");\n");

        return buf.toString();
    }

    public static String send_all_messages(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            String ns = i.namespace.replaceAll("/", ".");
            for (StructInfo si : i.structs) {
                buf.append(ws + " o = new " + ns + "." + si.name + "();\n");
                buf.append(ws + " out.write(avtas.lmcp.LMCPFactory.packMessage(o, true));\n ");
            }
        }
        return buf.toString();
    }
    
    public static String series_enum_new_instance(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();
        for (StructInfo si : info.structs) {
            sb.append(ws).append("case ").append(si.id).append(": return new ").append(si.name).append("();\n");
        }
        return sb.toString();
    }
    
    public static String series_enum_get_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();
        for (StructInfo si : info.structs) {
            sb.append(ws).append("case ").append(si.id).append(": return \"").append(si.name).append("\";\n");
        }
        return sb.toString();
    }
    
    public static String series_enum_get_type(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();
        for (StructInfo si : info.structs) {
            sb.append(ws).append("if ( name.equals(\"").append(si.name).append("\")) return ").append(si.id).append(";\n");
        }
        return sb.toString();
    }

    /******************* Utility Methods ******************************/
    /** this returns the java type that is associated with the LMCP type */
    public static String getJavaType(FieldInfo f, MDMInfo[] infos) throws Exception {
        String type = f.type;
        if (type.equalsIgnoreCase("uint16")) {
            return "int";
        }
        if (type.equalsIgnoreCase("uint32")) {
            return "long";
        }
        if (type.equalsIgnoreCase("byte")) {
            return "short";
        }
        if (type.equalsIgnoreCase("bool")) {
            return "boolean";
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
            return "String";
        }
        if (type.equals(MDMInfo.LMCP_OBJECT_NAME)) {
            return "avtas.lmcp.LMCPObject";
        }
        // otherwise, its a class, so return the full classname
        return getPackageName(infos, f.seriesName) + "." + type.substring(0, 1).toUpperCase() + type.substring(1);
    }

    /** returns the Object type associated with the LMCP type passed */
    public static String getJavaObjectType(FieldInfo f, MDMInfo[] infos) throws Exception {
        String type = f.type;
        if (type.equalsIgnoreCase("uint16")) {
            return "Integer";
        }
        if (type.equalsIgnoreCase("uint32")) {
            return "Long";
        }
        if (type.equalsIgnoreCase("byte")) {
            return "Short";
        }
        if (type.equalsIgnoreCase("bool")) {
            return "Boolean";
        }
        if (type.equalsIgnoreCase("char")) {
            return "Character";
        }
        if (type.equalsIgnoreCase("int16")) {
            return "Short";
        }
        if (type.equalsIgnoreCase("int32")) {
            return "Integer";
        }
        if (type.equalsIgnoreCase("int64")) {
            return "Long";
        }
        if (type.equalsIgnoreCase("real32")) {
            return "Float";
        }
        if (type.equalsIgnoreCase("real64")) {
            return "Double";
        }
        if (type.equalsIgnoreCase("string")) {
            return "String";
        }
        if (type.equals(MDMInfo.LMCP_OBJECT_NAME)) {
            return "avtas.lmcp.LMCPObject";
        }
        // otherwise, its a class, so return the full classname
        return getPackageName(infos, f.seriesName) + "." + type.substring(0, 1).toUpperCase() + type.substring(1);
    }

    /** returns the size in bytes of the LMCP type */
    public static int sizeOf(MDMInfo info, FieldInfo field) throws Exception {
        if (field.isEnum) {
            return 4;
        } else {
            return sizeOf(field.type);
        }
    }

    /** returns the size in bytes of the LMCP type */
    public static int sizeOf(String type) throws Exception {
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

        throw new Exception("Invalid Struct");
    }

    public static String getPackageName(MDMInfo[] infos, String seriesName) throws Exception {
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            if (i.seriesName.equals(seriesName)) {
                return i.namespace.replaceAll("/", ".");
            }
        }
        throw new Exception("Unknown Series Name: " + seriesName);
    }
    
    public static String getJavaClassName(MDMInfo[] infos, String structName, String seriesName) throws Exception {
        String seriesPath = getPackageName(infos, seriesName);
        return seriesPath + "." + structName;
        
    }
}
