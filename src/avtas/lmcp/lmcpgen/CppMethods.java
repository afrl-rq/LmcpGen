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
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.Vector;

public class CppMethods {

    public static String series_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        if (info.seriesName.length() > 8) {
            throw new Exception("Error: Series name must be 8 characters or less.\n");
        }
        return ws + info.seriesName;
    }

    public static String series_name_caps(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        if (info.seriesName.length() > 8) {
            throw new Exception("Error: Series name must be 8 characters or less.\n");
        }
        return ws + info.seriesName.toUpperCase();
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

    public static String namespace(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return info.namespace.replaceAll("/", "::");
    }

    public static String root_filepath(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + getRootFilepath(info);
    }

    private static String getRootFilepath(MDMInfo info) {
        String str = "";
        String[] tmp = info.namespace.split("/");
        for (int i = 0; i < tmp.length; i++) {
            str += "../";
        }
        return str;
    }

    // Returns the date that the package was made
    public static String creation_date(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + DateFormat.getDateInstance(DateFormat.FULL).format(new Date());
    }

    public static String makefile_series_dirs(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer(ws);
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            buf.append("-I").append(i.namespace).append(" ");
        }
        return buf.toString();
    }
    
    public static String all_descendants(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String ret = "";
        List<String> descendants = new ArrayList<String>();
        add_descendants(infos, st.name, st.seriesName, descendants);
        for(String child : descendants) {
            ret += ws + "descendants.push_back(\"" + child + "\");\n";
        }
        return ret;
    }
    
    public static String all_descendants_include(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String ret = "";
        List<String> descendants = new ArrayList<String>();
        add_descendants(infos, st.name, st.seriesName, descendants);
        for(String child : descendants) {
            ret += ws + "#include \"" + child.replace('.','/') + ".h\"\n";
        }
        return ret;
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
            

    /*
    public static String namespace_dir(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
    return ws + info.namespace;
    }
     */
    public static String using_series_namespace(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            buf.append(ws + "using namespace " + namespace(infos, i, outfile, st, en, ws) + ";\n");
        }
        return buf.toString();
    }

    public static String namespace_caps(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        String[] tmp = info.namespace.split("/");
        for (int i = 0; i < tmp.length; i++) {
            str += tmp[i].toUpperCase() + (i != (tmp.length - 1) ? "_" : "");
        }
        return ws + str;
    }

    public static String open_namespace(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        String[] tmp = info.namespace.split("/");
        for (int i = 0; i < tmp.length; i++) {
            str += ws + "namespace " + tmp[i] + " {\n";
        }
        return str;
    }

    public static String close_namespace(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        String[] tmp = info.namespace.split("/");
        for (int i = tmp.length-1; i >=0 ; i--) {
            str += ws + "} // end namespace " + tmp[i] + "\n";
        }
        return str;
    }

    public static String datatype_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.name;
    }
    
    public static String longdatatype_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        String[] tmp = info.namespace.split("/");
        for (int i = 0; i < tmp.length; i++) {
            str += tmp[i];
        }
        return ws + str + st.name;
    }
    
    public static String longdatatype_name_dots(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        String[] tmp = info.namespace.split("/");
        for (int i = 0; i < tmp.length; i++) {
            str += tmp[i] + ".";
        }
        return ws + str + st.name;
    }

    public static String datatype_name_caps(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.name.toUpperCase();
    }

    public static String enum_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + en.name;
    }

    public static String enum_name_caps(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + en.name.toUpperCase();
    }

    public static String gen_enum_fields(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        int len = en.entries.size();
        for (int i = 0; i < len; i++) {
            EnumInfo.EnumEntry entry = en.entries.get(i);
            buf.append(ws + "/** " + entry.comment + " */\n");
            buf.append(ws + entry.name);
            buf.append(" = " + entry.value);
            if (i != len - 1) {
                buf.append(",\n");
            }
            else {
                buf.append("\n");
            }
        }
        return buf.toString();
    }

    public static String gen_enum_for_string(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (EnumInfo.EnumEntry entry : en.entries) {
            buf.append(ws + "if ( str == \"" + entry.name + "\") return " + entry.name + ";\n");
        }
        buf.append(ws + " return " + en.entries.get(0).name + ";\n");
        return buf.toString();
    }

    public static String gen_string_for_enum(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (EnumInfo.EnumEntry entry : en.entries) {
            buf.append(ws + "case " + entry.name + ": return \"" + entry.name + "\";\n");
        }
        buf.append(ws + "default: return \"" + en.entries.get(0).name + "\";\n");
        return buf.toString();
    }

    public static String datatype_id(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.id;
    }

    public static String parent_datatype_filepath(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + (st.extends_name.length() == 0 ? "avtas/lmcp/Object" : getSeriesFilepath(infos, st.extends_series) + st.extends_name);
    }

    public static String full_parent_datatype(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + (st.extends_name.length() == 0 ? "avtas::lmcp::Object" : getSeriesNamespace(infos, st.extends_series) + st.extends_name);
    }

    public static String include_dependencies(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < st.fields.length; i++) {
            String namespace = MDMReader.getMDM(st.fields[i].seriesName, infos).namespace;
            if (st.fields[i].isStruct && !st.fields[i].type.equals(MDMInfo.LMCP_OBJECT_NAME)) {
                str += ws + "#include \"" + namespace + "/" + st.fields[i].type + ".h\"\n";
            }
            else if (st.fields[i].isEnum) {
                str += ws + "#include \"" + namespace + "/" + st.fields[i].type + ".h\"\n";
            }
        }
        return str;
    }

    public static String include_sub_dependencies(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        Vector<StructInfo> subs = new Vector<StructInfo>();
        for (int i = 0; i < st.fields.length; i++) {
            if (st.fields[i].isStruct) {
                subs.addAll(getAllCppSubclasses(infos, st.fields[i]));
            }
        }
        StringBuffer buf = new StringBuffer();
        for (StructInfo i : subs) {
            buf.append("#include \"" + i.namespace + "/" + i.name + ".h\"\n");
        }
        return buf.toString();
    }

    public static String include_series_headers(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            buf.append(ws).append("#include " + i.namespace + "/" + i.seriesName.toLowerCase() + ".h\n");
        }
        return buf.toString();
    }

    public static String include_parent_type(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StructInfo parent = MDMInfo.getParentType(infos, st);
        if (parent != null) {
            return "#include \"" + parent.namespace + "/" + parent.name + ".h\"\n";
        }
        return "#include \"avtas/lmcp/Object.h\"\n";
    }

    public static String declare_attributes(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < st.fields.length; i++) {
            String name = "__" + st.fields[i].name;
            String type = getResolvedTypeName(infos, st.fields[i]);
            // Add field comment
            str += ws + "/**" + st.fields[i].comment.replaceAll("\\s+", " ").replaceAll("<br", "\n" + ws + "*<br") + "*/\n";
            // Scalar
            if (!st.fields[i].isArray) {
                str += ws + type + (st.fields[i].isStruct ? "* " : " ") + name + ";\n";
            } // Arrays
            else {
                //variable length
                if (st.fields[i].length == -1) {
                    if (st.fields[i].isStruct) {
                        str += ws + "std::vector< " + type + "* > " + name + ";\n";
                    }
                    else {
                        str += ws + "std::vector< " + type + " > " + name + ";\n";
                    }
                } //fixed length
                else {
                    if (st.fields[i].isStruct) {
                        str += ws + type + "* " + name + "[" + st.fields[i].length + "];\n";
                    }
                    else {
                        str += ws + type + " " + name + "[" + st.fields[i].length + "];\n";
                    }
                }
            }
        }
        return str;
    }

    public static String copy_attributes(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < st.fields.length; i++) {
            String name = "__" + st.fields[i].name;
            if (st.fields[i].isStruct && !st.fields[i].isArray) {
                str += ws + "newObj->" + name + " = " + name + "->clone();\n";
            }
            else {
                str += ws + "newObj->" + name + " = " + name + ";\n";
            }
        }
        return str;
    }

    public static String gets_and_sets_header(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < st.fields.length; i++) {
            String name = "__" + st.fields[i].name;
            String name2 = name.substring(2, 3).toUpperCase() + name.substring(3);
            String type = getResolvedTypeName(infos, st.fields[i]);
            // Add field comment
            str += ws + "/**" + st.fields[i].comment.replaceAll("\\s+", " ").replaceAll("<br", "\n" + ws + "*<br")
                    + "(Units: " + st.fields[i].units + ")*/\n";
            // Scalar
            if (!st.fields[i].isArray) {
                // get method
                str += ws + type + (st.fields[i].isStruct ? "* const" : "")
                        + " get" + name2 + "(void) " + (st.fields[i].isStruct ? "" : "const ") + "{ return " + name + "; }\n";
                // set method (declaration only)
                str += ws + st.name + "& set" + name2 + "(const " + type + (st.fields[i].isStruct ? "* const" : "") + " val);\n";
            } // Arrays
            else {
                // get method only
                if (st.fields[i].length == -1) {
                    if (st.fields[i].isStruct) {
                        str += ws + "std::vector<" + type + "*> & get" + name2 + "(void) { return " + name + "; }\n";
                    }
                    else {
                        str += ws + "std::vector<" + type + "> & get" + name2 + "(void) { return " + name + "; }\n";
                    }
                }
                else {
                    if (st.fields[i].isStruct) {
                        str += ws + type + "** get" + name2 + "(void) { return " + name + "; }\n";
                    }
                    else {
                        str += ws + type + "* get" + name2 + "(void) { return " + name + "; }\n";
                    }
                }
            }
            str += "\n";
        }
        return str;
    }

    public static String gets_and_sets_implementation(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < st.fields.length; i++) {
            String name = "__" + st.fields[i].name;
            String name2 = name.substring(2, 3).toUpperCase() + name.substring(3);
            String type = getResolvedTypeName(infos, st.fields[i]);
            // Scalar
            if (!st.fields[i].isArray) {
                // set method
                str += ws + st.name + "& " + st.name + "::set" + name2 + "(const " + type + (st.fields[i].isStruct ? "* const" : "") + " val)\n" + ws + "{\n";
                if (st.fields[i].isStruct) {
                    str += ws + "   if (" + name + " != nullptr) { delete " + name + "; " + name + " = nullptr; }\n";
                    str += ws + "   if (val != nullptr) { " + name + " = const_cast< " + type + "* > (val); }\n";
                }
                else {
                    str += ws + "   " + name + " = val;\n";
                }
                str += ws + "   return *this;\n";
                str += ws + "}\n";
            } // Arrays
            else {
                // no implemented methods
            }
            str += "\n";
        }
        return str;
    }

    public static String default_initializer_list(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < st.fields.length; i++) {
            String type = getResolvedTypeName(infos, st.fields[i]);
            String name = "__" + st.fields[i].name;
            String defaultVal = getCppDefaultVal(infos, st.fields[i]);
            // Scalar
            if (!st.fields[i].isArray) {
                str += ws + name + "(" + defaultVal + ");\n";
            } // Arrays
            else {
                // fixed-length only
                if (st.fields[i].length != -1) {
                    str += ws + name + "(" + type + "(" + st.fields[i].length + "));\n";
                }
            }
        }
        return str;
    }

    public static String initialize_attributes(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < st.fields.length; i++) {
            String name = "__" + st.fields[i].name;
            String type = getResolvedTypeName(infos, st.fields[i]);
            String defaultVal = getCppDefaultVal(infos, st.fields[i]);
            // Scalar
            if (!st.fields[i].isArray) {
                if (st.fields[i].isStruct) {
                    if (st.fields[i].defaultVal.equalsIgnoreCase("null")) {
                        str += ws + name + " = nullptr;\n";
                    }
                    else {
                        str += ws + name + " = new " + type + "();\n";
                    }
                }
                else {
                    str += ws + name + " = " + defaultVal + ";\n";
                }
            } // Arrays
            else {
                // Fixed-length
                if (st.fields[i].length != -1) {
                    str += ws + "for (uint32_t i=0; i<" + st.fields[i].length + "; i++)\n" + ws + "{\n";
                    if (st.fields[i].isStruct) {
                        if (st.fields[i].defaultVal.equalsIgnoreCase("null")) {
                            str += ws + "   " + name + "[i] = nullptr;\n";
                        }
                        else {
                            str += ws + "   " + name + "[i] = new " + type + "();\n";
                        }
                    }
                    else {
                        str += ws + "   " + name + "[i] = " + defaultVal + ";\n";
                    }
                    str += ws + "}\n";
                }
            }
        }
        return str;
    }

    public static String copy_initializer_list(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < st.fields.length; i++) {
            String type = getResolvedTypeName(infos, st.fields[i]);
            String name = "__" + st.fields[i].name;
            // Scalar
            if (!st.fields[i].isArray) {
                if (st.fields[i].isStruct) {
                    str += ws + name + " = that." + name + " == nullptr ? nullptr : that." + name + "->clone();\n";
                }
                else {
                    str += ws + name + " = that." + name + ";\n";
                }
            } // Arrays
            else {
                if (st.fields[i].length != -1) {
                    str += ws + "for (uint32_t i=0; i<" + st.fields[i].length + "; i++)\n" + ws + "{\n";
                    if (st.fields[i].isStruct) {
                        str += ws + "   " + name + "[i] = ( that." + name + " == nullptr ? nullptr : that." + name + "[i]->clone());\n";
                    }
                    else {
                        str += ws + "   " + name + "[i] = that." + name + "[i];\n";
                    }
                    str += ws + "}\n";
                }
                else {
                    str += ws + name + ".clear();\n";
                    str += ws + "for (size_t i=0; i< that." + name + ".size(); i++)\n" + ws + "{\n";
                    if (st.fields[i].isStruct) {
                        str += ws + "   " + name + ".push_back( that." + name + "[i] == nullptr ? nullptr : that." + name + "[i]->clone());\n";
                    }
                    else {
                        str += ws + "   " + name + ".push_back( that." + name + "[i]);\n";
                    }
                    str += ws + "}\n";
                }
                //str += ",\n" + ws + name + "(that." + name + ")";
            }
        }
        return str;
    }

    private static String cppObjectCompare(MDMInfo[] infos, FieldInfo field, String name, String ws) throws Exception {
        String str = "";
        str += ws + "if(" + name + " && that." + name + ")\n" + ws + "{\n";
        str += ws + "   if(" + name + "->getSeriesNameAsLong() != that." + name + "->getSeriesNameAsLong()) return false;\n";
        str += ws + "   if(" + name + "->getSeriesVersion() != that." + name + "->getSeriesVersion()) return false;\n";
        str += ws + "   if(" + name + "->getLmcpType() != that." + name + "->getLmcpType()) return false;\n";
        str += ws + "   if( *(" + name + ") != *(that." + name + ") ) return false;\n";
        str += ws + "}\n" + ws + "else if(" + name + " != that." + name + ") return false;\n";
        return str;
    }

    public static String equals_attributes(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < st.fields.length; i++) {
            String type = getResolvedTypeName(infos, st.fields[i]);
            String name = "__" + st.fields[i].name;
            // Scalar
            if (!st.fields[i].isArray) {
                if (st.fields[i].isStruct) {
                    str += cppObjectCompare(infos, st.fields[i], name, ws);
                }
                else {
                    str += ws + "if(" + name + " != that." + name + ") return false;\n";
                }
            } // Arrays
            else {
                if (st.fields[i].length != -1) {
                    str += ws + "for (uint32_t i=0; i<" + st.fields[i].length + "; i++)\n" + ws + "{\n";
                }
                else {
                    str += ws + "if(" + name + ".size() != that." + name + ".size()) return false;\n";
                    str += ws + "for (size_t i=0; i<" + name + ".size(); i++)\n" + ws + "{\n";
                }
                if (st.fields[i].isStruct) {
                    str += cppObjectCompare(infos, st.fields[i], name + "[i]", ws + "   ");
                }
                else {
                    str += ws + "   if(" + name + "[i] != that." + name + "[i]) return false;\n";
                }
                str += ws + "}\n";
            }
        }
        str += ws + "return true;\n";
        return str;
    }

    public static String assign_attributes(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < st.fields.length; i++) {
            String type = getResolvedTypeName(infos, st.fields[i]);
            String name = "__" + st.fields[i].name;
            String name2 = name.substring(2, 3).toUpperCase() + name.substring(3);
            // Scalar
            if (!st.fields[i].isArray) {
                str += ws + "set" + name2 + "(that." + name + ");\n";
            } // Arrays
            else {
                str += ws + name + " = that." + name + ";\n";
            }
        }
        return str;
    }

    public static String destroy_attributes(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < st.fields.length; i++) {
            String name = "__" + st.fields[i].name;
            // Scalar
            if (st.fields[i].isScalar && st.fields[i].isStruct) {
                str += ws + "if (" + name + " != nullptr) delete " + name + ";\n";
            } // Arrays
            else {
                if (st.fields[i].length == -1) {
                    if (st.fields[i].isStruct) {
                        str += ws + "for (size_t i=0; i<" + name + ".size(); i++)\n" + ws + "{\n";
                        str += ws + "   if (" + name + "[i] != nullptr) delete " + name + "[i];\n" + ws + "}\n";
                    }
                }
                else {
                    if (st.fields[i].isStruct) {
                        str += ws + "for (uint32_t i=0; i<" + st.fields[i].length + "; i++)\n" + ws + "{\n";
                        str += ws + "   if ( " + name + "[i] != nullptr ) delete " + name + "[i];\n" + ws + "}\n";
                    }
                }
            }
        }
        return str;
    }

    public static String pack_attributes(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < st.fields.length; i++) {
            String name = "__" + st.fields[i].name;
            String type = getCppTypeName(infos, st.fields[i]);
            String putName = "put" + getByteBufferTypeUpperCase(st.fields[i].type);
            // Scalar
            if (!st.fields[i].isArray) {
                // objects
                if (st.fields[i].isStruct) {
                    if (!st.fields[i].isOptional) {
                        str += ws + String.format("assert(%s != nullptr);\n", name);
                    }
                    str += ws + "avtas::lmcp::Factory::putObject( (avtas::lmcp::Object*) " + name + ", buf);\n";
                }
                else if (st.fields[i].isEnum) {
                    str += ws + "buf.putInt( (int32_t) " + name + ");\n";
                } // primitives
                else {
                    str += ws + "buf." + putName + "(" + name + ");\n";
                }
            } // Arrays
            else {
                if (st.fields[i].length == -1) {
                    if (st.fields[i].isLargeArray) {
                        str += ws + "buf.putUInt( static_cast<uint32_t>(" + name + ".size()));\n";
                    }
                    else {
                        str += ws + "buf.putUShort( static_cast<uint16_t>(" + name + ".size()));\n";
                    }
                    str += ws + "for (size_t i=0; i<" + name + ".size(); i++)\n" + ws + "{\n";
                }
                else {
                    str += ws + "for (uint32_t i=0; i<" + st.fields[i].length + "; i++)\n" + ws + "{\n";
                }
                if (st.fields[i].isStruct) {
                    str += ws + "   assert(" + name + "[i] != nullptr);\n";
                    str += ws + "   avtas::lmcp::Factory::putObject( (avtas::lmcp::Object*) " + name + "[i], buf);\n";
                }
                else if (st.fields[i].isEnum) {
                    str += ws + "   buf.putInt( (int32_t) " + name + "[i]);\n";
                }
                else {
                    str += ws + "   buf." + putName + "(" + name + "[i]);\n";
                }
                str += ws + "}\n";
            }
        }
        return str;
    }

    public static String unpack_attributes(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < st.fields.length; i++) {
            String name = "__" + st.fields[i].name;
            String type = getResolvedTypeName(infos, st.fields[i]);
            String getFunc = "get" + getByteBufferTypeUpperCase(st.fields[i].type);
            // Scalar
            if (!st.fields[i].isArray) {
                // objects
                if (st.fields[i].isStruct) {
                    str += ws + "{\n";
                    str += ws + "   if (" + name + " != nullptr) delete " + name + ";\n";
                    str += ws + "   " + name + " = nullptr;\n";
                    str += ws + "   if (buf.getBool())\n" + ws + "   {\n";
                    str += ws + "      int64_t series_id = buf.getLong();\n";
                    str += ws + "      uint32_t msgtype = buf.getUInt();\n";
                    str += ws + "      uint16_t version = buf.getUShort();\n";
                    str += ws + "      " + name + " = (" + type + "*) avtas::lmcp::Factory::createObject( series_id, msgtype, version );\n";
                    str += ws + "      if (" + name + " != nullptr) " + name + "->unpack(buf);\n";
                    if (!st.fields[i].isOptional) {
                        str += ws + "      else assert(" + name + " != nullptr);\n";
                    }
                    str += ws + "   }\n";
                    str += ws + "}\n";
                }
                else if (st.fields[i].isEnum) {
                    str += ws + name + " = (" + type + ") buf.getInt();\n";
                } // primitives
                else {
                    str += ws + name + " = buf." + getFunc + "();\n";
                }
            } // Arrays
            else {
                if (st.fields[i].length == -1) {
                    if (st.fields[i].isStruct) {
                        str += ws + "for (size_t i=0; i<" + name + ".size(); i++)\n" + ws + "{\n";
                        str += ws + "   if (" + name + "[i] != nullptr)\n";
                        str += ws + "      delete " + name + "[i];\n";
                        str += ws + "}\n";
                    }
                    str += ws + name + ".clear();\n";
                    if (st.fields[i].isLargeArray) {
                        str += ws + "uint32_t " + name + "_length = buf.getUInt();\n";
                    }
                    else {
                        str += ws + "uint16_t " + name + "_length = buf.getUShort();\n";
                    }
                    str += ws + "for (uint32_t i=0; i< " + name + "_length; i++)\n" + ws + "{\n";
                }
                else {
                    if (st.fields[i].isStruct) {
                        str += ws + "for (uint32_t i=0; i<" + st.fields[i].length + "; i++)\n" + ws +"{\n";
                        str += ws + "   if (" + name + "[i] != nullptr)\n";
                        str += ws + "      delete " + name + "[i];\n";
                        str += ws + "      " + name + "[i] = nullptr;\n";
                        str += ws + "}\n";
                    }
                    str += ws + "for (uint32_t i=0; i<" + st.fields[i].length + "; i++)\n" + ws +"{\n";
                }
                if (st.fields[i].isStruct) {
                    str += ws + "   if (buf.getBool())\n" + ws + "   {\n";
                    str += ws + "      int64_t series_id = buf.getLong();\n";
                    str += ws + "      uint32_t msgtype = buf.getUInt();\n";
                    str += ws + "      uint16_t version = buf.getUShort();\n";
                    str += ws + "      " + type + "* e = (" + type + "*) avtas::lmcp::Factory::createObject( series_id, msgtype, version );\n";
                    str += ws + "      if ( e != nullptr) e->unpack(buf); \n";
                    str += ws + "      else assert( e != nullptr); \n";
                    if (st.fields[i].length == -1) {
                        str += ws + "      " + name + ".push_back(e);\n";
                    }
                    else {
                        str += ws + "      " + name + "[i] = e;\n";
                    }
                    str += ws + "   }\n";
                }
                else if (st.fields[i].isEnum) {
                    if (st.fields[i].length == -1) {
                        str += ws + "   " + name + ".push_back( (" + type + ") buf.getInt() );\n";
                    }
                    else {
                        str += ws + "   " + name + "[i] = (" + type + ") buf.getInt();\n";
                    }
                }
                else {
                    if (st.fields[i].length == -1) {
                        str += ws + "   " + name + ".push_back(buf." + getFunc + "() );\n";
                    }
                    else {
                        str += ws + "   " + name + "[i] = buf." + getFunc + "();\n";
                    }
                }
                str += ws + "}\n";
            }
        }
        return str;
    }

    public static String tostring_attributes(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        if (st.extends_name.length() > 0) {
            str += tostring_attributes(infos, info, outfile, MDMInfo.getParentType(infos, st), en, ws) + "\n";
        }
        for (int i = 0; i < st.fields.length; i++) {
            String name = "__" + st.fields[i].name;
            String type = getCppTypeName(infos, st.fields[i]);

            // Scalar
            if (!st.fields[i].isArray) {
                // objects
                if (st.fields[i].isStruct) {
                    str += ws + "oss << indent << \"" + st.fields[i].name + " (" + type + ")\";\n";
                    str += ws + "if (" + name + " == nullptr)\n";
                    str += ws + "   oss << \" = nullptr\";\n";
                    str += ws + "oss << \"\\n\";\n";
                } // primitives
                else {
                    str += ws + "oss << indent << \"" + st.fields[i].name + " (" + type + ") = \" << ";

                    if (type.equalsIgnoreCase("string")) {
                        str += "\"\\\"\" << " + name + "<< \"\\\"\"";
                    }
                    else if (type.equalsIgnoreCase("char")) {
                        str += "\"\\\'\" << " + name + "<< \"\\\'\"";
                    }
                    else if (type.equalsIgnoreCase("byte")) {
                        str += "(int32_t)" + name;
                    }
                    else {
                        str += name;
                    }

                    str += " << \"\\n\";\n";
                }
            } // Arrays
            else {
                if (st.fields[i].length == -1) {
                    str += ws + "oss << indent << \"" + st.fields[i].name + " (" + st.fields[i].type + " [ \" << " + name + ".size() << \", var ])\\n\";\n";
                }
                else {
                    str += ws + "oss << indent << \"" + st.fields[i].name + " (" + st.fields[i].type + " [ \" << " + name + " << \" ])\\n\";\n";
                }
            }
        }
        return str;
    }

    public static String calculate_packed_size(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < st.fields.length; i++) {
            String name = "__" + st.fields[i].name;
            String type = getResolvedTypeName(infos, st.fields[i]);
            // Scalar
            if (!st.fields[i].isArray) {
                // objects
                if (st.fields[i].isStruct) {
                    // 15 = boolean (1 byte), series name (8 bytes), type (4 bytes) , version number (2 bytes)
                    str += ws + "size += (" + name + " != nullptr ? " + name + "->calculatePackedSize() + 15 : 1);\n";
                } // primitives
                else {
                    if (st.fields[i].type.equalsIgnoreCase("string")) {
                        str += ws + "size += 2 + " + name + ".length();\n";
                    }
                    else {
                        str += ws + "size += sizeof(" + type + ");\n";
                    }
                }
            } // Arrays
            else {
                if (st.fields[i].length == -1) {
                    if (st.fields[i].isStruct) {
                        if (st.fields[i].isLargeArray) {
                            str += ws + "size += 4;\n"; // space for uint to indicate length
                        }
                        else {
                            str += ws + "size += 2;\n"; // space for ushort to indicate length
                        }
                        str += ws + "for (size_t i=0; i<" + name + ".size(); i++)\n" + ws + "{\n";
                        str += ws + "   if (" + name + "[i] != nullptr)\n" + ws + "   {\n";
                        // 15 = boolean (1 byte), series name (8 bytes), type (4 bytes) , version number (2 bytes)
                        str += ws + "      size += " + name + "[i]->calculatePackedSize() + 15;\n";
                        str += ws + "   }\n";
                        str += ws + "   else { size += 1; }\n";
                        str += ws + "}\n";
                    }
                    else if (st.fields[i].type.equals("string")) {
                        if (st.fields[i].isLargeArray) {
                            str += ws + "size += 4;\n"; // space for uint to indicate length
                        }
                        else {
                            str += ws + "size += 2;\n"; // space for ushort to indicate length
                        }
                        str += ws + "for (size_t i=0; i<" + name + ".size(); i++)\n" + ws + "{\n";
                        // get the length of the string + 2 bytes (placeholder for length)
                        str += ws + "   size += " + name + "[i].length() + 2;\n";
                        str += ws + "}\n";
                    }
                    else {
                        if (st.fields[i].isLargeArray) {
                            str += ws + "size += 4 + sizeof(" + type + ") * " + name + ".size();\n";
                        }
                        else {
                            str += ws + "size += 2 + sizeof(" + type + ") * " + name + ".size();\n";
                        }
                    }
                }
                else {
                    //str += ws + "size += 2;\n"; // space for ushort to indicate length
                    if (st.fields[i].isStruct) {
                        str += ws + "for (uint32_t i=0; i<" + st.fields[i].length + "; i++)\n" + ws +"{\n";
                        str += ws + "   if (" + name + "[i] != nullptr){\n;";
                        // 15 = boolean (1 byte), series name (8 bytes), type (4 bytes) , version number (2 bytes)
                        str += ws + "      size += " + name + "[i]->calculatePackedSize() + 15;\n";
                        str += ws + "   }\n" + ws + "   else { size += 1; }\n";
                        str += ws + "}\n";
                    }
                    else if (st.fields[i].type.equals("string")) {
                        str += ws + "for (uint32_t i=0; i<" + st.fields[i].length + "; i++)\n" + ws +"{\n";
                        // get the length of the string + 2 bytes (placeholder for length)
                        str += ws + "   size += " + name + "[i].length() + 2;\n";
                        str += ws + "}\n";
                    }
                    else {
                        str += ws + "size += sizeof(" + type + ") * " + st.fields[i].length + ";\n";
                    }
                }
            }
        }
        return str;
    }

    public static String include_all_types(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            for (StructInfo s : i.structs) {
                buf.append(ws + "#include \"" + i.namespace + "/" + s.name + ".h\"\n");
            }
            buf.append(ws + "#include \"" + i.namespace + "/" + i.seriesName + "Enum.h\"\n");
        }
        return buf.toString();
    }

    public static String series_factory_switch(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            buf.append(ws + "if (series_id == " + i.seriesNameAsLong + "LL)\n");
            buf.append(ws + "   if (version == " + i.version + ")\n");
            buf.append(ws + "      switch(type)\n" + ws + "      {\n");
            for (int j = 0; j < i.structs.length; j++) {
                buf.append(ws + "         case " + i.structs[j].id + ": return new " + namespace(infos, i, outfile, st, en, ws)
                        + "::" + i.structs[j].name + "; \n");
            }
            buf.append(ws + "      }\n");
        }
        return buf.toString();
    }

    public static String series_enumeration(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < info.structs.length; i++) {
            str += ws + info.structs[i].name.toUpperCase() + " = " + info.structs[i].id + ",\n";
        }
        return str.replaceAll(",\n$", "");
    }

    public static String include_array_dependencies(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < st.fields.length; i++) {
            if (st.fields[i].isArray && st.fields[i].length == -1) {
                str += ws + "#include <vector>\n";
                return str;
            }
        }
        return str;
    }

    public static String makefile_source_list(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            for (StructInfo si : i.structs) {
                String cleanNamespace = i.namespace.replaceAll("/","");
                buf.append("\t" + i.namespace + "/" + cleanNamespace + si.name + ".cpp\\\n");
            }
            buf.append("\t" + i.namespace + "/" + i.seriesName + "XMLReader.cpp\\\n");
        }
        return buf.toString();
    }
    
    public static String meson_source_list(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            for (StructInfo si : i.structs) {
                String cleanNamespace = i.namespace.replaceAll("/","");
                buf.append(ws + "'" + i.namespace + "/" + cleanNamespace + si.name + ".cpp',\n");
            }
            buf.append(ws + "'" + i.namespace + "/" + i.seriesName + "XMLReader.cpp',\n");
        }
        return buf.toString();
    }

    public static String include_all_series_headers(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < info.structs.length; i++) {
            str += ws + "#include \"" + info.namespace + "/" + info.structs[i].name + ".h\"\n";
        }
        for (int i = 0; i < info.enums.length; i++) {
            str += ws + "#include \"" + info.namespace + "/" + info.enums[i].name + ".h\"\n";
        }
        str += ws + "#include \"" + info.namespace + "/" + info.seriesName + "Enum.h\"\n";
        return str;
    }

    public static String include_entire_series(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < info.structs.length; i++) {
            str += ws + "#include \"" + info.namespace + "/" + info.structs[i].name + ".h\"\n";
        }
        for (int i = 0; i < info.enums.length; i++) {
            str += ws + "#include \"" + info.namespace + "/" + info.enums[i].name + ".h\"\n";
        }
        str += ws + "#include \"" + info.namespace + "/" + info.seriesName + "Enum.h\"\n";
        return str;
    }

    public static String include_every_series(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            str += ws + "#include \"" + i.namespace + "/" + i.seriesName + ".h\"\n";
        }
        return str;
    }

    public static String create_test_objects(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        for (int i = 0; i < info.structs.length; i++) {
            String type = namespace(infos, info, outfile, st, en, ws) + "::" + info.structs[i].name;
            String name = "obj" + info.structs[i].name.substring(0, 1).toUpperCase() + info.structs[i].name.substring(1);
            str += ws + type + "* " + name + " = new " + type + ";\n";
            str += ws + "objects.push_back(" + name + ");\n";
        }
        return str;
    }

    public static String xml_visit_series(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            String seriesFactory = getCppNamespace(i.namespace) + "::SeriesXMLReader";
            buf.append(ws + "if (node->getAttribute(\"Series\") == \"" + i.seriesName + "\") return " + seriesFactory + "::visitType(node);\n");
        }
        return buf.toString();
    }

    public static String xml_include_factories(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            buf.append(ws + "#include \"" + i.namespace + "/" + i.seriesName + "XMLReader.h\"\n");
        }
        return buf.toString();
    }

    public static String xml_create_visits(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";

        for (int i = 0; i < info.structs.length; i++) {
            StructInfo dt_tmp = info.structs[i];
            str += ws + "if (type == \"" + dt_tmp.name + "\"){\n";
            str += ws + "   " + dt_tmp.name + "* o = new " + dt_tmp.name + "();\n";
            str += ws + "   for (uint32_t i=0; i<el->getChildCount(); i++)\n" + ws + "   {\n";
            str += ws + "      std::string name = el->getChild(i)->getTagName();\n";

            ArrayList<FieldInfo> fields = new ArrayList<FieldInfo>();
            fields.addAll(Arrays.asList(dt_tmp.fields));
            while (dt_tmp.extends_name.length() != 0) {
                dt_tmp = MDMInfo.getParentType(infos, dt_tmp);
                fields.addAll(Arrays.asList(dt_tmp.fields));
            }

            for (int j = 0; j < fields.size(); j++) {
                FieldInfo f = fields.get(j);
                String setname = f.name.substring(0, 1).toUpperCase() + f.name.substring(1);
                String ctype = getResolvedTypeName(infos, f);
                str += ws + "      if(name == \"" + f.name + "\")\n" + ws + "      {\n";
                str += ws + "         Node* tmp = el->getChild(i);\n";
                if (f.isArray) {
                    str += ws + "         for (uint32_t j=0; j<tmp->getChildCount(); j++)\n" + ws + "         {\n";
                    if (f.length == -1) {
                        if (f.isStruct) {
                            str += ws + "            Object* oo = readXML( tmp->getChild(j));\n";
                            str += ws + "            o->get" + setname + "().push_back( (" + ctype + "*) oo);\n";
                            //str += ws + "                delete oo;\n";
                        }
                        else if (f.isEnum) {
                            String enumspace = getSeriesNamespace(infos, f.seriesName) + f.type;
                            str += ws + "            o->get" + setname + "().push_back( " + enumspace + "::get_" + f.type + "(get_string( tmp->getChild(j))));\n";
                        }
                        else {
                            str += ws + "            o->get" + setname + "().push_back( get_" + f.type + "( tmp->getChild(j)));\n";
                        }
                    }
                    else {
                        if (f.isStruct) {
                            str += ws + "            Object* oo = readXML( tmp->getChild(j));\n";
                            str += ws + "            o->get" + setname + "()[j] = (" + ctype + "*) oo;\n";
                            //str += ws + "                delete oo;\n";
                        }
                        else if (f.isEnum) {
                            String enumspace = getSeriesNamespace(infos, f.seriesName) + f.type;
                            str += ws + "            o->get" + setname + "()[j] = " + enumspace + "::get_" + f.type + "( get_string( tmp->getChild(j)));\n";
                        }
                        else {
                            str += ws + "            o->get" + setname + "()[j] = get_" + f.type + "( tmp->getChild(j));\n";
                        }
                    }
                    str += ws + "         }\n";
                }
                else {
                    if (f.isStruct) {
                        str += ws + "         Object* oo = readXML( tmp->getChild(0) );\n";
                        str += ws + "         o->set" + setname + "((" + ctype + "*) oo );\n";
                        //str += ws + "            delete oo;\n";
                    }
                    else if (f.isEnum) {
                        String enumspace = getSeriesNamespace(infos, f.seriesName) + f.type;
                        str += ws + "         o->set" + setname + "( " + enumspace + "::get_" + f.type + "( get_string( tmp )));\n";
                    }
                    else {
                        str += ws + "         o->set" + setname + "( get_" + f.type + "( tmp ));\n";
                    }
                }
                str += ws + "         continue;\n";
                str += ws + "      }\n";
            }

            str += ws + "   }\n";
            str += ws + "   return o;\n";
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
        str += ws + "str << ws << \"<" + st.name + " Series=\\\"" + st.seriesName + "\\\">\\n\";\n";

        for (FieldInfo f : fields) {
            String varname = "__" + f.name;
            String cast = "";
            if (f.type.equals("byte")) {
                cast = "(int32_t) ";
            }

            if (f.isArray) {
                str += ws + "str << ws << \"   <" + f.name + ">\\n\";\n";

                if (f.length == -1) {
                    str += ws + "for (size_t i=0; i<" + varname + ".size(); i++)\n" + ws + "{\n";
                }
                else {
                    str += ws + "for (uint32_t i=0; i<" + f.length + "; i++)\n" + ws +"{\n";
                }
                if (f.isStruct) {
                    str += ws + "   str << (" + varname + "[i] == nullptr ? ( ws + \"   <null/>\\n\") : (" + varname + "[i]->toXML(depth + 1))) ;\n";
                }
                else if (f.isEnum) {
                    String fulltype = getCppNamespace(getSeriesNamespace(infos, f.seriesName)) + f.type;
                    str += ws + "   str << ws << \"   <" + f.type + ">\" << " + fulltype + "::get_string(" + varname + "[i]) << \"</" + f.type + ">\\n\";\n";
                }
                else {
                    if (f.type.equals("bool")) {
                        str += ws + "   str << ws << \"   <" + f.type + ">\" << (" + varname + "[i] ? \"true\" : \"false\") << \"</" + f.type + ">\\n\";\n";
                    }
                    else {
                        str += ws + "   str << ws << \"   <" + f.type + ">\" << " + cast + varname + "[i] << \"</" + f.type + ">\\n\";\n";
                    }
                }
                str += ws + "}\n";
                str += ws + "str << ws << \"   </" + f.name + ">\\n\";\n";
            }
            else if (f.isStruct) {
                str += ws + "if (" + varname + " != nullptr)\n" + ws + "{\n";
                str += ws + "   str << ws << \"   <" + f.name + ">\";\n";
                str += ws + "   str << \"\\n\" + " + varname + "->toXML(depth + 1) + ws + \"   \";\n";
                str += ws + "   str << \"</" + f.name + ">\\n\";\n";
                str += ws + "}\n";
            }
            else if (f.isEnum) {
                String fulltype = getCppNamespace(getSeriesNamespace(infos, f.seriesName)) + f.type;
                str += ws + "str << ws << \"   <" + f.name + ">\" << " + fulltype + "::get_string(" + varname + ") << \"</" + f.name + ">\\n\";\n";
            }
            else {
                if (f.type.equals("bool")) {
                    str += ws + "str << ws << \"   <" + f.name + ">\" << (" + varname + " ? \"true\" : \"false\") << \"</" + f.name + ">\\n\";\n";
                } else {
                    str += ws + "str << ws << \"   <" + f.name + ">\" << " + cast + varname + " << \"</" + f.name + ">\\n\";\n";
                }
            }
        }

        str += ws + "str << ws << \"</" + st.name + ">\\n\";\n";

        return str;
    }

    public static String send_all_messages(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = ws + "// send out all of messages\n";
        str += ws + "avtas::lmcp::ByteBuffer* sendBuf = nullptr;\n";
        str += ws + "uint8_t* pBuf = nullptr;\n";

        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            for (StructInfo s : i.structs) {
                String ns = s.namespace.replaceAll("/", "::") + "::" + s.name;
                String cn = s.namespace.replaceAll("/", "") + s.name;
                str += "\n";
                str += ws + ns + "* _" + cn.toLowerCase() + " = new " + ns + "();\n";
                str += ws + "std::cout << \"Sending " + ns + "\" << std::endl;\n";
                str += ws + "sendBuf = avtas::lmcp::Factory::packMessage(_" + cn.toLowerCase() + ", true);\n";
                str += ws + "delete _" + cn.toLowerCase() + ";\n";
                str += ws + "std::cout << sendBuf->toString() << std::endl;\n";
                str += ws + "pBuf = sendBuf->array();\n";
                str += ws + "send(connectionSocket, (char*) pBuf, sendBuf->position(), 0);\n";
                str += ws + "delete sendBuf;\n";
            }
        }
        return str;
    }

    public static String project_header_files(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        // lmcp specific headers
        String str = ws + "<Filter Name=\"lmcp\">\n";
        str += ws + "   <File RelativePath=\".\\avtas\\lmcp\\ByteBuffer.h\"> </File>\n";
        str += ws + "   <File RelativePath=\".\\avtas\\lmcp\\Factory.h\"> </File>\n";
        str += ws + "   <File RelativePath=\".\\avtas\\lmcp\\LmcpXMLReader.h\"> </File>\n";
        str += ws + "   <File RelativePath=\".\\avtas\\lmcp\\Node.h\"> </File>\n";
        str += ws + "   <File RelativePath=\".\\avtas\\lmcp\\NodeUtil.h\"> </File>\n";
        str += ws + "   <File RelativePath=\".\\avtas\\lmcp\\Object.h\"> </File>\n";
        str += ws + "   <File RelativePath=\".\\avtas\\lmcp\\XMLParser.h\"> </File>\n";
        str += ws + "</Filter>\n";

        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            String winDir = i.namespace.replaceAll("/", "\\\\");
            str += ws + "<Filter Name=\"" + winDir + "\">\n";
            for (StructInfo s : i.structs) {
                str += ws + "   <File RelativePath=\".\\" + winDir + "\\" + s.name + ".h\"> </File>\n";
            }
            for (EnumInfo e : i.enums) {
                str += ws + "   <File RelativePath=\".\\" + winDir + "\\" + e.name + ".h\"> </File>\n";
            }
            str += ws + "   <File RelativePath=\".\\" + winDir + "\\" + i.seriesName + "XMLReader.h\"> </File>\n";
            str += ws + "   <File RelativePath=\".\\" + winDir + "\\" + i.seriesName + "Enum.h\"> </File>\n";
            str += ws + "   <File RelativePath=\".\\" + winDir + "\\" + i.seriesName + ".h\"> </File>\n";
            str += ws + "</Filter>\n";
        }
        return str;
    }

    public static String project_source_files(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = ws + "<Filter Name=\"lmcp\">\n";
        str += ws + "   <File RelativePath=\".\\avtas\\lmcp\\ByteBuffer.cpp\"> </File>\n";
        str += ws + "   <File RelativePath=\".\\avtas\\lmcp\\Factory.cpp\"> </File>\n";
        str += ws + "   <File RelativePath=\".\\avtas\\lmcp\\Node.cpp\"> </File>\n";
        str += ws + "   <File RelativePath=\".\\avtas\\lmcp\\NodeUtil.cpp\"> </File>\n";
        str += ws + "   <File RelativePath=\".\\avtas\\lmcp\\XMLParser.cpp\"> </File>\n";
        str += ws + "</Filter>\n";

        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            String winDir = i.namespace.replaceAll("/", "\\\\");
            String cleanNamespace = i.namespace.replaceAll("/","");
            str += ws + "<Filter Name=\"" + winDir + "\">\n";
            for (StructInfo s : i.structs) {
                str += ws + "   <File RelativePath=\".\\" + winDir + "\\" + cleanNamespace + s.name + ".cpp\"> </File>\n";
            }
            str += ws + "   <File RelativePath=\".\\" + winDir + "\\" + i.seriesName + "XMLReader.cpp\"> </File>\n";
            str += ws + "</Filter>\n";
        }
        return str;
    }

    public static String xproject_header_files(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        // lmcp specific headers
        String str = ws + "";
        str += ws + "   <ClInclude Include=\"avtas\\lmcp\\ByteBuffer.h\"/>\n";
        str += ws + "   <ClInclude Include=\"avtas\\lmcp\\Factory.h\"/>\n";
        str += ws + "   <ClInclude Include=\"avtas\\lmcp\\LmcpXMLReader.h\"/>\n";
        str += ws + "   <ClInclude Include=\"avtas\\lmcp\\Node.h\"/>\n";
        str += ws + "   <ClInclude Include=\"avtas\\lmcp\\NodeUtil.h\"/>\n";
        str += ws + "   <ClInclude Include=\"avtas\\lmcp\\Object.h\"/>\n";
        str += ws + "   <ClInclude Include=\"avtas\\lmcp\\XMLParser.h\"/>\n";

        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            String winDir = i.namespace.replaceAll("/", "\\\\");
            for (StructInfo s : i.structs) {
                str += ws + "   <ClInclude Include=\"" + winDir + "\\" + s.name + ".h\"/>\n";
            }
            for (EnumInfo e : i.enums) {
                str += ws + "   <ClInclude Include=\"" + winDir + "\\" + e.name + ".h\"/>\n";
            }
            str += ws + "   <ClInclude Include=\"" + winDir + "\\" + i.seriesName + "XMLReader.h\"/>\n";
            str += ws + "   <ClInclude Include=\"" + winDir + "\\" + i.seriesName + "Enum.h\"/>\n";
            str += ws + "   <ClInclude Include=\"" + winDir + "\\" + i.seriesName + ".h\"/>\n";
        }
        return str;
    }

    public static String xproject_source_files(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        str += ws + "   <ClCompile Include=\"avtas\\lmcp\\ByteBuffer.cpp\"/>\n";
        str += ws + "   <ClCompile Include=\"avtas\\lmcp\\Factory.cpp\"/>\n";
        str += ws + "   <ClCompile Include=\"avtas\\lmcp\\Node.cpp\"/>\n";
        str += ws + "   <ClCompile Include=\"avtas\\lmcp\\NodeUtil.cpp\"/>\n";
        str += ws + "   <ClCompile Include=\"avtas\\lmcp\\XMLParser.cpp\"/>\n";

        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            String winDir = i.namespace.replaceAll("/", "\\\\");
            String cleanNamespace = i.namespace.replaceAll("/","");
            for (StructInfo s : i.structs) {
                str += ws + "   <ClCompile Include=\"" + winDir + "\\" + cleanNamespace + s.name + ".cpp\"/>\n";
            }
            str += ws + "   <ClCompile Include=\"" + winDir + "\\" + i.seriesName + "XMLReader.cpp\"/>\n";
        }
        return str;
    }

    public static String xproject_setup_filters(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        List<String> namespaces = new ArrayList<String>();
        namespaces.add("lmcp");
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            namespaces.add(i.namespace.replaceAll("/", "%255c"));
        }
        for (String namespace : namespaces) {
            str += ws + "<Filter Include=\"Header Files\\" + namespace + "\">\n";
            str += ws + " <UniqueIdentifier>{" + makeGUID() + "}</UniqueIdentifier>\n";
            str += ws + "</Filter>\n";
            str += ws + "<Filter Include=\"Source Files\\" + namespace + "\">\n";
            str += ws + " <UniqueIdentifier>{" + makeGUID() + "}</UniqueIdentifier>\n";
            str += ws + "</Filter>\n";
        }

        return str;
    }

    public static String xproject_header_filters(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";

        str += ws + "   <ClInclude Include=\"avtas\\lmcp\\ByteBuffer.h\">\n";
        str += ws + "   <Filter>Header Files\\lmcp</Filter>\n</ClInclude>\n";
        str += ws + "   <ClInclude Include=\"avtas\\lmcp\\Factory.h\">\n";
        str += ws + "   <Filter>Header Files\\lmcp</Filter>\n</ClInclude>\n";
        str += ws + "   <ClInclude Include=\"avtas\\lmcp\\LmcpXMLReader.h\">\n";
        str += ws + "   <Filter>Header Files\\lmcp</Filter>\n</ClInclude>\n";
        str += ws + "   <ClInclude Include=\"avtas\\lmcp\\Node.h\">\n";
        str += ws + "   <Filter>Header Files\\lmcp</Filter>\n</ClInclude>\n";
        str += ws + "   <ClInclude Include=\"avtas\\lmcp\\NodeUtil.h\">\n";
        str += ws + "   <Filter>Header Files\\lmcp</Filter>\n</ClInclude>\n";
        str += ws + "   <ClInclude Include=\"avtas\\lmcp\\Object.h\">\n";
        str += ws + "   <Filter>Header Files\\lmcp</Filter>\n</ClInclude>\n";
        str += ws + "   <ClInclude Include=\"avtas\\lmcp\\XMLParser.h\">\n";
        str += ws + "   <Filter>Header Files\\lmcp</Filter>\n</ClInclude>\n";

        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            String filter = "Header Files\\" + i.namespace.replaceAll("/", "%255c");
            String folder = i.namespace.replaceAll("/", "\\\\");
            for (StructInfo s : i.structs) {
                str += ws + "   <ClInclude Include=\"" + folder + "\\" + s.name + ".h\">\n <Filter>" + filter + "</Filter>\n</ClInclude>\n";
            }
            for (EnumInfo e : i.enums) {
                str += ws + "   <ClInclude Include=\"" + folder + "\\" + e.name + ".h\">\n <Filter>" + filter + "</Filter>\n</ClInclude>\n";
            }
            str += ws + "   <ClInclude Include=\"" + folder + "\\" + i.seriesName + "XMLReader.h\">\n <Filter>" + filter + "</Filter>\n</ClInclude>\n";
            str += ws + "   <ClInclude Include=\"" + folder + "\\" + i.seriesName + "Enum.h\">\n <Filter>" + filter + "</Filter>\n</ClInclude>\n";
            str += ws + "   <ClInclude Include=\"" + folder + "\\" + i.seriesName + ".h\">\n <Filter>" + filter + "</Filter>\n</ClInclude>\n";
        }
        return str;
    }

    public static String xproject_source_filters(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";

        str += ws + "   <ClCompile Include=\"avtas\\lmcp\\ByteBuffer.cpp\">\n";
        str += ws + "   <Filter>Source Files\\lmcp</Filter>\n</ClCompile>\n";
        str += ws + "   <ClCompile Include=\"avtas\\lmcp\\Factory.cpp\">\n";
        str += ws + "   <Filter>Source Files\\lmcp</Filter>\n</ClCompile>\n";
        str += ws + "   <ClCompile Include=\"avtas\\lmcp\\LmcpXMLReader.cpp\">\n";
        str += ws + "   <Filter>Source Files\\lmcp</Filter>\n</ClCompile>\n";
        str += ws + "   <ClCompile Include=\"avtas\\lmcp\\Node.cpp\">\n";
        str += ws + "   <Filter>Source Files\\lmcp</Filter>\n</ClCompile>\n";
        str += ws + "   <ClCompile Include=\"avtas\\lmcp\\NodeUtil.cpp\">\n";
        str += ws + "   <Filter>Source Files\\lmcp</Filter>\n</ClCompile>\n";
        str += ws + "   <ClCompile Include=\"avtas\\lmcp\\Object.cpp\">\n";
        str += ws + "   <Filter>Source Files\\lmcp</Filter>\n</ClCompile>\n";
        str += ws + "   <ClCompile Include=\"avtas\\lmcp\\XMLParser.cpp\">\n";
        str += ws + "   <Filter>Source Files\\lmcp</Filter>\n</ClCompile>\n";

        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            String filter = "Source Files\\" + i.namespace.replaceAll("/", "%255c");
            String folder = i.namespace.replaceAll("/", "\\\\");
            String cleanNamespace = i.namespace.replaceAll("/","");

            for (StructInfo s : i.structs) {
                str += ws + "   <ClCompile Include=\"" + folder + "\\" + cleanNamespace + s.name + ".cpp\">\n <Filter>" + filter + "</Filter>\n</ClCompile>\n";
            }

            str += ws + "   <ClCompile Include=\"" + folder + "\\" + i.seriesName + "XMLReader.cpp\">\n <Filter>" + filter + "</Filter>\n</ClCompile>\n";
            str += ws + "   <ClCompile Include=\"" + folder + "\\" + i.seriesName + "Enum.cpp\">\n <Filter>" + filter + "</Filter>\n</ClCompile>\n";
            str += ws + "   <ClCompile Include=\"" + folder + "\\" + i.seriesName + ".cpp\">\n <Filter>" + filter + "</Filter>\n</ClCompile>\n";
        }
        return str;
    }

    public static String pro_source_files(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = ws + ".\\avtas\\lmcp\\ByteBuffer.cpp\\\n";
        str += ws + "\t.\\avtas\\lmcp\\Factory.cpp\\\n";
        str += ws + "\t.\\avtas\\lmcp\\Node.cpp\\\n";
        str += ws + "\t.\\avtas\\lmcp\\NodeUtil.cpp\\\n";
        str += ws + "\t.\\avtas\\lmcp\\XMLParser.cpp\\\n";

        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            String winDir = i.namespace.replaceAll("/", "\\\\");
            String cleanNamespace = i.namespace.replaceAll("/","");
            for (StructInfo s : i.structs) {
                str += ws + "\t.\\" + winDir + "\\" + cleanNamespace + s.name + ".cpp\\\n";
            }
            str += ws + "\t.\\" + winDir + "\\" + i.seriesName + "XMLReader.cpp\\\n";
        }
        return str.substring(0, str.length() - 2);
    }

    public static String vcproj_make_guid(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return makeGUID();
    }

    ////////////////// Utility Functions ////////////////////////////
    private static String getCppTypeName(MDMInfo[] infos, FieldInfo field) {
        String type = field.type;
        if (type.equalsIgnoreCase("byte")) {
            return "uint8_t";
        }
        if (type.equalsIgnoreCase("char")) {
            return "char";
        }
        if (type.equalsIgnoreCase("bool")) {
            return "bool";
        }
        if (type.equalsIgnoreCase("int16")) {
            return "int16_t";
        }
        if (type.equalsIgnoreCase("uint16")) {
            return "uint16_t";
        }
        if (type.equalsIgnoreCase("int32")) {
            return "int32_t";
        }
        if (type.equalsIgnoreCase("uint32")) {
            return "uint32_t";
        }
        if (type.equalsIgnoreCase("int64")) {
            return "int64_t";
        }
        if (type.equalsIgnoreCase("real32")) {
            return "float";
        }
        if (type.equalsIgnoreCase("real64")) {
            return "double";
        }
        if (type.equalsIgnoreCase("string")) {
            return "std::string";
        }
        if (type.equals(MDMInfo.LMCP_OBJECT_NAME)) {
            return "avtas::lmcp::Object";
        }
        else if (field.isStruct) {
            getResolvedTypeName(infos, field);
        }
        return type;
    }

    private static String getCppTypeSize(String type) {
        if (type.equalsIgnoreCase("byte")) {
            return "1";
        }
        if (type.equalsIgnoreCase("char")) {
            return "1";
        }
        if (type.equalsIgnoreCase("bool")) {
            return "1";
        }
        if (type.equalsIgnoreCase("int16")) {
            return "2";
        }
        if (type.equalsIgnoreCase("uint16")) {
            return "2";
        }
        if (type.equalsIgnoreCase("int32")) {
            return "4";
        }
        if (type.equalsIgnoreCase("uint32")) {
            return "4";
        }
        if (type.equalsIgnoreCase("int64")) {
            return "8";
        }
        if (type.equalsIgnoreCase("real32")) {
            return "4";
        }
        if (type.equalsIgnoreCase("real64")) {
            return "8";
        }
        return "0";
    }

    private static String getLmcpTypeUpperCase(String type) {
        if (type.substring(0, 1).equalsIgnoreCase("u")) {
            return type.substring(0, 2).toUpperCase() + type.substring(2);
        }
        return type.substring(0, 1).toUpperCase() + type.substring(1);
    }

    private static String getByteBufferTypeUpperCase(String type) {
        if (type.equalsIgnoreCase("byte")) {
            return "Byte";
        }
        if (type.equalsIgnoreCase("char")) {
            return "Byte";
        }
        if (type.equalsIgnoreCase("bool")) {
            return "Bool";
        }
        if (type.equalsIgnoreCase("int16")) {
            return "Short";
        }
        if (type.equalsIgnoreCase("uint16")) {
            return "UShort";
        }
        if (type.equalsIgnoreCase("int32")) {
            return "Int";
        }
        if (type.equalsIgnoreCase("uint32")) {
            return "UInt";
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
        return "0";
    }

    private static String getCppDefaultVal(MDMInfo[] infos, FieldInfo field) {
        String type = field.type;
        if (!field.defaultVal.isEmpty()) {
            if (type.equalsIgnoreCase("string")) {
                return "std::string(\"" + field.defaultVal + "\")";
            }
            else if (type.equals("char")) {
                return "'" + field.defaultVal + "'";
            }
            else if (type.equals("int64")) {
                return field.defaultVal + "LL";
            }
            else if (type.equals("real32")) {
                if (field.defaultVal.contains("."))
                    return field.defaultVal + "f";
                else 
                    return field.defaultVal + ".f";
            }
            else if (field.isStruct) {
                if (field.defaultVal.equalsIgnoreCase("null")) {
                    return "0";
                }
            }
            else if (field.isEnum) {
                return getSeriesNamespace(infos, field.seriesName) + field.type + "::" + field.defaultVal;
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
            return "false";
        }
        if (type.equalsIgnoreCase("int16") || type.equalsIgnoreCase("int16_t")) {
            return "0";
        }
        if (type.equalsIgnoreCase("uint16") || type.equalsIgnoreCase("uint16_t")) {
            return "0";
        }
        if (type.equalsIgnoreCase("int32") || type.equalsIgnoreCase("int32_t")) {
            return "0";
        }
        if (type.equalsIgnoreCase("uint32") || type.equalsIgnoreCase("uint32_t")) {
            return "0";
        }
        if (type.equalsIgnoreCase("int64") || type.equalsIgnoreCase("int64_t")) {
            return "0";
        }
        if (type.equalsIgnoreCase("real32") || type.equalsIgnoreCase("float")) {
            return "0.f";
        }
        if (type.equalsIgnoreCase("real64") || type.equalsIgnoreCase("double")) {
            return "0.";
        }
        if (type.equalsIgnoreCase("string")) {
            return "std::string(\"\")";
        }
        if (type.equals(MDMInfo.LMCP_OBJECT_NAME)) {
            return "nullptr";
        }
        if (field.isEnum) {
            return getResolvedTypeName(infos, field) + "::" + MDMInfo.getEnumByName(infos, field).entries.get(0).name;
        }
        return "new " + getResolvedTypeName(infos, field); // for objects
    }

    private static String getArrayTypeName(String type) {
        if (type.equalsIgnoreCase("byte")
                || type.equalsIgnoreCase("char")
                || type.equalsIgnoreCase("bool")
                || type.equalsIgnoreCase("int16")
                || type.equalsIgnoreCase("int16_t")
                || type.equalsIgnoreCase("uint16")
                || type.equalsIgnoreCase("uint16_t")
                || type.equalsIgnoreCase("int32")
                || type.equalsIgnoreCase("int32_t")
                || type.equalsIgnoreCase("uint32")
                || type.equalsIgnoreCase("uint32_t")
                || type.equalsIgnoreCase("int64")
                || type.equalsIgnoreCase("int64_t")
                || type.equalsIgnoreCase("real32")
                || type.equalsIgnoreCase("float")
                || type.equalsIgnoreCase("real64")
                || type.equalsIgnoreCase("double")
                || type.equalsIgnoreCase("string")) {
            return "PrimitiveArray";
        }
        return type + "Array"; // for objects
    }

    private static String getSeriesNamespace(MDMInfo[] infos, String series_name) {
        MDMInfo i = MDMReader.getMDM(series_name, infos);
        if (i != null) {
            return i.namespace.replaceAll("/", "::") + "::";
        }
        return "";
    }

    private static String getSeriesFilepath(MDMInfo[] infos, String series_name) {
        MDMInfo i = MDMReader.getMDM(series_name, infos);
        return i.namespace + "/";
    }

    public static Vector<StructInfo> getAllCppSubclasses(MDMInfo[] infos, FieldInfo f) throws Exception {
        if (f.type.equals(MDMInfo.LMCP_OBJECT_NAME)) {
            return new Vector<StructInfo>();
        }
        StructInfo i = MDMInfo.getStructByName(infos, f);
        return getAllCppSubclasses(infos, i);
    }

    private static Vector<StructInfo> getAllCppSubclasses(MDMInfo[] infos, StructInfo st) throws Exception {
        Vector<StructInfo> ret = new Vector<StructInfo>();
        if (st.name.equals(MDMInfo.LMCP_OBJECT_NAME)) {
            return ret;
        }
        for (MDMInfo in : infos) {
            if (st.extends_series.equals(in.seriesName)) {
                for (int i = 0; i < in.structs.length; i++) {
                    if (in.structs[i].extends_name.equals(st.name)) {
                        ret.add(in.structs[i]);
                        ret.addAll(getAllCppSubclasses(infos, in.structs[i]));
                    }
                }
            }
        }
        return ret;
    }

    private static String getCppNamespace(String namespace) {
        return namespace.replaceAll("/", "::");
    }

    private static String getResolvedTypeName(MDMInfo[] infos, FieldInfo field) {
        String type = field.type;
        if (field.isStruct) {
            if (field.type.equals(MDMInfo.LMCP_OBJECT_NAME)) {
                return "avtas::lmcp::Object";
            }
            return getSeriesNamespace(infos, field.seriesName) + type;
        }
        else if (field.isEnum) {
            return getSeriesNamespace(infos, field.seriesName) + type + "::" + type;
        }
        else {
            return getCppTypeName(infos, field);
        }
    }
    static final String block_comment = "\n//";

    /** Generates a Globally Unique ID based on RFC 4122.
     * @return a Globally unique ID with the format: 8-4-4-4-12 digits
     */
    static String makeGUID() {

        return UUID.randomUUID().toString().toUpperCase();

//        Original method Based on code available at www.somacon.com/p113.php
//
//        int n1 = (int) (Math.random() * 65535);
//        int n2 = (int) (Math.random() * 65535);
//        int n3 = (int) (Math.random() * 65535);
//        int n4 = (int) (Math.random() * 4095);
//        String str5 = Integer.toBinaryString((int) (Math.random() * 65535));
//        str5 = str5.substring(0, 5) + "01" + str5.substring(8, str5.length() - 1);
//        int n5 = Integer.valueOf(str5, 2);
//        int n6 = (int) (Math.random() * 65535);
//        int n7 = (int) (Math.random() * 65535);
//        int n8 = (int) (Math.random() * 65535);
//
//        return String.format("%04x%04x-%04x-%03x4-%04x-%04x%04x%04x", n1, n2, n3, n4, n5, n6, n7, n8).toUpperCase();

    }
};
