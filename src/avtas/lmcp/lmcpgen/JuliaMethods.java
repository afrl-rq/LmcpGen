/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package avtas.lmcp.lmcpgen;

import java.io.File;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Dictionary;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Set;
import java.util.Vector;

/**
 *
 * @author colintaylor
 */
public class JuliaMethods {
    
    private static class DependencyContainer {
        MDMInfo mdmInfo;
        StructInfo structInfo;
        Set<DependencyContainer> dependencies;
            
        DependencyContainer(MDMInfo mdmInfo, StructInfo structInfo) {
            this.mdmInfo = mdmInfo;
            this.structInfo = structInfo;
            this.dependencies = new HashSet();
        }
            
            
        DependencyContainer find(Vector<DependencyContainer> containers, String structName, String seriesName ) {
            for (DependencyContainer dc : containers) {
                if (dc.structInfo.name.equals(structName) && dc.structInfo.seriesName.equals(seriesName)) {
                    return dc;
                }
            }
            return null;
        }
    }
    
    private static Vector<DependencyContainer> BuildDependencyGraph(MDMInfo[] infos) {
        
        Vector<DependencyContainer> missingDependencies = new Vector();
        
        //capture all possible dependencies
        for (MDMInfo mi : infos) {
            for (StructInfo si : mi.structs) {
                    missingDependencies.add(new DependencyContainer(mi, si));
                    //System.out.println(si.seriesName);
            }
        }
        
        //build the dependency graph
        for (DependencyContainer dc : missingDependencies) {
            StructInfo si = dc.structInfo;
            
            if (!si.extends_name.isBlank()) { //check dependencies on interfaces
                DependencyContainer extendsStruct = dc.find(missingDependencies, si.extends_name, si.extends_series);
                dc.dependencies.add(extendsStruct);
                //System.out.println(String.format("%s depends on %s", si.name, extendsStruct.structInfo.name));

            }
            for (FieldInfo fi : si.fields) {
                if (!isTypePrimitive(fi.type) && !fi.isEnum) { //check field types
                    DependencyContainer fieldStruct = dc.find(missingDependencies, fi.type, fi.seriesName);
                    dc.dependencies.add(fieldStruct);
                    //System.out.println(String.format("%s depends on %s", si.name, fi.type));
                }
            }
        }
        return missingDependencies;
    }
    
    public static String series_dir(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + info.namespace;
    }
    
    public static String datatype_name(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.name;
    }
    public static String enumtype_name(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + en.name;
    }
    
    public static String classname(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.name;
    }
    
    public static String typeName(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.name;
    }
    
    public static String extends_check(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + (st.extends_name.isBlank() ? "LMCP.LmcpBase" : st.extends_name);
    }
    public static String extends_impl(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + (st.extends_name.isBlank() ? "LmcpMessage" : st.extends_name);
    }
    public static String extends_series(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + (st.extends_name.isBlank() ? "LMCP." : st.extends_series + ".");
    }
        
    public static String list_imports(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return " ";
    }
    
    public static String lmcp_type(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.name;
    }
    public static String lmcp_type_id(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.id + "";
    }
    
    public static String full_datatype_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        String[] tmp = info.namespace.split("/");
        for (int i = 0; i < tmp.length; i++) {
            str += tmp[i] + ".";
        }
        return ws + str + st.name;
    }
    
    public static String series_name(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + info.seriesName;
    }
    public static String series_name_id(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + info.seriesNameAsLong;
    }
    public static String series_version(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + info.version;
    }
    public static String creation_date(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + DateFormat.getDateInstance(DateFormat.FULL).format(new Date());
    }
    
    public static String unpack_vars(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + "\"" + info.seriesName + "\"";
    }
        
    public static String print_vars(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + "\"" + info.seriesName + "\"";
    }
        
    public static String enum_name(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + en.name;
    }
    
    public static String enumType(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + en.name;
    }
    
    public static String enum_gen_entries(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for(EnumInfo.EnumEntry entry : en.entries) {
            buf.append(ws + entry.name + " = " + entry.value + "\n");
        }
        return buf.toString();
    }
    
    public static String enum_from_string(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for(EnumInfo.EnumEntry entry : en.entries) {
            buf.append(ws + "if str == \"" + entry.name + "\" return " + entry.name + " end\n");
        }
        return buf.toString();
    }

    public static String enum_from_int(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuffer buf = new StringBuffer();
        for(EnumInfo.EnumEntry entry : en.entries) {
            buf.append(ws + "if val == " + entry.name + " return \"" + entry.name + "\"" + " end \n");
        }
        buf.append(ws + "return " + en.entries.get(0).name + "\n");
        return buf.toString();
    }
    public static String testPack(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        
        for (MDMInfo mdmInfo : infos) {
            str += ws + String.format("import LMCP.%s\n", mdmInfo.seriesName);
            for (StructInfo structInfo : mdmInfo.structs) {
                str += ws + String.format("tmp = LMCP.%s()\n", structInfo.seriesName + "." + structInfo.name);
                str += ws + String.format("buffer = LMCP.pack_message(tmp)\n");
                str += ws + String.format("tmp2 = LMCP.unpack_message(buffer)\n");
                //str += ws + "@assert(tmp == tmp2)\n";
            }
            str += "\n";
        }
        return str;
    }
    
    public static String testXML(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        
        for (MDMInfo mdmInfo : infos) {
            str += ws + String.format("import LMCP.%s\n", mdmInfo.seriesName);
            for (StructInfo structInfo : mdmInfo.structs) {
                str += ws + String.format("tmp = LMCP.%s()\n", structInfo.seriesName + "." + structInfo.name);
                str += ws + String.format("xml = LMCP.to_xml(tmp)\n");
                str += ws + String.format("tmp2 = LMCP.from_xml(xml)\n");
                //str += ws + "@assert(tmp == tmp2)\n";
            }
            str += "\n";

        }
        return str;
    }
    
   public static String tests(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        
        for (MDMInfo mdmInfo : infos) {
            str += ws + String.format("import LMCP.%s\n", mdmInfo.seriesName);
            for (StructInfo structInfo : mdmInfo.structs) {
                 //str += ws + String.format("println(\"Testing %s\")\n", structInfo.name);            
                 str += ws + String.format("@test test_pack_unpack(LMCP.%s.%s())\n", mdmInfo.seriesName, structInfo.name);            
                 str += ws + String.format("@test test_xml(LMCP.%s.%s())\n", mdmInfo.seriesName, structInfo.name);            
            }
            str += "\n";

        }
        return str;
   }
    
    public static String testClient(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        
        for (MDMInfo mdmInfo : infos) {
            for (StructInfo structInfo : mdmInfo.structs) {
                str += ws + String.format("tmp = LMCP.%s.%s()\n", mdmInfo.seriesName, structInfo.name); 
                str += ws + String.format("bytes = LMCP.pack_message(tmp)\n");
                str += ws + String.format("tmp = write(clientSocket, bytes)\n"); 
            }
        }
        return str;
    }
    
    public static List<FieldInfo> getParentFields(MDMInfo[] infos, String parentName, String parentSeries) {
        ArrayList<FieldInfo> ret = new ArrayList();
        List<StructInfo> parentStructs = findAllParentStructs(infos, parentName, parentSeries);
        
        for (StructInfo si : parentStructs) {
            for (FieldInfo fi : si.fields) {
                ret.add(fi);
            }
        }
        return ret;
    }
    
    public static List<StructInfo> findAllParentStructs(MDMInfo[] infos, String parentName, String parentSeries) {
        ArrayList<StructInfo> ret = new ArrayList();
        StructInfo parent = findParentStruct(infos, parentName, parentSeries);
        while (parent != null) {
            ret.add(0, parent);
            parent = findParentStruct(infos, parent.extends_name, parent.extends_series);
        }
        return ret;
    }
    
    public static StructInfo findParentStruct(MDMInfo[] infos, String parentName, String parentSeries) {
        
        for (MDMInfo info : infos) {
            if (!info.seriesName.equalsIgnoreCase(parentSeries)) {
                continue;
            }
            for (StructInfo si : info.structs) {
                if (si.name.equalsIgnoreCase(parentName)) {
                    return si;
                }
            }
        }
        return null;
    }
    
    public static String define_vars(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        

        List<FieldInfo> fields = getParentFields(infos, st.extends_name, st.extends_series);
        fields.addAll(Arrays.asList(st.fields));
        
        for (FieldInfo f : fields) {
            String name = f.name;
            String type = getJuliaType(f.type);
            String qualifier = f.isEnum ? f.seriesName + "." : "";
            if (f.isArray) {
                //String qualifier = isTypePrimitive(f.type) ? "" : type + "Module.";
                if (f.isEnum) {
                    str += ws + name + "::Vector{" + qualifier + type + "Module." + type + "}";
                }
                else if (f.isStruct){
                    type = f.seriesName + ".Abstract" + type;
                    str += ws + name + "::Vector{" + type + "}";
                }
                else {
                    str += ws + name + "::Vector{" + type + "}";
                }
            }
            else if (f.isEnum) {
                str += ws + name + "::" + qualifier + type + "Module." + type;
            }
            else if (f.isStruct) {
                type = f.seriesName + ".Abstract" + type;
                if (f.isOptional || f.defaultVal.equalsIgnoreCase("null")) {
                    type = "Union{" + type + ", Nothing}";
                }
                str += ws + name + "::" + type;
            }
            else 
            {
                str += ws + name + "::" + type;
            }
            str += "\n";
        }
        return str;
    }    
    
    public static String check_large_arrays(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String ret = "";
        for (FieldInfo f : st.fields) {
            if (f.isLargeArray) {
                ret += String.format("push!(LMCP.LARGE_ARRAY_REGISTRY, (%s, Symbol(\"%s\")))\n", st.name, f.name);
            }
        }
        return ret;
    }
    
    public static String doc_string_mdm(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return info.comment;
    }
    
    public static String doc_string_struct(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return st.comment;
    }
    
    public static String doc_string_enum(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return en.comment;
    }
    
    public static String LMCPFactoryModuleSetup(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";

        return str;
    }

    public static String root_module_includes(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        
        Vector<DependencyContainer> missingDependencies = BuildDependencyGraph(infos);

        
        String str = "";
        HashSet<String> mdmInsertions = new HashSet();
        Vector<MDMInfo> mdms = new Vector();
        for (MDMInfo mdmInfo : infos)
            mdms.add(mdmInfo);

//        for (MDMInfo mdmInfo : infos) {
//            for (String dep: mdmInfo.mdmDependencies) {
//                System.out.println(String.format("%s   %s", mdmInfo.seriesName, dep));
//            }
//        }
        
        
        while (!mdms.isEmpty()) {
            //loop through MDMs
            for (MDMInfo mdmInfo : mdms) {
                if (mdmInsertions.containsAll(mdmInfo.mdmDependencies)) {
                    //str += "# " + mdmInfo.seriesName + "\n";
                    //String module = "";
//                    for (EnumInfo f : mdmInfo.enums) { //knock out enums. These have no dependencies
//                        //Enums have no dependencies.
//                        module = String.format("\"%s/%sModule.jl\"", mdmInfo.namespace, f.name);
//                        String newInclude = String.format("include(%s)\n", module);
//                        str += newInclude;            
//                    }
                    str += String.format("include(\"%s/%s.jl\")\n", mdmInfo.namespace, mdmInfo.seriesName);
                
                    //get all structs in this MDM
                    Vector<DependencyContainer> mdmStructs = new Vector();
                    for (DependencyContainer dc : missingDependencies) {
                        if (dc.mdmInfo.equals(mdmInfo)) {
                            mdmStructs.add(dc);
                        }
                    }
                
                    //unroll
                    while (!mdmStructs.isEmpty()) {
                        int startCount = mdmStructs.size();
                        for (DependencyContainer dc : mdmStructs) {
                            if (dc.dependencies.isEmpty()) {
                                String dependentStruct = String.format("\"%s/%s.jl\"", dc.mdmInfo.namespace, dc.structInfo.name);
                                String dependentInclude = String.format("include(%s)\n", dependentStruct);
                                //str += dependentInclude;
                
                                mdmStructs.remove(dc);
                                for(DependencyContainer innerDc : missingDependencies) {
                                    innerDc.dependencies.remove(dc);
                                }
                                break;
                            }
                        }
                        if (startCount == mdmStructs.size()) {
                            String desc = "";
                            for (DependencyContainer dc : mdmStructs) {
                                for (DependencyContainer innerDc : dc.dependencies) {
                                    System.out.print(String.format("ERROR: Dep %s not found for %s\n", innerDc.structInfo.name, dc.structInfo.name));
                                }
                            }
                            break;
                        }
                    }
                    mdmInsertions.add(mdmInfo.seriesName);
                    mdms.remove(mdmInfo);
                    break;
                }

            }  

        }
        
        //System.out.println(str);

        return str;
    }
    public static String module_includes(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
                
        String str = "";
        for (EnumInfo f : info.enums) { //knock out enums. These have no dependencies
            //Enums have no dependencies.
            String newInclude = String.format("include(\"%sModule.jl\")\n", f.name);
            str += newInclude;            
        }
        
        Vector<DependencyContainer> missingDependencies = BuildDependencyGraph(new MDMInfo[]{info});
        
        while (!missingDependencies.isEmpty()) {
            
            DependencyContainer justRemoved = null;
            for (DependencyContainer dc : missingDependencies) {
                if (dc.dependencies.isEmpty()) {
                    str += String.format("include(\"%s.jl\")\n", dc.structInfo.name);
                    justRemoved = dc;
                    break;
                }                
            }
            missingDependencies.remove(justRemoved);
            for (DependencyContainer dc : missingDependencies) {
                dc.dependencies.remove(justRemoved);
            }
        }
        
        return str;
    }
    
    public static String struct_imports(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        
        //imports for single struct
        
        HashSet<String> deps = new HashSet<String>();
        for (FieldInfo fi : st.fields) {
            deps.add(fi.seriesName);
        }
        deps.add(st.extends_series);
        deps.remove(st.seriesName); //redundant
        
        String ret = "";
        for (String s : deps) {
            ret += String.format("import ..%s\n", s);
        }
        return ret;
    }
    
    public static String define_defaults(MDMInfo[] infos, MDMInfo info, final File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = ws;
        
        List<FieldInfo> fields = getParentFields(infos, st.extends_name, st.extends_series);
        fields.addAll(Arrays.asList(st.fields));
        
        if (fields.isEmpty()) {
            return str;
        }
        
        for (FieldInfo f : fields) {
            String type = getJuliaType(f.type);
            String qualifier = f.isEnum ? f.seriesName + "." : "";
            if (!f.isArray) {
                if (f.type.equalsIgnoreCase("string")) {
                    str += f.defaultVal.isBlank() ? "\"\"," : "\"" + f.defaultVal + "\", ";
                }
                else if (f.isStruct) {
                    str += f.defaultVal.equalsIgnoreCase("null") || f.isOptional ? "nothing, " : f.seriesName + "." + f.type + "(),";
                }
                else if (f.isEnum) {
                    str += qualifier + f.type + "Module." + f.defaultVal + ", ";
                }
                else {
                    String defaultVal = f.defaultVal;
                    if (defaultVal.isBlank()) {
                        defaultVal = getDefaultValue(f.type);
                    }
                    str += defaultVal + ", ";
                }
            
            // variable length array
            } else  {
                String qualifiedName = type;
                if (f.isEnum) {
                    qualifiedName = qualifier + type + "Module." + type;
                }
                else if (f.isStruct) {
                    qualifiedName = f.seriesName + "." + type;
                }
                if (f.length < 0) {
                    str += "Vector{" + qualifiedName + "}(), ";
                } else {
                    String initializer = isTypePrimitive(f.type) ? "0" : "";
                    str += String.format("[%s(%s) for x in 1:%s], ", qualifiedName, initializer, f.length);
                    //str += "Vector{" + qualifiedName + "}(undef, "+ f.length + "), ";
                }
            }
        }
        str = String.format("%s() = %s(%s)", st.name, st.name, str);
        return str;
    }
    
    private static boolean isTypePrimitive(String type) {
        return type.toLowerCase().matches("(bool)|(string)|(byte)|(char)|(real64)|(real32)|(int64)|(int32)|(int16)|(uint32)|(uint16)");
    }
    
    private static String getDefaultValue(String type) {
        if (type.toLowerCase().equals("bool")) {
            return "false";
        }
        if (type.toLowerCase().equals("string")) {
            return "\"\"";
        }
        else {
            return "0";
        }
    }
    
    private static String getJuliaType(String type) {

        if (type.toLowerCase().equals("bool")) {
            return "Bool";
        }
        if (type.toLowerCase().equals("string")) {
            return "String";
        }
        if (type.toLowerCase().equals("byte")) {
            return "UInt8";
        }
        if (type.toLowerCase().equals("char")) {
            return "Char";
        }
        if (type.toLowerCase().equals("real64")) {
            return "Float64";
        }
        if (type.toLowerCase().equals("real32")) {
            return "Float32";
        }
        if (type.toLowerCase().equals("int64")) {
            return "Int64";
        }
        if (type.toLowerCase().equals("int32")) {
            return "Int32";
        }
        if (type.toLowerCase().equals("int16")) {
            return "Int16";
        }
        if (type.toLowerCase().equals("uint32")) {
            return "UInt32";
        }
        if (type.toLowerCase().equals("uint16")) {
            return "UInt16";
        }
        if (type.equalsIgnoreCase(MDMInfo.LMCP_OBJECT_NAME)) {
            return "LMCPObject";
        }
        return type;
    }
}
