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
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Stream;

public class RustMethods {

    public static String series_id(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + info.seriesNameAsLong;
    }

    public static String series_version(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + info.version;
    }

    public static String series_dir(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + info.namespace;
    }

    public static String namespace_subdir(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        // this will be the dummy MDM created by the PER_NS_SUBDIR driver
        return ws + info.namespace;
    }

    public static String namespace(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return info.namespace.replaceAll("/", "::");
    }

    // Returns the date that the package was made
    public static String creation_date(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + DateFormat.getDateInstance(DateFormat.FULL).format(new Date());
    }

    public static String datatype_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.name;
    }

    public static String datatype_snake_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + snake_case(st.name);
    }

    public static String series_snake_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + getSeriesSnake(infos, st.seriesName);
    }

    public static String longdatatype_name_dots(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        String str = "";
        String[] tmp = info.namespace.split("/");
        for (int i = 0; i < tmp.length; i++) {
            str += tmp[i] + ".";
        }
        return ws + str + st.name;
    }

    public static String enum_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + en.name;
    }

    public static String enum_snake_name(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + snake_case(en.name);
    }

    public static String declare_enum_fields(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();
        for (EnumInfo.EnumEntry entry : en.entries) {
            sb.append(String.format("\n    %s = %s,", entry.name, entry.value));
        }
        return sb.toString();
    }

    public static String match_enum_from_i32(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();

        // first, figure out whether we'll have to number them
        // ourselves, and error out unless they're all one or the
        // other
        boolean hasCustomValues = en.entries.stream().allMatch(entry -> entry.value != null);
        boolean generateValues = en.entries.stream().allMatch(entry -> entry.value == null);
        if (!(hasCustomValues || generateValues)) {
            throw new IllegalArgumentException("Enums must have values for all entries, or for no entries");
        }

        int gen = 0;
        for (EnumInfo.EnumEntry entry : en.entries) {
            int v = hasCustomValues ? Integer.parseInt(entry.value) : gen;
            sb.append(ws);
            sb.append(String.format("%d => Some(%s::%s),\n", v, en.name, entry.name));
            gen++;
        }
        return sb.toString();
    }

    public static String enum_default_variant(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return en.entries.get(0).name;
    }

    public static String enum_variant_choices(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();
        for (EnumInfo.EnumEntry entry : en.entries) {
            sb.append(ws);
            sb.append(String.format("%s::%s,", en.name, entry.name));
        }
        return sb.toString();
    }

    public static String datatype_id(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        return ws + st.id;
    }

    public static String use_dependents(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        SortedSet<String> uses = new TreeSet<>();

        List<StructInfo> parents = MDMInfo.getAllParents(infos, st0);
        Set<StructInfo> children = MDMInfo.getAllChildren(infos, st0);

        // use all parents and children
        Stream.concat(parents.stream(), children.stream())
            .filter(relative -> !relative.equals(st0)) // exclude current module
            .forEach(relative -> {
                    String prefix = getSeriesModule(infos, relative.seriesName);
                    uses.add(ws + String.format("use %s::%s;\n", prefix, snake_case(relative.name)));
                });

        for (StructInfo st : MDMInfo.getAllParents(infos, st0)) {
            for (FieldInfo field : st.fields) {
                if (!(field.isEnum || field.isStruct)) {
                    continue;
                }
                String type = snake_case(getResolvedTypeName(infos, field));
                uses.add(String.format(ws + "use %s;\n", type));
            }
        }

        StringBuilder sb = new StringBuilder();
        for (String use : uses) {
            sb.append(use);
        }
        return sb.toString();
    }

    public static String use_dependents_tests(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        SortedSet<String> uses = new TreeSet<>();

        for (StructInfo st : MDMInfo.getAllParents(infos, st0)) {
            for (FieldInfo field : st.fields) {
                if (!(field.isEnum || field.isStruct)) {
                    continue;
                }
                String type = snake_case(getResolvedTypeName(infos, field));
                uses.add(String.format(ws + "use %s::tests as %sTests;\n", type, field.type));
            }
        }

        StringBuilder sb = new StringBuilder();
        for (String use : uses) {
            sb.append(use);
        }
        return sb.toString();
    }

    public static String struct_copy(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        if (can_copy_struct(infos, st)) {
            return ws + "Copy, ";
        } else {
            return ws;
        }
    }

    public static String declare_parent_trait(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        if (!st.hasParent()) {
            return ws;
        } else {
            return String.format(ws + "+ ::%s::%s::%sT", getSeriesModule(infos, st.extends_series), snake_case(st.extends_name), st.extends_name);
        }
    }

    public static String declare_fields(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();
        for (StructInfo st : MDMInfo.getAllParents(infos, st0)) {
            for (FieldInfo field : st.fields) {
                String type = getBoxedTypeName(infos, field);
                sb.append(String.format("\n    pub %s: %s,", snake_case(field.name), type));
            }
        }
        return sb.toString();
    }

    public static String declare_trait_methods(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();

        sb.append(ws + String.format("fn as_%s_%s(&self) -> Option<&%s> { None }\n",
                                     getSeriesSnake(infos, st0.seriesName), snake_case(st0.name), st0.name));
        sb.append(ws + String.format("fn as_mut_%s_%s(&mut self) -> Option<&mut %s> { None }\n",
                                     getSeriesSnake(infos, st0.seriesName), snake_case(st0.name), st0.name));
        Set<StructInfo> children = MDMInfo.getAllChildren(infos, st0);
        for (StructInfo child : children) {
            String prefix = getSeriesModule(infos, child.seriesName);
            sb.append(ws + String.format("fn as_%s_%s(&self) -> Option<&::%s::%s::%s> { None }\n",
                                         getSeriesSnake(infos, child.seriesName), snake_case(child.name), prefix, snake_case(child.name), child.name));
            sb.append(ws + String.format("fn as_mut_%s_%s(&mut self) -> Option<&mut ::%s::%s::%s> { None }\n",
                                         getSeriesSnake(infos, child.seriesName), snake_case(child.name), prefix, snake_case(child.name), child.name));
        }
        for (FieldInfo field : st0.fields) {
            String ref = field.isReallyScalar() ? "" : "&";
            String type = getBoxedTypeName(infos, field);
            sb.append(ws + String.format("fn %s(&self) -> %s%s;\n", snake_case(field.name), ref, type));
            sb.append(ws + String.format("fn %s_mut(&mut self) -> &mut %s;\n", snake_case(field.name), type));
        }
        return sb.toString();
    }

    public static String declare_trait_impls(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();

        for (StructInfo st : MDMInfo.getAllParents(infos, st0)) {
            String prefix = st == st0 ? "" : "::" + getSeriesModule(infos, st.seriesName) + "::" + snake_case(st.name) + "::";
            sb.append(String.format("impl %s%sT for %s {\n", prefix, st.name, st0.name));
            sb.append(String.format("    fn as_%s_%s(&self) -> Option<&%s> { Some(self) }\n"
                                    , getSeriesSnake(infos, st0.seriesName), snake_case(st0.name), st0.name));
            sb.append(String.format("    fn as_mut_%s_%s(&mut self) -> Option<&mut %s> { Some(self) }\n"
                                    , getSeriesSnake(infos, st0.seriesName), snake_case(st0.name), st0.name));
            for (FieldInfo field : st.fields) {
                String name = snake_case(field.name);
                String ref = field.isReallyScalar() ? "" : "&";
                String type = getBoxedTypeName(infos, field);
                sb.append(String.format("    fn %s(&self) -> %s%s { %sself.%s }\n", name, ref, type, ref, name));
                sb.append(String.format("    fn %s_mut(&mut self) -> &mut %s { &mut self.%s }\n", name, type, name));
            }
            sb.append("}\n");
        }

        return sb.toString();
    }

    public static String trait_clone_cases(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();

        for (StructInfo child : MDMInfo.getAllChildren(infos, st0)) {
            String prefix = getSeriesSnake(infos, child.seriesName);
            sb.append(ws + String.format("} else if let Some(x) = %sT::as_%s_%s(self.as_ref()) {\n",
                                         st0.name, prefix, snake_case(child.name)));
            sb.append(ws + String.format("    Box::new(x.clone())\n"));
        }

        return sb.toString();
    }

    public static String struct_partialeq_cases(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();

        for (FieldInfo field : st0.fields) {
            String name = snake_case(field.name);
            sb.append(ws + String.format("&& &self.%s == &_other.%s\n", name, name));
        }

        return sb.toString();
    }

    public static String trait_partialeq_cases(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();

        for (StructInfo child : MDMInfo.getAllChildren(infos, st0)) {
            String prefix = getSeriesSnake(infos, child.seriesName);
            sb.append(ws + String.format("} else if let (Some(x), Some(y)) =\n"));
            sb.append(ws + String.format("    (%sT::as_%s_%s(self.as_ref()),\n", st0.name, prefix, snake_case(child.name)));
            sb.append(ws + String.format("     %sT::as_%s_%s(other.as_ref())) {\n", st0.name, prefix, snake_case(child.name)));
            sb.append(ws + String.format("        x == y\n"));
        }

        return sb.toString();
    }

    public static String trait_lmcp_ser_cases(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();

        for (StructInfo child : MDMInfo.getAllChildren(infos, st0)) {
            String prefix = getSeriesSnake(infos, child.seriesName);
            sb.append(ws + String.format("} else if let Some(x) = %sT::as_%s_%s(self.as_ref()) {\n",
                                         st0.name, prefix, snake_case(child.name)));
            sb.append(ws + String.format("    x.lmcp_ser(buf)\n"));
        }

        return sb.toString();
    }

    public static String trait_lmcp_deser_cases(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();

        for (StructInfo child : MDMInfo.getAllChildren(infos, st0)) {
            String prefix = getSeriesModule(infos, child.seriesName);
            sb.append(ws + String.format("} else if si == ::%s::%s::%s::struct_info() {\n",
                                         prefix, snake_case(child.name), child.name));
            sb.append(ws + String.format("    let (x, readb) = get!(::%s::%s::%s::lmcp_deser(buf));\n",
                                         prefix, snake_case(child.name), child.name));
            sb.append(ws + String.format("    Some((Box::new(x), readb))\n"));
        }

        return sb.toString();
    }

    public static String trait_lmcp_size_cases(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();

        for (StructInfo child : MDMInfo.getAllChildren(infos, st0)) {
            String prefix = getSeriesSnake(infos, child.seriesName);
            sb.append(ws + String.format("} else if let Some(x) = %sT::as_%s_%s(self.as_ref()) {\n",
                                         st0.name, prefix, snake_case(child.name)));
            sb.append(ws + String.format("    x.lmcp_size()\n"));
        }

        return sb.toString();
    }

    public static String struct_lmcp_ser_body(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en0, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();
        for (StructInfo st : MDMInfo.getAllParents(infos, st0)) {
            for (FieldInfo field : st.fields) {
                sb.append(ws + "{\n");
                sb.append(ws + "    let r = get!(buf.get_mut(pos ..));\n");
                sb.append(ws + String.format("    let writeb: usize = get!(self.%s.lmcp_ser(r));\n", snake_case(field.name)));
                sb.append(ws + "    pos += writeb;\n");
                sb.append(ws + "}\n");
            }
        }
        return sb.toString();
    }

    public static String struct_lmcp_deser_body(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en0, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();

        boolean needMutable = false;
        for (StructInfo st : MDMInfo.getAllParents(infos, st0)) {
            for (FieldInfo field : st.fields) {
                needMutable = true;
                sb.append(ws + "{\n");
                sb.append(ws + "    let r = get!(buf.get(pos ..));\n");
                sb.append(ws + String.format("    let (x, readb): (%s, usize) = get!(LmcpSer::lmcp_deser(r));\n",
                                             getBoxedTypeName(infos, field)));
                sb.append(ws + String.format("    out.%s = x;\n", snake_case(field.name)));
                sb.append(ws + "    pos += readb;\n");
                sb.append(ws + "}\n");
            }
        }

        String mut = needMutable ? " mut" : "";
        sb.insert(0, ws + String.format("let%s out: %s = Default::default();\n", mut, st0.name));

        return sb.toString();
    }

    public static String struct_lmcp_size_body(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en0, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();

        boolean needMutable = false;
        for (StructInfo st : MDMInfo.getAllParents(infos, st0)) {
            for (FieldInfo field : st.fields) {
                needMutable = true;
                sb.append(ws + String.format("size += self.%s.lmcp_size();\n", snake_case(field.name)));
            }
        }

        String mut = needMutable ? " mut" : "";
        sb.insert(0, ws + String.format("let%s size = 15;\n", mut));

        return sb.toString();
    }

    public static String declare_entire_series(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en0, String ws) throws Exception {
        // we're going to be passed the dummy subdir MDM, so we need
        // to look up the real ones if they exist, in addition to any
        // other MDMs that are immediate children of the prefix

        List<MDMInfo> realMDMs = new LinkedList<>();
        for (MDMInfo mdm : infos) {
            if (mdm.namespace.equals(info.namespace)) {
                realMDMs.add(mdm);
            }
        }

        Set<String> children = new HashSet<>();
        for (MDMInfo mdm : infos) {
            if (mdm.namespace.equals(info.namespace)) {
                continue;
            }
            if (mdm.namespace.startsWith(info.namespace)) {
                String child = mdm.namespace.replaceFirst(info.namespace, "");
                children.add(child.split("/")[1]);
            }
        }

        StringBuilder sb = new StringBuilder();

        for (MDMInfo mdm : realMDMs) {
            for (StructInfo st : mdm.structs) {
                sb.append(ws);
                sb.append(String.format("pub mod %s;\n", snake_case(st.name)));
            }
            for (EnumInfo en : mdm.enums) {
                sb.append(ws);
                sb.append(String.format("pub mod %s;\n", snake_case(en.name)));
            }
        }

        for (String child : children) {
            sb.append(ws);
            sb.append(String.format("pub mod %s;\n", child));
        }

        return sb.toString();
    }

    public static String declare_top_level_modules(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();
        SortedSet<String> children = new TreeSet<>();
        for (MDMInfo i : infos) {
            if(i.seriesNameAsLong == 0)
            {
                continue;
            }
            children.add(i.namespace.split("/")[0]);
        }
        for (String child : children) {
            sb.append(ws);
            sb.append(String.format("pub mod %s;\n", child));
        }
        return sb.toString();
    }

    public static String declare_top_enum(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();
        for (MDMInfo mdm : infos) {
            for (StructInfo st : mdm.structs) {
                sb.append(ws);
                String prefix_module = getSeriesModule(infos, st.seriesName);
                String prefix_camel = getSeriesCamel(infos, st.seriesName);
                sb.append(String.format("%s%s(%s::%s::%s),\n", prefix_camel, st.name, prefix_module, snake_case(st.name), st.name));
            }
        }
        return sb.toString();
    }

    public static String use_all_structs(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();
        for (MDMInfo mdm : infos) {
            for (StructInfo st : mdm.structs) {
                sb.append(ws);
                String prefix = getSeriesModule(infos, st.seriesName);
                sb.append(String.format("use %s::%s;\n", prefix, snake_case(st.name)));
            }
        }
        return sb.toString();
    }

    public static String match_lmcp_ser(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();
        for (MDMInfo mdm : infos) {
            for (StructInfo st : mdm.structs) {
                sb.append(ws);
                String prefix = getSeriesCamel(infos, st.seriesName);
                sb.append(String.format("LmcpType::%s%s(ref x) => x.lmcp_ser(buf),\n", prefix, st.name));
            }
        }
        return sb.toString();
    }

    public static String match_lmcp_size(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();
        for (MDMInfo mdm : infos) {
            for (StructInfo st : mdm.structs) {
                sb.append(ws);
                String prefix = getSeriesCamel(infos, st.seriesName);
                sb.append(String.format("LmcpType::%s%s(ref x) => x.lmcp_size(),\n", prefix, st.name));
            }
        }
        return sb.toString();
    }

    public static String match_lmcp_deser(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();
        for (MDMInfo mdm : infos) {
            for (StructInfo st : mdm.structs) {
                sb.append(ws);
                sb.append(String.format("(%d, %d) => {\n", mdm.seriesNameAsLong, st.id));
                sb.append(ws);
                String prefix_module = getSeriesModule(infos, st.seriesName);
                sb.append(String.format("    let (s, i) = get!(%s::%s::%s::lmcp_deser(buf));\n", prefix_module, snake_case(st.name), st.name));
                sb.append(ws);
                String prefix_camel = getSeriesCamel(infos, st.seriesName);
                sb.append(String.format("    Some((LmcpType::%s%s(s), i))\n", prefix_camel, st.name));
                sb.append(ws);
                sb.append("}\n");
            }
        }
        return sb.toString();
    }

    public static String match_subscription(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();
        for (MDMInfo mdm : infos) {
            for (StructInfo st : mdm.structs) {
                sb.append(ws);
                String prefix_module = getSeriesModule(infos, st.seriesName);
                String prefix_camel = getSeriesCamel(infos, st.seriesName);
                sb.append(String.format("LmcpType::%s%s(_) => %s::%s::%s::subscription(),\n", prefix_camel, st.name, prefix_module, snake_case(st.name), st.name));
            }
        }
        return sb.toString();
    }

    public static String declare_arbitrary_fields(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();
        for (StructInfo st : MDMInfo.getAllParents(infos, st0)) {
            for (FieldInfo field : st.fields) {
                if (field.isArray && field.isStruct) {
                    sb.append(ws + String.format(
                        "%s: Vec::<%s::%s>::arbitrary(_g).into_iter().map(|x| Box::new(x) as Box<%s::%sT>).collect(),\n",
                        snake_case(field.name), snake_case(field.type), field.type, snake_case(field.type), field.type));
                } else if (field.isStruct && field.isOptional) {
                    sb.append(ws + String.format("%s: {\n", snake_case(field.name)));
                    sb.append(ws + String.format("    if _g.gen() {\n"));
                    sb.append(ws + String.format("        Some(Box::new(%s::%s::arbitrary(_g)))\n",
                                                 snake_case(field.type), field.type));
                    sb.append(ws + String.format("    } else {\n"));
                    sb.append(ws + String.format("        None\n"));
                    sb.append(ws + String.format("    }\n"));
                    sb.append(ws + String.format("},\n"));
                } else if (field.isStruct) {
                    sb.append(ws + String.format("%s: Box::new(%s::%s::arbitrary(_g)),\n",
                                                 snake_case(field.name), snake_case(field.type), field.type));
                } else {
                    sb.append(ws + String.format("%s: Arbitrary::arbitrary(_g),\n", snake_case(field.name)));
                }
            }
        }
        return sb.toString();
    }

    public static String discard_long_fields(MDMInfo[] infos, MDMInfo info, File outfile, StructInfo st0, EnumInfo en, String ws) throws Exception {
        StringBuilder sb = new StringBuilder();

        boolean needImport = false;
        for (StructInfo st : MDMInfo.getAllParents(infos, st0)) {
            for (FieldInfo field : st.fields) {
                if (!field.isArray) {
                    continue;
                }
                needImport = true;
                sb.append(ws);
                sb.append(String.format("if x.%s.len() > (u16::MAX as usize) { return TestResult::discard(); }\n", snake_case(field.name)));
            }
        }
        if (needImport) {
            sb.insert(0, ws + "use std::u16;\n");
        }
        return sb.toString();
    }

    ////////////////// Utility Functions ////////////////////////////

    private static String getRustTypeName(MDMInfo[] infos, FieldInfo field, boolean fq) {
        String type = field.type;
        if (type.equalsIgnoreCase("byte")) {
            return "u8";
        }
        if (type.equalsIgnoreCase("char")) {
            return "char";
        }
        if (type.equalsIgnoreCase("bool")) {
            return "bool";
        }
        if (type.equalsIgnoreCase("int16")) {
            return "i16";
        }
        if (type.equalsIgnoreCase("uint16")) {
            return "u16";
        }
        if (type.equalsIgnoreCase("int32")) {
            return "i32";
        }
        if (type.equalsIgnoreCase("uint32")) {
            return "u32";
        }
        if (type.equalsIgnoreCase("int64")) {
            return "i64";
        }
        if (type.equalsIgnoreCase("real32")) {
            return "f32";
        }
        if (type.equalsIgnoreCase("real64")) {
            return "f64";
        }
        if (type.equalsIgnoreCase("string")) {
            // Converting to a vector of bytes offers the most
            // general-purpose API, but is perhaps less convenient
            // than converting to String or CString. Revisit if these
            // conversions get awkward
            return "Vec<u8>";
        }
        if (type.equals(MDMInfo.LMCP_OBJECT_NAME)) {
            throw new RuntimeException("avtas::lmcp::Object type not supported for Rust");
        }
        else if (field.isStruct) {
            getTypeName(infos, field, fq);
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

    private static String getSeriesModule(MDMInfo[] infos, String series_name) {
        MDMInfo i = MDMReader.getMDM(series_name, infos);
        return i.namespace.replaceAll("/", "::");
    }

    private static String getSeriesSnake(MDMInfo[] infos, String series_name) {
        MDMInfo i = MDMReader.getMDM(series_name, infos);
        return i.namespace.replaceAll("/", "_").toLowerCase();
    }

    private static String getSeriesCamel(MDMInfo[] infos, String series_name) {
        MDMInfo i = MDMReader.getMDM(series_name, infos);
        String[] components = i.namespace.split("/");
        StringBuilder sb = new StringBuilder();
        for (String c: components) {
            sb.append(c.substring(0, 1).toUpperCase());
            sb.append(c.substring(1));
        }
        return sb.toString();
    }

    private static String getTypeName(MDMInfo[] infos, FieldInfo field, boolean fullyQualified) {
        if (fullyQualified) {
            return getResolvedTypeName(infos, field);
        } else {
            return getShortTypeName(infos, field);
        }
    }

    private static String getResolvedTypeName(MDMInfo[] infos, FieldInfo field) {
        String type = field.type;
        if (field.isStruct || field.isEnum) {
            if (field.type.equals(MDMInfo.LMCP_OBJECT_NAME)) {
                throw new RuntimeException("avtas::lmcp::Object type not supported for Rust");
            }
            return getSeriesNamespace(infos, field.seriesName) + type;
        } else {
            return getRustTypeName(infos, field, true);
        }
    }

    /**
     * Look up the "short" name of a type. Since our modules already
     * use the modules containing types they depend upon, that means
     * we just qualify them with the names of those modules, not the
     * whole path.
     */
    private static String getShortTypeName(MDMInfo[] infos, FieldInfo field) {
        String type = field.type;
        String base;
        if (field.isStruct || field.isEnum) {
            if (type.equals(MDMInfo.LMCP_OBJECT_NAME)) {
                throw new RuntimeException("avtas::lmcp::Object type not supported for Rust");
            }
            base = String.format("%s::%s", snake_case(type), type);
        }
        else {
            base = getRustTypeName(infos, field, false);
        }
        if (field.isArray) {
            return String.format("Vec<%s>", base);
        } else if (field.isOptional) {
            return String.format("Option<%s>", base);
        } else {
            return base;
        }
    }

    /**
     * Like getShortTypeName, but turns structs into boxed trait objects
     */
    private static String getBoxedTypeName(MDMInfo[] infos, FieldInfo field) {
        String type = field.type;
        String base;
        String prefix = getSeriesModule(infos, field.seriesName);
        if (field.isStruct) {
            if (type.equals(MDMInfo.LMCP_OBJECT_NAME)) {
                throw new RuntimeException("avtas::lmcp::Object type not supported for Rust");
            }
            base = String.format("Box<::%s::%s::%sT>", prefix, snake_case(type), type);
        } else if (field.isEnum) {
            base = String.format("::%s::%s::%s", prefix, snake_case(type), type);
        } else {
            base = getRustTypeName(infos, field, false);
        }
        if (field.isArray) {
            return String.format("Vec<%s>", base);
        } else if (field.isOptional) {
            return String.format("Option<%s>", base);
        } else {
            return base;
        }
    }

    private static boolean can_copy_struct(MDMInfo[] infos, StructInfo st0) throws Exception {
        for (StructInfo st : MDMInfo.getAllParents(infos, st0)) {
            for (FieldInfo field : st.fields) {
                if (field.isStruct ||
                    field.isArray ||
                    field.isLargeArray ||
                    field.isMap ||
                    field.type.equals("string")) {
                    return false;
                }
            }
        }
        return true;
    }

    private static String snake_case(String str0) {
        StringBuilder sb = new StringBuilder();
        // add a dummy lowercase character to the end to make room for the sliding window
        char[] str = (str0 + "x").toCharArray();
        // loop with sliding window of 2 chars, so stop before the last index
        for (int i = 0; i < str.length - 1; i++) {
            sb.append(Character.toLowerCase(str[i]));
            int j = i + 1;
            if (Character.isLowerCase(str[i]) && Character.isUpperCase(str[j])) {
                sb.append('_');
            }
        }
        return sb.toString();
    }
};
