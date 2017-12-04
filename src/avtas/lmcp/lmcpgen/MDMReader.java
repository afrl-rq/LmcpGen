// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

/*
 * MDMReader.java
 *
 * Created on August 7, 2007, 7:05 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package avtas.lmcp.lmcpgen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.UUID;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 *
 * @author default
 */
public class MDMReader {
    
    public static String DECIMAL_MATCHER = "[-+]?[0-9]*\\.?[0-9]*([eE][-+]?[0-9]*)?";
    public static String INTEGER_MATCHER = "[-+]?[0-9]*";

    public static MDMInfo readMDM(File file) throws Exception {

        if (file == null || !file.exists()) {
            return null;
        }

        MDMErrorHandler errorHandler = new MDMErrorHandler();

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setValidating(true);

        DocumentBuilder builder = factory.newDocumentBuilder();
        builder.setErrorHandler(errorHandler);

        final InputStream is = LmcpGen.class.getResourceAsStream("MDM.DTD");

        EntityResolver resolver = new EntityResolver() {
            public InputSource resolveEntity(String publicId, String systemId) {
                if (systemId.toUpperCase().contains("MDM.DTD")) {
                    return new InputSource(is);
                }
                return null;
            }
        };
        builder.setEntityResolver(resolver);

        Document doc = builder.parse(file);
        MDMInfo info = getFromXML(doc.getDocumentElement());

        BufferedReader reader = new BufferedReader(new FileReader(file));
        StringBuffer filebuf = new StringBuffer();
        while (reader.ready()) {
            filebuf.append(reader.readLine());
            filebuf.append("\n");
        }
        reader.close();
        info.mdmString = filebuf.toString();

        if (errorHandler.error) {
            throw new Exception(errorHandler.buf.toString());
        }

        return info;
    }

    /**
     * returns a new MDMInfo object from the XML Node
     */
    public static MDMInfo getFromXML(Node node) throws Exception {

        MDMInfo info = new MDMInfo();

        info.seriesName = XMLUtil.get(node, "SeriesName", "");
        info.guid = UUID.nameUUIDFromBytes(info.seriesName.getBytes()).toString().toUpperCase(); // UUID.randomUUID().toString().toUpperCase();
        info.seriesNameAsLong = seriesNameToLong(info.seriesName);
        int startId = 1;
        info.namespace = XMLUtil.get(node, "Namespace", "");
        info.comment = XMLUtil.get(node, "Comment", "").replaceAll("[\n\r\f]+", "");
        info.version = XMLUtil.getInt(node, "Version", 0);

        if (info.comment.isEmpty()) {
            info.comment = getComment(node).replaceAll("[\n\r\f]+", "");
        }

        info.structs = fillStructs(XMLUtil.getList(node, "StructList", "Struct"), info);

        // Determine start ID by looking for the maximum specified ID (0 by default)
        for (int i = 0; i < info.structs.length; i++) {
            startId = Math.max(startId, info.structs[i].id + 1);
        }

        // Assign IDs for structs that do not yet have one
        int idOffset = 0;
        for (int i = 0; i < info.structs.length; i++) {
            if (info.structs[i].id < 1) {
                info.structs[i].id = startId + idOffset;
                idOffset++;
            }
        }

        info.enums = fillEnums(XMLUtil.getList(node, "EnumList", "Enum"), info);

        return info;
    }

    public static String getComment(Node node) {
        Node comment = node.getPreviousSibling();
        while (comment != null && comment.getNodeType() != Node.ELEMENT_NODE) {
            if (comment.getNodeType() == Node.COMMENT_NODE) {
                return comment.getNodeValue();
            }
            comment = comment.getPreviousSibling();
        }
        return "";
    }

    public static StructInfo[] fillStructs(Node[] list, MDMInfo mdm) throws Exception {

        StructInfo[] retList = new StructInfo[list.length];

        for (int i = 0; i < list.length; i++) {
            StructInfo struct = new StructInfo();
            retList[i] = struct;
            struct.namespace = mdm.namespace;
            struct.seriesName = mdm.seriesName;
            struct.name = XMLUtil.getAttribute(list[i], "Name", "");
            struct.extends_name = XMLUtil.getAttribute(list[i], "Extends", "");
            if (struct.extends_name.contains("/")) {
                String[] split = struct.extends_name.split("/");
                struct.extends_name = split[1];
                struct.extends_series = split[0];
            }
            else {
                struct.extends_series = XMLUtil.getAttribute(list[i], "Series", mdm.seriesName);
            }
            if (struct.extends_series.isEmpty())
                struct.extends_series = mdm.seriesName;

            struct.comment = XMLUtil.get(list[i], "Comment", "").replaceAll("[\n\r\f]+", "");
            if (struct.comment.isEmpty()) {
                struct.comment = getComment(list[i]).replaceAll("[\n\r\f]+", "");
            }

            // Optional explicit ID
            String idStr = XMLUtil.getAttribute(list[i], "ID", "");
            if (!idStr.isEmpty()) {
                struct.id = Integer.parseInt(idStr);
            }


            Node[] fieldNodes;
            fieldNodes = XMLUtil.getChildren(list[i], "Field");

            struct.fields = new FieldInfo[fieldNodes.length];
            for (int j = 0; j < fieldNodes.length; j++) {

                FieldInfo f = new FieldInfo();
                struct.fields[j] = f;

                f.name = XMLUtil.getAttribute(fieldNodes[j], "Name", "");

                String type = XMLUtil.getAttribute(fieldNodes[j], "Type", "");
                type = type.replaceAll("\\s+", "");

                if (type.contains("/")) {
                    String[] split = type.split("/");
                    f.seriesName = split[0];
                    type = split[1];
                }
                else {
                    f.seriesName = XMLUtil.getAttribute(fieldNodes[j], "Series", mdm.seriesName);
                }

                if (f.seriesName.isEmpty()) f.seriesName = mdm.seriesName;

                if (type.endsWith("[]")) {
                    f.type = type.substring(0, type.length() - 2);
                    f.length = -1;
                    f.isArray = true;
                }
                else if (type.endsWith("]")) {
                    int index = type.indexOf("[");
                    f.type = type.substring(0, index);
                    f.length = Integer.valueOf(type.substring(index + 1, type.length() - 1));
                    f.isArray = true;
                }
                else if (type.startsWith("<") && type.endsWith(">")) {
                    String[] types = type.substring(0, type.length() - 1).substring(1).split(",");
                    if (types.length != 2) {
                        throw new Exception("badly formed map type for field " + f.name + " in struct " + struct.name);
                    }
                    f.type = types[0].trim() + "," + types[1].trim();

                }
                else {
                    f.length = -1;
                    f.isArray = false;
                    f.isMap = false;
                    f.isScalar = true;
                    f.type = type;
                }

                f.comment = XMLUtil.get(fieldNodes[j], "Comment", "").replaceAll("[\n\r\f]+", "");
                if (f.comment.isEmpty()) {
                    f.comment = getComment(fieldNodes[j]).replaceAll("[\n\r\f]+", "");
                }

                f.units = XMLUtil.getAttribute(fieldNodes[j], "Units", "");

                f.defaultVal = XMLUtil.getAttribute(fieldNodes[j], "Default", "");

                checkDefault(struct, f);

                // large array tag
                f.isLargeArray = Boolean.valueOf(XMLUtil.getAttribute(fieldNodes[j], "LargeArray", "false"));
                
                // optional
                f.isOptional = Boolean.valueOf(XMLUtil.getAttribute(fieldNodes[j], "Optional", "false"));
                
                // max length of array
                String arrLen = XMLUtil.getAttribute(fieldNodes[j], "MaxArrayLength", "0");
                if(arrLen != "") {
                    f.maxArrayLength = Integer.valueOf(arrLen);
                }
            }
        }

        return retList;

    }

    public static EnumInfo[] fillEnums(Node[] list, MDMInfo mdm) {

        ArrayList<EnumInfo> retList = new ArrayList<EnumInfo>();

        for (int i = 0; i < list.length; i++) {
            EnumInfo enInfo = new EnumInfo();
            retList.add(enInfo);
            enInfo.name = XMLUtil.getAttribute(list[i], "Name", "");
            enInfo.comment = XMLUtil.get(list[i], "Comment", "").replaceAll("[\n\r\f]+", "");
            if (enInfo.comment.isEmpty()) {
                enInfo.comment = getComment(list[i]).replaceAll("[\n\r\f]+", "");
            }
            enInfo.namespace = mdm.namespace;
            enInfo.seriesName = mdm.seriesName;


            Node[] fieldNodes;
            fieldNodes = XMLUtil.getChildren(list[i], "Entry");
            for (int j = 0; j < fieldNodes.length; j++) {
                EnumInfo.EnumEntry entry = new EnumInfo.EnumEntry();
                entry.name = XMLUtil.getAttribute(fieldNodes[j], "Name", "");
                String val = String.valueOf(j);
                String valAttr = XMLUtil.getAttribute(fieldNodes[j], "Value", "");
                if (!valAttr.isEmpty()) {
                    val = valAttr;
                }
                entry.value = val;
                entry.comment = getComment(fieldNodes[j]).replaceAll("[\n\r\f]+", "");
                enInfo.entries.add(entry);
            }
        }
        return retList.toArray(new EnumInfo[]{});
    }

    /**
     * Checks to make sure that all struct references and enum references
     * actually exist and are valid.
     */
    public static void checkMDMs(MDMInfo[] infos) throws Exception {

        for (MDMInfo info : infos) {
            if(info.seriesNameAsLong == 0)
            {
                continue;
            }
            //check all fields, set to enum or struct if not primitive
            for (StructInfo st : info.structs) {
                // setup structs and enum annotations on fields
                for (FieldInfo f : st.fields) {
                    if (!f.type.matches(MDMInfo.primitive_matcher)) {
                        if (isStruct(f.type, f.seriesName, infos)) {
                            f.isStruct = true;
                        }
                        else if (isEnum(f.type, f.seriesName, infos)) {
                            f.isEnum = true;
                            EnumInfo ei = MDMInfo.getEnumByName(infos, f);
                            if (f.defaultVal.isEmpty()) {
                                f.defaultVal = ei.entries.get(0).name;
                            }
                        }
                        else {
                            throw new Exception("struct " + st.name + "in series " + st.seriesName
                                    + " contains a field of non-existent type " + f.type);
                        }
                    }
                }
                if (!st.extends_name.isEmpty()) {
                    if (!isStruct(st.extends_name, st.extends_series, infos)) {
                        throw new Exception("struct " + st.name + " in series " + st.seriesName
                                + " extends non-existent struct " + st.extends_name);
                    }
                    // check for circular inheritence
                    StructInfo extStruct = MDMInfo.getParentType(infos, st);
                    while (extStruct != null) {
                        if (extStruct.extends_name.equals(st.name)) {
                            throw new Exception("struct " + st.name
                                    + " has circular inheritance with " + extStruct.name);
                        }
                        extStruct = MDMInfo.getParentType(infos, extStruct);
                    }
                }

                for (EnumInfo en : info.enums) {
                    if (en.name.equals(st.name)) {
                        throw new Exception("struct and Enum with same name: " + st.name + " in MDM: " + st.seriesName);
                    }
                }

            }
        }
    }

    public static void checkDefault(StructInfo s, FieldInfo f) throws Exception {
        if (MDMInfo.isNumber(f.type) && !f.isArray) {
            if (f.defaultVal.isEmpty()) {
                f.defaultVal = "0";
            }
            else if (f.type.startsWith("real")) {
                try {
                    Double.valueOf(f.defaultVal);
                } catch (NumberFormatException ex) {
                    throw new Exception("Bad Default Value for " + f.name + " in " + s.name);
                }
            }
            else if (f.type.startsWith("int") ) {
                try {
                    Long.valueOf(f.defaultVal);
                } catch (NumberFormatException ex) {
                    throw new Exception("Bad Default Value for " + f.name + " in " + s.name);
                }
            }
            else if (f.type.startsWith("uint")) {
                try {
                    long val = Long.valueOf(f.defaultVal);
                    if (val < 0)
                        throw new Exception("Bad Default Value for " + f.name + " in " + s.name + ". Unsigned values must be > 0.");
                } catch (NumberFormatException ex) {
                    throw new Exception("Bad Default Value for " + f.name + " in " + s.name);
                }
            }
            else if (f.type.equals("byte")) {
                try {
                    int val = Integer.valueOf(f.defaultVal);
                    if (val < 0 || val > 255) {
                        throw new Exception("Bad Default Value for " + f.name + " in " + s.name);
                    }
                } catch (NumberFormatException ex) {
                    throw new Exception("Bad Default Value for " + f.name + " in " + s.name);
                }
            }
            else if (f.type.equals("bool")) {
                try {
                    Boolean.valueOf(f.defaultVal);
                } catch (NumberFormatException ex) {
                    throw new Exception("Bad Default Value for " + f.name + " in " + s.name);
                }
            }
        }
        else if (f.type.equals(MDMInfo.LMCP_OBJECT_NAME)) {
            f.defaultVal = "null";
        }
    }

    protected static MDMInfo getMDM(String seriesName, MDMInfo[] mdms) {
        for (MDMInfo i : mdms) {
            if (i.seriesName.equals(seriesName)) {
                return i;
            }
        }
        return null;
    }

    protected static boolean isStruct(String structName, String seriesName, MDMInfo[] mdms) {
        if (structName.equals(MDMInfo.LMCP_OBJECT_NAME)) {
            return true;
        }
        MDMInfo i = getMDM(seriesName, mdms);
        if (i == null) return false;
        return i.isStruct(structName);
    }

    protected static boolean isEnum(String enumName, String seriesName, MDMInfo[] mdms) {
        MDMInfo i = getMDM(seriesName, mdms);
        if (i == null) return false;
        return i.isEnum(enumName);
    }

    public static long seriesNameToLong(String seriesName) throws Exception {
        if (seriesName.length() > 8) {
            throw new Exception("Series Name \"" + seriesName + "\" is too long (max 8 chars)");
        }
        for (int i = 0; i < 8; i++) {
            seriesName += "\0";
        }
        ByteBuffer buf = ByteBuffer.wrap(seriesName.getBytes());
        return buf.getLong();
    }
}

class MDMErrorHandler implements ErrorHandler {

    StringBuffer buf = new StringBuffer();
    boolean error = false;

    public MDMErrorHandler() {
    }

    public void error(SAXParseException ex) throws SAXException {
        buf.append("Error: (Line " + ex.getLineNumber() + ") " + ex.getMessage() + "\n");
        error = true;
    }

    public void fatalError(SAXParseException ex) throws SAXException {
        error = true;
        buf.append("Fatal Error: (Line " + ex.getLineNumber() + ") " + ex.getMessage() + "\n");
        throw new SAXException(buf.toString());
    }

    public void warning(SAXParseException ex) throws SAXException {
        buf.append("Warning: (Line " + ex.getLineNumber() + ") " + ex.getMessage() + "\n");
    }
}
