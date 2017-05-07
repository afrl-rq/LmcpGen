// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

package misc;

import avtas.lmcp.lmcpgen.StructInfo;
import avtas.lmcp.lmcpgen.FieldInfo;
import avtas.lmcp.lmcpgen.MDMInfo;
import avtas.lmcp.lmcpgen.MDMReader;
import java.io.File;
import java.io.FileWriter;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JOptionPane;

/**
 * Creates a Thrift file from an MDM file
 * @author matt
 */
public class ThriftConverter {
    
    public static void main(String[] args) {
        /*JFileChooser c = new JFileChooser();
        c.setDialogTitle("Open an MDM File");
        int ans = c.showOpenDialog(null);
        if (ans == JFileChooser.CANCEL_OPTION) {
            return;
        }
        File mdmFile = c.getSelectedFile();
        c.setDialogTitle("Save Thrift file as...");
        ans = c.showSaveDialog(null);
        if (ans == JFileChooser.CANCEL_OPTION) {
            return;
        }
        File thriftFile = c.getSelectedFile();
        
        convert(mdmFile, thriftFile);*/

        convert(new File("c:/matt/AMASE/Tentative UAVCC_2009-07-13.xml"),
                new File("c:/Documents and Settings/matt/Desktop/Tentative UAVCC_2009-07-13.thrift"));
    }

    public static void convert(File mdmFile, File thriftFile) {
        try {

            MDMInfo info = MDMReader.readMDM(mdmFile);

            reorderMsgs(info);

            if (thriftFile.exists()) {
                if (JOptionPane.showConfirmDialog(null, "Thrift File Exists.  Overwrite?") == JOptionPane.NO_OPTION) {
                    return;
                }
            }

            FileWriter fw = new FileWriter(thriftFile);

            fw.write("/**" + cleanComment(info.comment) + "*/\n");

            for (StructInfo dt : info.structs) {

                if (!dt.comment.isEmpty()) {
                    fw.write("\n/**" + cleanComment(dt.comment) + "*/\n" );
                }
                fw.write("struct " + dt.name + " {\n");

                for(int i=0; i<dt.fields.length; i++) {
                    FieldInfo f = dt.fields[i];
                    if (!f.comment.isEmpty()){
                        fw.write("    /** " + cleanComment(f.comment) + " */\n");
                    }
                    if (f.isArray) {
                        fw.write("    " + (i+1) + ": optional list<" + convertType(f.type) + "> " + f.name);
                    }
                    else {
                        fw.write("    " + (i+1) + ": optional " + convertType(f.type) + " " + f.name);
                    }
                    //if (!f.defaultVal.isEmpty() && !f.defaultVal.equalsIgnoreCase("null")) {
                    //    fw.write(" = " + f.defaultVal);
                    //}
                    fw.write(", \n");
                }
                fw.write("}\n\n");

            }

            fw.close();

        } catch (Exception ex) {
            Logger.getLogger(ThriftConverter.class.getName()).log(Level.SEVERE, null, ex);
        }

    }

    public static void reorderMsgs(MDMInfo info) {

        Vector<StructInfo> dtList = new Vector<StructInfo>();

        for(StructInfo dt : info.structs) {
            for(FieldInfo f : dt.fields) {
                if ( f.isStruct ) {
                    StructInfo type = MDMInfo.getStructByName(new MDMInfo[] {info}, f);
                    if (!dtList.contains(type)) {
                        dtList.insertElementAt(type, 0);
                    }
                }
            }
            if (!dtList.contains(dt)) {
                dtList.add(dt);
            }
        }
        info.structs = dtList.toArray(new StructInfo[] {});

    }

    public static String cleanComment(String comment) {
        comment = comment.replaceAll("[\n\r\f]+", "");
        comment = comment.replaceAll("\\s+", " ");
        return comment;
    }

    public static String convertType(String type) {
        if (type.equalsIgnoreCase("uint16")) {
            return "i32";
        }
        if (type.equalsIgnoreCase("uint32")) {
            return "i64";
        }
        if (type.equalsIgnoreCase("byte")) {
            return "byte";
        }
        if (type.equalsIgnoreCase("bool")) {
            return "bool";
        }
        if (type.equalsIgnoreCase("int16")) {
            return "i16";
        }
        if (type.equalsIgnoreCase("int32")) {
            return "i32";
        }
        if (type.equalsIgnoreCase("int64")) {
            return "i64";
        }
        if (type.equalsIgnoreCase("real32")) {
            return "double";
        }
        if (type.equalsIgnoreCase("real64")) {
            return "double";
        }
        if (type.equalsIgnoreCase("string")) {
            return "string";
        }
        return type;
    }

}
