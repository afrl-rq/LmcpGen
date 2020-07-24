// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
//
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

/*
 * LmcpGen.java
 *
 * Created on July 28, 2007, 6:48 PM
 *
 * The LMCP generator is used to automatically produce code for the LMCP system.  In order to produce code, an MDM file must be
 * specified as well as a template file (.tl file).  See the LMCP autocoder specification for details.
 */
package avtas.lmcp.lmcpgen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.nio.file.Files;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.Set;
import java.util.Vector;

/**
 *
 * @author Matt Duquette AFRL/VACD
 */
public class LmcpGen {

    static String ws = "    ";

    /** makes a new code package based on the mdm passed.  The methodClass is any class on the classpath that implements the methods
     *  specified in the template files
     */
    public static void makePackage(MDMInfo info, File outputDir, URL templateFile, Class methodClass) throws Exception {
        makePackage(new MDMInfo[]{info}, outputDir, templateFile, methodClass);
    }

    public static void makePackage(MDMInfo[] infos, File outputDir, URL templateFile, Class methodClass) throws Exception {

        MDMReader.checkMDMs(infos);

        writeMDMs(infos, outputDir);

        BufferedReader reader = new BufferedReader(new InputStreamReader(templateFile.openStream()));

        while (reader.ready()) {
            String line = reader.readLine();

            if (line.startsWith("#")) {
                continue;
            }

            String[] splits = line.split("\\s+");


            if (splits.length >= 3) {

                if (splits[0].equals("ONCE")) {
                    // call methods in the methods class to replace string tokens
                    String fname = replaceTags(splits[2], null, methodClass, infos, null, null, null);
                    URL f = new URL(templateFile, splits[1]);
                    String outString = readFile(f);
                    File outfile = new File(outputDir, fname);
                    outString = replaceTags(outString, outfile, methodClass, infos, null, null, null);
                    outfile.getParentFile().mkdirs();
                    writeFile(outfile, outString);

                }
                if (splits[0].equals("PER_MDM")) {

                    for (MDMInfo info : infos) {
                        if(info.seriesNameAsLong == 0)
                        {
                            continue;
                        }
                        String fname = replaceTags(splits[2], null, methodClass, infos, info, null, null);
                        URL f = new URL(templateFile, splits[1]);
                        String outString = readFile(f);
                        File outfile = new File(outputDir, fname);
                        outString = replaceTags(outString, outfile, methodClass, infos, info, null, null);
                        outfile.getParentFile().mkdirs();
                        writeFile(outfile, outString);
                    }
                }
                else if (splits[0].equals("PER_STRUCT")) {

                    URL f = new URL(templateFile, splits[1]);
                    String fileString = readFile(f);
                    for (MDMInfo info : infos) {
                        if(info.seriesNameAsLong == 0)
                        {
                            continue;
                        }
                        for (int i = 0; i < info.structs.length; i++) {
                            // call methods in the methods class to replace string tokens
                            String fname = replaceTags(splits[2], null, methodClass, infos, info, info.structs[i], null);
                            File outfile = new File(outputDir, fname);
                            String outString = replaceTags(fileString, outfile, methodClass, infos, info, info.structs[i], null);
                            outfile.getParentFile().mkdirs();
                            writeFile(outfile, outString);
                        }
                    }
                }
                else if (splits[0].equals("PER_ENUM")) {

                    URL f = new URL(templateFile, splits[1]);
                    String fileString = readFile(f);
                    for (MDMInfo info : infos) {
                        if(info.seriesNameAsLong == 0)
                        {
                            continue;
                        }
                        for (int i = 0; i < info.enums.length; i++) {
                            // call methods in the methods class to replace string tokens
                            String fname = replaceTags(splits[2], null, methodClass, infos, info, null, info.enums[i]);
                            File outfile = new File(outputDir, fname);
                            String outString = replaceTags(fileString, outfile, methodClass, infos, info, null, info.enums[i]);
                            outfile.getParentFile().mkdirs();
                            writeFile(outfile, outString);
                        }
                    }
                }
		else if (splits[0].equals("PER_NS_SUBDIR")) {
		    URL f = new URL(templateFile, splits[1]);
		    String fileString = readFile(f);
		    Set<String> subdirs = MDMInfo.getNamespaceSubdirs(infos);
		    for (String subdir : subdirs) {
			// make a fake MDMInfo for this part of the namespace
			MDMInfo dummyInfo = new MDMInfo();
			dummyInfo.namespace = subdir;
			String fname = replaceTags(splits[2], null, methodClass, infos, dummyInfo, null, null);
			File outfile = new File(outputDir, fname);
			String outString = replaceTags(fileString, outfile, methodClass, infos, dummyInfo, null, null);
			outfile.getParentFile().mkdirs();
			writeFile(outfile, outString);
		    }
		}
            }
        }
    }

    /** writes each MDM file to the given directory */
    public static void writeMDMs(MDMInfo[] mdms, File dir) throws Exception {
        for (MDMInfo info : mdms) {
            if(info.seriesNameAsLong != 0)
            {
                File outfile = new File(dir, info.seriesName + ".xml");
                outfile.getParentFile().mkdirs();
                writeFile(outfile, info.mdmString);
            }
        }
    }

    /** reads a file from a URL (useful when looking into jar files) and returns a String with that file's contents. */
    public static String readFile(URL url) throws Exception {

        StringBuffer buf = new StringBuffer();

        BufferedReader reader = new BufferedReader(new InputStreamReader(url.openStream()));
        while (reader.ready()) {
            buf.append(reader.readLine());
            buf.append("\n");
        }
        reader.close();

        return buf.toString();
    }

    public static String FileIntoString(File file) throws Exception {
        return new String(Files.readAllBytes(file.toPath()));
    }



    /** simply writes the contents of a string to a file if the existing file isn't already identical */
    public static boolean writeFile(File file, String contents) throws Exception {
        if(file.exists())
        {
            String oldcontents = FileIntoString(file);
            if(!oldcontents.equals(contents))
            {
                FileWriter writer = new FileWriter(file);
                writer.write(contents);
                writer.close();
            }
        }
        else
        {
            FileWriter writer = new FileWriter(file);
            writer.write(contents);
            writer.close();
        }
        return true;
    }

    /** calls the methodClass methods based on strings encountered in the file.  Each tag has a -&lt;\\w*&gt;- format */
    public static String replaceTags(String input, File outfile, Class<?> methodClass,
            MDMInfo[] infos, MDMInfo info, StructInfo st, EnumInfo en) throws Exception {
        String outputStr = input;
        String[] strs = splitString(input);


        if (!strs[2].equals("")) {

            Method method = methodClass.getMethod(strs[1], MDMInfo[].class, MDMInfo.class, File.class,
                    StructInfo.class, EnumInfo.class, String.class);
//            System.err.println("accessing method: " + method.getName());
            try {
                outputStr = strs[0] + (String) method.invoke(null, infos, info, outfile, st, en, strs[3]) + strs[2];
                outputStr = replaceTags(outputStr, outfile, methodClass, infos, info, st, en);
            } catch (InvocationTargetException ex) {
                throw new Exception("Method calling exception.  "
                        + "Trying to access method name: " + method.getName() + " for file: " + outfile.getName(),
                        ex);
            } catch (Exception ex2) {
                System.out.println(ex2.getMessage());
                throw new Exception("Error writing file: " + outfile.getName() + " using method " + method.getName());
            }

        }

        return outputStr;
    }

    /** splits a string up and returns an array of [ head, method name, tail, whitespace ] */
    public static String[] splitString(String input) throws Exception {

        String tailStr = "";
        String headStr = "";
        String ws = "";
        String methodName = "";

        String[] firstSplit = input.split("-<", 2);

        if (firstSplit.length == 2) {
            tailStr = firstSplit[1];
            String[] tailSplit = tailStr.split(">-", 2);
            methodName = tailSplit[0];
            if (tailSplit.length < 2) {
                throw new Exception(tailSplit[0]);
            }
            tailStr = tailSplit[1];

            headStr = new StringBuffer(firstSplit[0]).reverse().toString();

            String[] secSplit = headStr.split("[^ \\t]+", 2);
            if (secSplit.length == 2) {
                ws = secSplit[0];
                headStr = headStr.replaceFirst(ws, "");
            }
            else {
                headStr = secSplit[0];
            }
            headStr = new StringBuffer(headStr).reverse().toString();
        }
        else {
            headStr = firstSplit[0];
        }

        return new String[]{headStr, methodName, tailStr, ws};
    }

    public static void main(String[] args) {

        String outputDir = null;
        Vector<String> mdmFiles = new Vector<String>();
        String methodClassName = null;
        URL template = null;

        for (int i = 0; i < args.length; i += 2) {
            if (args[i].equalsIgnoreCase("-dir")) {
                outputDir = args[i + 1];
            }
            else if(args[i].equalsIgnoreCase("-mdmdir")) {
                String mdmDir = args[i+1];
                File[] files = new File(mdmDir).listFiles();
                for (File file : files) {
                    if (file.isFile() &&
                            ( file.getName().toLowerCase().endsWith(".xml") ||
                              file.getName().toLowerCase().endsWith(".mdm")))
                    {
                        mdmFiles.add(file.getAbsolutePath());
                    }
                }
            }
            else if (args[i].equalsIgnoreCase("-mdm")) {
                mdmFiles.add(args[i + 1]);
            }
            else if (args[i].equalsIgnoreCase("-methods")) {
                methodClassName = args[i + 1];
            }
            else if (args[i].equalsIgnoreCase("-template")) {
                template = getURL(args[i + 1]);
            }
            else if (args[i].equalsIgnoreCase("-java")) {
                template = LmcpGen.class.getResource("/templates/java.tl");
                methodClassName = "avtas.lmcp.lmcpgen.JavaMethods";
                i -= 1;
            }
            else if (args[i].equalsIgnoreCase("-cpp")) {
                template = LmcpGen.class.getResource("/templates/cpp.tl");
                methodClassName = "avtas.lmcp.lmcpgen.CppMethods";
                i -= 1;
            }
            else if (args[i].equalsIgnoreCase("-cs")) {
                template = LmcpGen.class.getResource("/templates/cs.tl");
                methodClassName = "avtas.lmcp.lmcpgen.CsMethods";
                i -= 1;
            }
            else if (args[i].equalsIgnoreCase("-py")) {
                template = LmcpGen.class.getResource("/templates/python.tl");
                methodClassName = "avtas.lmcp.lmcpgen.PythonMethods";
                i -= 1;
            }
            else if (args[i].equalsIgnoreCase("-rs")) {
                template = LmcpGen.class.getResource("/templates/rust.tl");
                methodClassName = "avtas.lmcp.lmcpgen.RustMethods";
                i -= 1;
            }
            else if (args[i].equalsIgnoreCase("-ada")) {
                template = LmcpGen.class.getResource("/templates/ada.tl");
                methodClassName = "avtas.lmcp.lmcpgen.AdaMethods";
                i -= 1;
            }
            else if (args[i].equalsIgnoreCase("-xsd")) {
                template = LmcpGen.class.getResource("/templates/xsd.tl");
                methodClassName = "avtas.lmcp.lmcpgen.XsdMethods";
                i -= 1;
            }
            else if (args[i].equalsIgnoreCase("-doc")) {
                template = LmcpGen.class.getResource("/templates/doc.tl");
                methodClassName = "avtas.lmcp.lmcpgen.DocMethods";
                i -= 1;
            }
            else if (args[i].equalsIgnoreCase("-aadl")) {
                template = LmcpGen.class.getResource("/templates/aadl.tl");
                methodClassName = "avtas.lmcp.lmcpgen.AadlMethods";
                i -= 1;
            }
            else if (args[i].equalsIgnoreCase("-help")) {
                System.out.println(getHelpString());
                return;
            }
            else if (args[i].equalsIgnoreCase("-checkMDM")) {
                checkMDM(args[i + 1]);
                return;
            }
        }

        if (mdmFiles.size() == 0 || outputDir == null || methodClassName == null || template == null) {
            System.out.println("\nError reading inputs.  Use -help for more info.\n");
            System.out.println(getHelpString());
            System.exit(1);
        }


        try {
            MDMInfo[] infoArray = new MDMInfo[mdmFiles.size()];
            for (int i = 0; i < mdmFiles.size(); i++) {
                infoArray[i] = MDMReader.readMDM(new File(mdmFiles.get(i)));
            }
            LmcpGen.makePackage(infoArray, new File(outputDir),
                    template, Class.forName(methodClassName));
        } catch (Exception ex) {
            throw new IllegalArgumentException(ex);
        }


    }

    private static URL getURL(String name) {

        File f = new File(name);
        if (f.exists()) {
            try {
                return f.toURI().toURL();
            }
            catch (Exception ex) {
                return null;
            }
        }
        else if (!f.isAbsolute()) {
            URL url = LmcpGen.class.getResource('/' + name);
            if (url != null) return url;
        }
        System.out.println("\nCan't find " + name);
        return null;
    }

    private static String getHelpString() {

        StringBuffer buf = new StringBuffer();
        buf.append("usage: java -jar LmcpGen.jar [options]\n\n");
        buf.append("options:\n");
        buf.append("-mdm <filename> path to the MDM XML file.  Multiple MDMs can be specified by repeating the -mdm tag.\n\n");
        buf.append("-mdmdir <directory path> to directory containing multiple MDM XML files.\n\n");
        buf.append("-java Adds proper template and method name for Java output.\n\n");
        buf.append("-cpp Adds proper template and method name for C++ output.\n\n");
        buf.append("-cs Adds proper template and method name for C# output.\n\n");
        buf.append("-py Adds proper template and method name for Python output.\n\n");
        buf.append("-aadl Adds proper template and method name for AADL output.\n\n");
        buf.append("-xsd Adds proper template and method name for XML schema output.\n\n");
        buf.append("-doc Adds proper template and method name for documentation output.\n\n");
        buf.append("-dir <directory path> path to the directory where files are to be written\n");
        buf.append("     The directory must exist.\n\n");
        buf.append("-methods <method class name> Specifies the fullly resolved \n");
        buf.append("     class name.  The class must be on the classpath.\n\n");
        buf.append("-template <template file> The .tl file used as an index for file.\n ");
        buf.append("-checkMDM <mdm file> Checks the MDM file for errors and exits ");
        buf.append("creation. \n\n");

        return buf.toString();
    }

    private static void checkMDM(String string) {
        try {
            MDMReader.readMDM(new File(string));
        } catch (Exception ex) {
            System.out.println(ex.getMessage());
        }

    }
}
