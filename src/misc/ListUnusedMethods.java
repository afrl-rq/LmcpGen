// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

package misc;

import avtas.lmcp.lmcpgen.JavaMethods;
import avtas.lmcp.lmcpgen.LmcpGen;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Goes through a Methods class and finds methods that are not being used during the
 * LMCP generation process.
 *
 * @author Matt Duquette
 */
public class ListUnusedMethods {
    
    public static ArrayList<String> listUnusedMethods(Class methodsClass, File templateFile) {
        try {
            List<Method> methods = Arrays.asList(methodsClass.getDeclaredMethods());
            ArrayList<String> usedMethods = new ArrayList<String>();
            
            String str = LmcpGen.readFile(templateFile.toURI().toURL());
            usedMethods.addAll(getMethodNames(str));
            
            BufferedReader reader = new BufferedReader(new FileReader(templateFile));
            while(reader.ready()) {
                String line = reader.readLine();
                if (line.trim().startsWith("#") || line.trim().isEmpty())
                    continue;
                String[] splits = line.split("\\s+");
                if (splits.length >= 2) {
                    File newFile = new File(templateFile.getParentFile(), splits[1]);
                    str = LmcpGen.readFile(newFile.toURI().toURL());
                    usedMethods.addAll(getMethodNames(str));
                } 
            }
            
            ArrayList<String> unusedMethods = new ArrayList<String>();
            for (Method m : methods) {
                if (!usedMethods.contains(m.getName())) {
                    System.out.println(m.getName());
                    unusedMethods.add(m.getName());
                }
            } 
            return unusedMethods;

        } catch (Exception ex) {
            Logger.getLogger(ListUnusedMethods.class.getName()).log(Level.SEVERE, null, ex);
            return null;
        }
        
    }

    static ArrayList<String> getMethodNames(String fileStr) {
        ArrayList<String> retList = new ArrayList<String>();
        try {
            String[] splits = LmcpGen.splitString(fileStr);
            while (!splits[1].isEmpty()) {
                retList.add(splits[1]);
                splits = LmcpGen.splitString(splits[2]);
            }
        } catch (Exception ex) {
            Logger.getLogger(ListUnusedMethods.class.getName()).log(Level.SEVERE, null, ex);
        }
        return retList;
    }

    public static void main(String[] args) {
        listUnusedMethods(JavaMethods.class, new File("./src/templates/java.tl"));
    }

}
