// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

/*
 * MDMInfo.java
 *
 * Created on August 1, 2007, 1:54 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package avtas.lmcp.lmcpgen;

/**
 *
 * @author default
 */
public class MDMInfo {

    public static final String primitive_matcher = "(int16)|(int32)|(int64)|(byte)|(bool)|(char)|(real32)|(real64)|(uint32)|(uint16)|(string)";
    public static final String number_matcher = "(int16)|(int32)|(int64)|(byte)|(real32)|(real64)|(uint32)|(uint16)";
    
    public static String LMCP_OBJECT_NAME = "LmcpObject";
    
    
    public String seriesName = "";
    public String namespace = "";
    public String comment = "";
    public StructInfo[] structs = new StructInfo[0];
    public EnumInfo[] enums = new EnumInfo[0];
    /** a string containing the contents of the MDM file */
    public String mdmString = "";
    public int version = 0;
    public long seriesNameAsLong = 0;

    /** Creates a new instance of MDMInfo */
    public MDMInfo() {
    }

    private StructInfo getStructByName(String name) {
        for (StructInfo st : structs) {
            if (st.name.equals(name)) {
                return st;
            }
        }
        return null;
    }

    private EnumInfo getEnumByName(String name) {
        for (EnumInfo e : enums) {
            if (e.name.equals(name)) {
                return e;
            }
        }
        return null;
    }

    /** returns true if the given field is a struct (not primitive, not an en) */
    public boolean isStruct(String type) {
        return !type.matches(primitive_matcher) && (getEnumByName(type) == null);
    }

    /** returns true if the given field is an en (not primitive, not a struct) */
    public boolean isEnum(String type) {
        return !type.matches(primitive_matcher) && (getStructByName(type) == null);
    }

    /** Returns true if the type is a numerical type (real, unsigned/signed int, or byte) */
    public static boolean isNumber(String type) {
        return type.matches(number_matcher);
    }
    
    public static boolean isPrimitive(String type) {
        return !type.matches(primitive_matcher);
    }

    /** returns the struct that the given struct extends.  Returns null if there is no parent type declared
     * and throws an exception if the parent type does not exist.
     * @param st
     * @return
     */
    public static StructInfo getParentType(MDMInfo[] infos, StructInfo st) throws Exception {
        if (st.extends_name.isEmpty()) {
            return null;
        }
        for (MDMInfo info : infos) {
            if (info.seriesName.equals(st.extends_series)) {
                for (StructInfo tmp : info.structs) {
                    if (tmp.name.equals(st.extends_name)) {
                        return tmp;
                    }
                }
            }
        }
        throw new Exception("Parent type \"" + st.extends_name + "\" from \"" + st.name + "\" does not exist");
    }

    public static StructInfo getStructByName(MDMInfo[] infos, FieldInfo f) {
        for (MDMInfo info : infos) {
            if (info.seriesName.equals(f.seriesName)) {
                StructInfo si = info.getStructByName(f.type);
                if (si != null) {
                    return si;
                }
            }
        }
        return null;
    }

    public static EnumInfo getEnumByName(MDMInfo[] infos, FieldInfo f) {
        for (MDMInfo info : infos) {
            if (info.seriesName.equals(f.seriesName)) {
                EnumInfo si = info.getEnumByName(f.type);
                if (si != null) {
                    return si;
                }
            }
        }
        return null;
    }
}




