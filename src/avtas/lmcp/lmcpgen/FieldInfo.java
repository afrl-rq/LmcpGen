// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

/*
 * FieldInfo.java
 *
 * Created on August 1, 2007, 5:10 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package avtas.lmcp.lmcpgen;

/**
 *
 * @author default
 */
public class FieldInfo {

//    static final int SCALAR = 0;
//    static final int FIXED_LIST = 1;
//    static final int VAR_LIST = 2;
//    static final int MAP = 3;
//
//    static final int PRIMITIVE = 0;
//    static final int STUCT = 1;
//    static final int ENUM = 2;
    
    public String name = "";
    public String comment = "";
    public int length = 1;
    public String type = "";
    public String defaultVal = "";
    public String units = "";
    public String seriesName = "";
    public boolean isScalar = false;
    public boolean isArray = false;
    public boolean isLargeArray = false;
    public boolean isEnum = false;
    public boolean isStruct = false;
    public boolean isMap = false;
    public boolean isOptional = false;
    
    /** Creates a new instance of FieldInfo */
    public FieldInfo() {
    }

    @Override
    public String toString() {
        return "FieldInfo{" + "name=" + name + ", comment=" + comment + ", length=" + length + ", type=" + type + ", defaultVal=" + defaultVal + ", units=" + units + ", seriesName=" + seriesName + ", isScalar=" + isScalar + ", isArray=" + isArray + ", isLargeArray=" + isLargeArray + ", isEnum=" + isEnum + ", isStruct=" + isStruct + ", isMap=" + isMap + ", isOptional=" + isOptional + '}';
    }
    
}
