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

import java.util.Objects;

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

    public boolean isReallyScalar() {
        return !(isArray ||
                 isLargeArray ||
                 isStruct ||
                 isMap ||
                 isOptional ||
                 type.equals("string"));
    }

    @Override
    public String toString() {
        return "FieldInfo{" + "name=" + name + ", comment=" + comment + ", length=" + length + ", type=" + type + ", defaultVal=" + defaultVal + ", units=" + units + ", seriesName=" + seriesName + ", isScalar=" + isScalar + ", isArray=" + isArray + ", isLargeArray=" + isLargeArray + ", isEnum=" + isEnum + ", isStruct=" + isStruct + ", isMap=" + isMap + ", isOptional=" + isOptional + '}';
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 61 * hash + Objects.hashCode(this.name);
        hash = 61 * hash + Objects.hashCode(this.comment);
        hash = 61 * hash + this.length;
        hash = 61 * hash + Objects.hashCode(this.type);
        hash = 61 * hash + Objects.hashCode(this.defaultVal);
        hash = 61 * hash + Objects.hashCode(this.units);
        hash = 61 * hash + Objects.hashCode(this.seriesName);
        hash = 61 * hash + (this.isScalar ? 1 : 0);
        hash = 61 * hash + (this.isArray ? 1 : 0);
        hash = 61 * hash + (this.isLargeArray ? 1 : 0);
        hash = 61 * hash + (this.isEnum ? 1 : 0);
        hash = 61 * hash + (this.isStruct ? 1 : 0);
        hash = 61 * hash + (this.isMap ? 1 : 0);
        hash = 61 * hash + (this.isOptional ? 1 : 0);
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final FieldInfo other = (FieldInfo) obj;
        if (this.length != other.length) {
            return false;
        }
        if (this.isScalar != other.isScalar) {
            return false;
        }
        if (this.isArray != other.isArray) {
            return false;
        }
        if (this.isLargeArray != other.isLargeArray) {
            return false;
        }
        if (this.isEnum != other.isEnum) {
            return false;
        }
        if (this.isStruct != other.isStruct) {
            return false;
        }
        if (this.isMap != other.isMap) {
            return false;
        }
        if (this.isOptional != other.isOptional) {
            return false;
        }
        if (!Objects.equals(this.name, other.name)) {
            return false;
        }
        if (!Objects.equals(this.comment, other.comment)) {
            return false;
        }
        if (!Objects.equals(this.type, other.type)) {
            return false;
        }
        if (!Objects.equals(this.defaultVal, other.defaultVal)) {
            return false;
        }
        if (!Objects.equals(this.units, other.units)) {
            return false;
        }
        if (!Objects.equals(this.seriesName, other.seriesName)) {
            return false;
        }
        return true;
    }

}
