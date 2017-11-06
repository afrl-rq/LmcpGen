// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

/*
 * StructInfo.java
 *
 * Created on August 1, 2007, 5:10 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package avtas.lmcp.lmcpgen;

import java.util.Arrays;
import java.util.Objects;

/**
 *
 * @author default
 */
public class StructInfo {
    
    public String name = "";
    public int id = 0;
    public String extends_name = "";
    public String extends_series = "";
    public String comment = "";
    public FieldInfo[] fields = new FieldInfo[0];
    public String seriesName = "";
    public String namespace = "";
    
    /**
     * Creates a new instance of StructInfo
     */
    public StructInfo() {
    }

    public boolean hasParent() {
        return !this.extends_name.isEmpty();
    }

    @Override
    public String toString() {
        return "StructInfo{" + "name=" + name + ", id=" + id + ", extends_name=" + extends_name + ", extends_series=" + extends_series + ", comment=" + comment + ", fields=" + fields + ", seriesName=" + seriesName + ", namespace=" + namespace + '}';
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 47 * hash + Objects.hashCode(this.name);
        hash = 47 * hash + this.id;
        hash = 47 * hash + Objects.hashCode(this.extends_name);
        hash = 47 * hash + Objects.hashCode(this.extends_series);
        hash = 47 * hash + Objects.hashCode(this.comment);
        hash = 47 * hash + Arrays.deepHashCode(this.fields);
        hash = 47 * hash + Objects.hashCode(this.seriesName);
        hash = 47 * hash + Objects.hashCode(this.namespace);
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
        final StructInfo other = (StructInfo) obj;
        if (this.id != other.id) {
            return false;
        }
        if (!Objects.equals(this.name, other.name)) {
            return false;
        }
        if (!Objects.equals(this.extends_name, other.extends_name)) {
            return false;
        }
        if (!Objects.equals(this.extends_series, other.extends_series)) {
            return false;
        }
        if (!Objects.equals(this.comment, other.comment)) {
            return false;
        }
        if (!Objects.equals(this.seriesName, other.seriesName)) {
            return false;
        }
        if (!Objects.equals(this.namespace, other.namespace)) {
            return false;
        }
        if (!Arrays.deepEquals(this.fields, other.fields)) {
            return false;
        }
        return true;
    }
    
    
}
