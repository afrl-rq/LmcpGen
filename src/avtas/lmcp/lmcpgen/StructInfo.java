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
    
}
