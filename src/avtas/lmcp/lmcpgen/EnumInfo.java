// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package avtas.lmcp.lmcpgen;

import java.util.ArrayList;

/**
 * Stores information about an LMCP Enum object
 * @author matt
 */
public class EnumInfo {

    public String name = "";
    public String comment = "";
    public String namespace = "";
    public String seriesName = "";

    public ArrayList<EnumEntry> entries = new ArrayList<EnumEntry>();

    public static class EnumEntry {

        public String name = "";
        public String value = "0";
        public String comment = "";

    }

    public boolean containsEnum(String name) {
        for (EnumEntry e : entries) {
            if (e.name.equals(name)){
                return true;
            }
        }
        return false;
    }

}
