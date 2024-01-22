# -*- coding: utf-8 -*-

# Before running this script, one needs to manually add Discharge (l) to the Output parameters in the SOBEK GUI, 
# because the importer does not know when to add the output parameter itself. 

import os
import os.path
import netCDF4
import numpy

import read_his
import read_cnf
import read_netcdf
import read_network_gr

def find_testcases (root) :
    l = []
    for f1 in [ x for x in os.listdir(root) if os.path.isdir(os.path.join(root, x)) ] : 
        f1_abs = os.path.join(root, f1)
        for f2 in [ x for x in os.listdir(f1_abs) if os.path.isdir(os.path.join(f1_abs, x)) ] :
            f2_abs = os.path.join(f1_abs, f2)
            for f3 in [ x for x in os.listdir(f2_abs) if os.path.isdir(os.path.join(f2_abs, x)) ] :
                f3_abs = os.path.join(f2_abs, f3)
                if "network.tp" in [ x.lower() for x in os.listdir(f3_abs) ] :
                    cnf = os.path.join(f1_abs, "sobek.cnf")
                    l.append((f1, f3_abs, cnf))
    
    return filter_testcases(l)

def filter_testcases (l) : 
    allowed = ("030",  "033",  "034",  "036",  "039b", "052",  "056",  "107",  "112b", "114", "116",
               "133b", "134",  "147b", "148",  "149",  "150",  "153",  "155",  "156",  "159b",
               "160b", "161b", "163",  "164b", "165b", "166",  "167",  "168",  "169",  "170b",
               "171b", "172",  "173",  "175",  "176",  "177",  "178",  "179",  "180b", "181", "182",
               "183",  "184",  "185",  "186",  "187",  "188",  "189",  "197",  "198",  "199",
               "200",  "201",  "202",  "203",  "204",  "206b", "211a", "220",  "221",  "222",
               "223",  "224",  "225b", "226b", "227",  "228",  "231",  "235",  "243",  "245",
               "246b", "248",  "249",  "250",  "251",  "252b", "253b", "254b", "256",  "257",
               "258",  "259c", "260b", "261b", "263",  "264",  "267",  "269",  "270",  "271",
               "272b", "273b", "276b", "277",  "280",  "284b", "285b", "286b", "293",  "294b",
               "295b", "296",  "297",  "298",  "301",  "302",  "303",  "304",  "305",  "306",
               "307",  "309",  "310",  "355",  "356",  "358",  "359",  "360",  "361",  "362",
               "363",  "364",  "365",  "366",  "367",  "368",  "369",  "370",  "371",  "372",
               "373",  "374",  "375",  "376",  "377",  "378",  "379",  "380",  "400",  "401",
               "402",  "403",  "404",  "405",  "406",  "407",  "408",  "409",  "410",  "411",
               "412",  "413",  "414",  "415",  "444",  "445")
        
    l_new = [ c for c in l if c[0][5:] in allowed ] # [5:] in order to remove "Test_"
    return l_new

# These dictionaries need to be expanded for all types of variables!! 
dict_translate_sobekcnf_his = {
  ("upflowdt.his", "Groundw.outfl.")        : "Groundw.outfl.[m3/s]",
  ("upflowdt.his", "Rainfall")              : "Rainfall     [m3/s]",
  ("upflowdt.his", "Infiltration")          : "Infiltration [m3/s]",
  ("upflowdt.his", "Percolation")           : "Percolation  [m3/s]",
  ("upflowdt.his", "Actual Evap.")          : "Actual Evap. [m3/s]",
  ("upflowdt.his", "Pot. Evapor.")          : "Pot. Evapor. [m3/s]",
  ("upflowdt.his", "Groundw.Volume")        : "Groundw.Volume [m3]",
  ("upflowdt.his", "Groundw.Level")         : "Groundw.Level   [m]",
  ("upflowdt.his", "GWLevel-Surface")       : "GWLevel-Surface[m]",
  ("upflowdt.his", "Capill.Rise")           : "Capill.Rise  [m3/s]",
  ("upflowdt.his", "Net Seepage")           : "Net Seepage  [m3/s]",
  ("upflowdt.his", "Storage Land   [mm]")   : "Storage Land   [mm]",
  ("upflowdt.his", "Storage Land [mm]")     : "Storage Land   [mm]",
  ("upflowdt.his", "Storage Land   [m3]")   : "Storage Land   [m3]",
  ("upflowdt.his", "Storage Land [m3]")     : "Storage Land   [m3]",
  ("upflowdt.his", "Surf. Runoff")          : "Surf. Runoff  [m3/s]",
  ("calcpnt.his",  "Waterlevel")            : "Waterlevel  (m AD)",
  ("calcpnt.his",  "Waterdepth")            : "Waterdepth  (m)",
  ("calcpnt.his",  "Volume")                : "Volume (m³)",
  ("calcpnt.his",  "Total area")            : "Total area (m2)",
  ("calcpnt.his",  "Total width")           : "Total width (m)",
  ("calcpnt.his",  "Free Board")            : "Free board (m)",
  ("qlat.his",     "Lateral disch.")        : "Lateral disch.(m3/s)",
  ("qlat.his",     "Defined Lateral")       : "Defined Lateral Disc",
  ("qwb.his",      "Volume")                : "Volume (1000 m³)",
  ("reachseg.his", "Velocity")              : "Velocity (m/s)",
  ("reachseg.his", "velocity")              : "Velocity (m/s)",
  ("reachseg.his", "Discharge")             : "Discharge (m³/s)",
  ("reachseg.his", "Conveyance")            : "Conveyance (m3/s)",
  ("reachseg.his", "Froude number")         : "Froude number ( )",
  ("rsegsub.his",  "Discharge floodp 1")    : "Discharge floodp 1 (",     # m3/s)",   reahis.exe converts param name from HIS to txt with ONLY 20 CHARS
  ("rsegsub.his",  "Discharge floodp 2")    : "Discharge floodp 2 (",     # m3/s)",   reahis.exe converts param name from HIS to txt with ONLY 20 CHARS
  ("rsegsub.his",  "Area floodp 1")         : "Area floodp 1 (m2)",
  ("rsegsub.his",  "Area floodp 2")         : "Area floodp 2 (m2)",
  ("rsegsub.his",  "Width floodp 1")        : "Width floodp 1 (m)",
  ("rsegsub.his",  "Width floodp 2")        : "Width floodp 2 (m)",
  ("rsegsub.his",  "Chezy floodp 1")        : "Chezy floodp 1 (m^1/",    #2·s^-1)",   reahis.exe converts param name from HIS to txt with ONLY 20 CHARS
  ("rsegsub.his",  "Chezy floodp 2")        : "Chezy floodp 2 (m^1/",    #2·s^-1)",   reahis.exe converts param name from HIS to txt with ONLY 20 CHARS
  ("rsegsub.his",  "Hydr radius floodp 1")  : "Hydr radius floodp 1",    # (m)",      reahis.exe converts param name from HIS to txt with ONLY 20 CHARS
  ("rsegsub.his",  "Hydr radius floodp 2")  : "Hydr radius floodp 2",    # (m)",      reahis.exe converts param name from HIS to txt with ONLY 20 CHARS
  ("rsegsub.his",  "Area main")             : "Area main (m2)",
  ("rsegsub.his",  "Width main")            : "Width main (m)",
  ("rsegsub.his",  "Hydr radius main")      : "Hydr radius main (m)",
  ("rsegsub.his",  "Discharge main")        : "Discharge main (m3/s",    #)",         reahis.exe converts param name from HIS to txt with ONLY 20 CHARS
  ("rsegsub.his",  "Chezy main")            : "Chezy main (m^1/2·s^",    #-1)"        reahis.exe converts param name from HIS to txt with ONLY 20 CHARS
  ("struc.his",    "Crest level (m AD)")    : "Crest level (m AD)",
  ("struc.his",    "Crest level")           : "Crest level (m AD)",  # ?? Double?
  ("struc.his",    "Crest Width (m)")       : "Crest Width (m)",
  ("struc.his",    "Crest Width")           : "Crest Width (m)",  # ?? Double?
  ("struc.his",    "Discharge")             : "Discharge (m³/s)",
  ("struc.his",    "Velocity")              : "Velocity (m/s)",
  ("struc.his",    "Velocity (m/s)")        : "Velocity (m/s)",
  ("struc.his",    "Velocity (m/ s)")       : "Velocity (m/s)",
  ("struc.his",    "Waterlevel down")       : "Waterlevel down (m A",    #D)
  ("struc.his",    "Waterlevel up")         : "Waterlevel up (m AD)",
  ("struc.his",    "Waterlevel down (m AD)"): "Waterlevel down (m A",    #D)
  ("struc.his",    "Waterlevel down (m A")  : "Waterlevel down (m A",    #D)
  ("struc.his",    "Waterlevel up (m AD)")  : "Waterlevel up (m AD)",
  ("struc.his",    "Gate lower edge")       : "Gate lower edge leve",
  ("struc.his",    "Gate lower edge level (m AD)") : "Gate lower edge leve",
  ("struc.his",    "Head")                  : "Head (m)",
  ("struc.his",    "Head (m)")              : "Head (m)",
  ("struc.his",    "Flow Area (m2)")        : "Flow Area (m2)"
}

dict_translate_sobekcnf_netcdf = { 
  ("upflowdt.his", "Groundw.outfl.")        : "Groundwater outflow (unp)",
  ("upflowdt.his", "Rainfall")              : "Rainfall (unp)", 
  ("upflowdt.his", "Infiltration")          : "Infiltration (unp)",
  ("upflowdt.his", "Percolation")           : "Percolation (unp)",
  ("upflowdt.his", "Actual Evap.")          : "Actual evaporation (unp)",
  ("upflowdt.his", "Pot. Evapor.")          : "Potential evaporation (unp)",
  ("upflowdt.his", "Groundw.Volume")        : "Groundwater volume (unp)",
  ("upflowdt.his", "Groundw.Level")         : "Groundwater level (unp)", 
  ("upflowdt.his", "GWLevel-Surface")       : "Groundwater level surface (unp)",
  ("upflowdt.his", "Capill.Rise")           : "Capillary Rise (unp)",
  ("upflowdt.his", "Net Seepage")           : "Net seepage (unp)",
  ("upflowdt.his", "Storage Land   [mm]")   : "Storage land (unp)", 
  ("upflowdt.his", "Storage Land   [m3]")   : "Storage land m3 (unp)",
  ("upflowdt.his", "Surf. Runoff")          : "Surface runoff (unp)",
  ("calcpnt.his",  "Waterlevel")            : "Water level",
  ("calcpnt.his",  "Waterdepth")            : "Water depth",
  ("calcpnt.his",  "Volume")                : "Water volume",
  ("calcpnt.his",  "Total area")            : "Total area",
  ("calcpnt.his",  "Total width")           : "Total width",
  #("calcpnt.his",  "Lateral flow at Node")  : "",
  ("qlat.his",     "Lateral disch.")        : "Discharge (l)",
  #("qlat.his",    "Defined Lateral")        : "",
  ("reachseg.his", "Velocity")              : "Velocity",
  ("reachseg.his", "velocity")              : "Velocity",
  ("reachseg.his", "Discharge")             : "Discharge",
  ("reachseg.his", "Conveyance")            : "Conveyance",
  ("reachseg.his", "Froude number")         : "Froude number",
  ("rsegsub.his",  "Discharge floodp 1")    : "FloodPlain1 Discharge",
  ("rsegsub.his",  "Discharge floodp 2")    : "FloodPlain2 Discharge",
  ("rsegsub.his",  "Area floodp 1")         : "FloodPlain1 Flow area",
  ("rsegsub.his",  "Area floodp 2")         : "FloodPlain2 Flow area",
  ("rsegsub.his",  "Width floodp 1")        : "FloodPlain1 Flow width",
  ("rsegsub.his",  "Width floodp 2")        : "FloodPlain2 Flow width",
  ("rsegsub.his",  "Chezy floodp 1")        : "FloodPlain1 Chezy values",
  ("rsegsub.his",  "Chezy floodp 2")        : "FloodPlain2 Chezy values",
  ("rsegsub.his",  "Hydr radius floodp 1")  : "FloodPlain1 Hydraulic radius",
  ("rsegsub.his",  "Hydr radius floodp 2")  : "FloodPlain2 Hydraulic radius",
  ("rsegsub.his",  "Area main")             : "Main Flow area",
  ("rsegsub.his",  "Width main")            : "Main Flow width",
  ("rsegsub.his",  "Hydr radius main")      : "Main Hydraulic radius",
  ("rsegsub.his",  "Discharge main")        : "Main Discharge",
  ("rsegsub.his",  "Chezy main")            : "Main Chezy values",
  ("struc.his",    "Crest level (m AD)")    : "Crest level (s)",
  ("struc.his",    "Crest level")           : "Crest level (s)",  # ?? Double?
  ("struc.his",    "Crest Width (m)")       : "Crest width (s)",
  ("struc.his",    "Crest Width")           : "Crest width (s)",  # ?? Double?
  ("struc.his",    "Discharge")             : "Discharge (s)",
  ("struc.his",    "Velocity")              : "Velocity (s)",
  ("struc.his",    "Velocity (m/s)")        : "Velocity (s)",
  ("struc.his",    "Velocity (m/ s)")       : "Velocity (s)",
  ("struc.his",    "Waterlevel down")       : "Water level down (s)",
  ("struc.his",    "Waterlevel up")         : "Water level up (s)",
  ("struc.his",    "Waterlevel down (m AD)"): "Water level down (s)",
  ("struc.his",    "Waterlevel down (m A")  : "Water level down (s)",
  ("struc.his",    "Waterlevel up (m AD)")  : "Water level up (s)",
  ("struc.his",    "Gate lower edge")       : "Gate lower edge level (s)",
  ("struc.his",    "Gate lower edge level (m AD)") : "Gate lower edge level (s)",
  ("struc.his",    "Head")                  : "Head Difference (s)",
  ("struc.his",    "Head (m)")              : "Head Difference (s)",
  ("struc.his",    "Opening height???")     : "Opening height (s)",
  ("struc.his",    "Flow Area (m2)")        : "Flow area (s)"
}

def find_sobek2_values (filename, his_data, parameter, location) : 
    # Find SOBEK 2 data. 
    # Returns a tuple: 
    # - A two-dimensional NumPy array with float64, with time on the vertical axis and location on the horizontal axis.  
    # - A list of names of locations. This can be more than 1 in case the input parameter is '*'. 
    # Returns None if they cannot be retrieved. 

    # First, convert parameter to HIS-file style. 
    if (filename.lower(), parameter) not in dict_translate_sobekcnf_his :
        print "  ERROR: filename/parameter combination is not supported in SOBEK2 part: %s %s" % (filename, parameter)
        return None
    parameter_his = dict_translate_sobekcnf_his[(filename.lower(), parameter)]

    sobek2_values = None
    return_locations = []

    if filename not in his_data : 
        print "  ERROR: Filename %s not found." % filename
        return None
    his_columns, his_data_local = his_data[filename]

    results = []
    if filename.lower() == "qwb.his" :
        # For qwb.his, always query the two same pseudo-locations: 
        pseudo_locations = [ "Volume water system", "Error (B+L-V+V0)" ]
        for pseudo_location in pseudo_locations :    
            try :
                i_column = his_columns.index((parameter_his,pseudo_location))
                results.append(his_data_local[i_column])
            except ValueError :
                print "  ERROR: Cannot find location %s in qwb.his" % (pseudo_location)
                return None
        return_locations = pseudo_locations
    elif location == "*" : 
        # Several locations need to be queried
        for i in range(len(his_columns)) :
            i_parameter, i_location = his_columns[i]
            if i_parameter == parameter_his : 
                return_locations.append(i_location)
                results.append(his_data_local[i])
        if len(return_locations) == 0 :
            print "  ERROR: No locations found for parameter %s in SOBEK2 data." % parameter
            return None
    else : 
        # One specific location is queried. 
        try :
            i_column = his_columns.index((parameter_his,location))
        except ValueError :
            print "  ERROR: Cannot find parameter/location combination %s in SOBEK2 data." % str((parameter,location))
            return None
        return_locations.append(location)
        results.append(his_data_local[i_column])
    
    sobek2_values = numpy.transpose(numpy.array(results))
    return (sobek2_values, return_locations)

def guess_netcdf_column (netcdf_dataset, location_name, sobek2_info) : 
    # sobek2_info consists of (sobek2_name, branch_name, chainage, x, y) tuples. 
    # Two ways of figuring out which column needs to be taken. 

    # 1. Based on the name of the location. For RR models and STRUC.HIS, this is listed in the feature_name table. 
    if "feature_name" in netcdf_dataset.variables : 
        netcdf_var_feature_names = netcdf_dataset.variables['feature_name']
        lst_feature_names = [ "".join(x).strip() for x in netcdf_var_feature_names ]
        if location_name in lst_feature_names :
            return lst_feature_names.index(location_name)
        elif location_name + "##1##1" in lst_feature_names :  # But: composite structure cannot be output in Delta Shell, so this is dead code. 
            print "  WARNING: location %s interpreted as location %s" % (location, location + "##1##1")
            return lst_feature_names.index(location_name + "##1##1")
        else :
            print "  ERROR: Cannot find feature name %s" % location_name
            return None

    # 2. Based on branch_name and branch_chainage
    if "branch_name" in netcdf_dataset.variables and "branch_chainage" in netcdf_dataset.variables : 
        matching_info_lst = [ x for x in sobek2_info if x[0] == location_name ]
        if len(matching_info_lst) == 0 :
            return None
        branch_name = matching_info_lst[0][1]
        chainage = matching_info_lst[0][2]
        #x = matching_info_lst[0][3]   # The information is available, but not used yet. 
        #y = matching_info_lst[0][4]   # The information is available, but not used yet. 

        all_branch_names = [ "".join(name_array).strip() for name_array in netcdf_dataset.variables['branch_name'][:] ]
        all_chainages = netcdf_dataset.variables['branch_chainage'][:]
        names_chainages = zip(all_branch_names, all_chainages)
        for i, (netcdf_branch_name, netcdf_chainage) in enumerate(names_chainages) : 
            if netcdf_branch_name == branch_name and abs(netcdf_chainage-chainage) < 1.0 : 
                return i
        
    print "  ERROR: Cannot find location %s in netCDF file." % location_name
    return None

def find_sobek3_values_qwb_his (netcdf_data) :
    # qwb.his abuses the location parameter for different types of parameters. 
    # This method will always check on total volume and total volume error only. 
    lst_results = []
    
    netcdf_dataset = netcdf_data["Water system total volume"]
    netcdf_var_values = netcdf_dataset.variables["value"]   
    lst_results.append (netcdf_var_values[:,0])

    netcdf_dataset = netcdf_data["Water system volume error"]
    netcdf_var_values = netcdf_dataset.variables["value"]   
    lst_results.append (netcdf_var_values[:,0])
    
    sobek3_values = numpy.transpose(numpy.array(lst_results))
    return sobek3_values

    
def find_sobek3_values (netcdf_data, filename, parameter, location_names, gridpoints_info, segments_info) : 
    # Find SOBEK 3 data for a certain parameter, on a number of locations. 
    # gridpoints_info and segments_info is information that can be used to derive the sobek2 location names. 
    # Returns a two-dimensional NumPy array with float64. 
    # Returns None if the results cannot be retrieved. 
    sobek3_values = None
    
    # Special case: qwb.his. 
    if filename.lower() == "qwb.his" :
        return find_sobek3_values_qwb_his (netcdf_data)

    if (filename.lower(), parameter) not in dict_translate_sobekcnf_netcdf : 
        print "  ERROR: filename/parameter combination is not supported in SOBEK3 part: %s %s" % (filename, parameter)
        return None
    
    netcdf_varname = dict_translate_sobekcnf_netcdf[(filename.lower(), parameter)]
    if netcdf_varname not in netcdf_data : 
        print "  ERROR: Netcdf parameter %s not found." % netcdf_varname
        return None
    netcdf_dataset = netcdf_data[netcdf_varname]
    
    lst_results = []
    for location_name in location_names : 
        
        # Find correct column in SOBEK 3 netCDF file.
        netcdf_idx = None
        if filename.lower() in [ "calcpnt.his", "qlat.his" ] :   # This list needs to be expanded in case more .HIS files refer to grid points. 
            # Comparison at grid point level. 
            netcdf_idx = guess_netcdf_column(netcdf_dataset, location_name, gridpoints_info)
        else : 
            # Comparison at segment level. 
            netcdf_idx = guess_netcdf_column(netcdf_dataset, location_name, segments_info)

        if netcdf_idx == None :
            print "  ERROR: Can not find location %s in netCDF file." % location_name
            return None
            
        # Retrieve all values from the correct column
        netcdf_var_values = None
        if netcdf_varname in netcdf_dataset.variables : 
            netcdf_var_values = netcdf_dataset.variables[netcdf_varname]
        elif "value" in netcdf_dataset.variables : 
            netcdf_var_values = netcdf_dataset.variables["value"]
        else : 
            print "  ERROR: Cannot find variable %s (or 'value') in netCDF file." % netcdf_varname
            return None
        lst_results.append (netcdf_var_values[:,netcdf_idx])

    sobek3_values = numpy.transpose(numpy.array(lst_results))
    return sobek3_values

def compare_values (values1, values2, abs_tolerance, rel_tolerance) : 
    # Compares two numpy arrays for near-equality. 
    # Prints ERROR message when the arrays are not equal enough. 
    # Returns None when an error occurred, False when the differences were too large, and True when the difference was small enough. 
    # TODO: Relative tolerance. 

    if len(values1.shape) != 2 or len(values2.shape) != 2 :
        print "  ERROR: Incorrect shapes of values to be compared."
        return None
    if values1.shape != values2.shape : 
        print "  ERROR: Unequal number of values to be compared: %s %s" % (str(values1.shape), str(values2.shape))
        return None

    diff = numpy.abs(values1-values2)
    max_diff = numpy.max(diff)

    if max_diff > abs_tolerance : 
        print "  ERROR: compared %i floats. Maximum absolute difference (%f) has been exceeded: %f" % (values1.shape[0] * values1.shape[1], abs_tolerance, max_diff)
        return False
    else : 
        print "SUCCESS: compared %i floats. Maximum absolute difference: %f" % (values1.shape[0] * values1.shape[1], max_diff)
        return True

def log_differences (path, values1, values2) : 
    f_log = open (path, 'w')
    if values1.ndim == 1 :
        f_log.write ("\tSOBEK2\tSOBEK3\tDifference")
        for i in range(len(values1)) : 
            f_log.write ("%8i\t%.7e\t%.7e\t%.7e\n" % (i, values1[i], values2[i], values2[i]-values1[i]))
    else :
        f_log.write ("SOBEK2\n")
        f_log.write (str(values1) + '\n')
        f_log.write ("SOBEK3\n")
        f_log.write (str(values2) + '\n')
        f_log.write ("Difference\n")
        f_log.write (str(values2-values1) + '\n')
    f_log.close()

def main() :
    
    # This is where Thieu's testbench is located (SOBEK2 testcases)
    root_src = "d:\\temp\\testcases"
    testcases_lst = find_testcases (root_src)
    
    # This is where all converted test cases (SOBEK3 dsprojs) are located. 
    root_target = "d:\\temp\\output"
    
    # These variables will eventually not be necessary any more, once all parameters have been implemented in the dicionaries.
    missing_params2 = set()
    missing_params3 = set()
        
    # For each test case... 
    for testcase_name, testcase_absdir, testcase_cnf in testcases_lst : 
        print "Test case: " + testcase_name
    
        # Read all .HIS files
        cnf = read_cnf.Sobek2ConfigFile(testcase_cnf)
        hisfiles = set([ x[0] for x in cnf.configLines])
        his_data = read_his.get_data_from_his_files (testcase_absdir, hisfiles)
        
        # Read network.gr file (for future mapping of sobek3 grid points/segments to their sobek2 counterparts. 
        (gridpoints_info, segments_info) = read_network_gr.get_locations(os.path.join(testcase_absdir, "NETWORK.GR"))

        # Read all netCDF files
        target_dir = os.path.join (root_target, testcase_name)
        target_data_dir = os.path.join (target_dir, testcase_name + ".dsproj_data")
        netcdf_data = read_netcdf.read_netcdf_files (target_data_dir)

        # TODO: Write all netcdf files to txt files, with the same ordering of location columns. 
    
        # For each line in the sobek.cnf file...
        for filename, parameter, location, abs_tolerance, rel_tolerance in cnf.configLines : 
            print "Verifying: %s %s at location '%s'" % (filename, parameter, location)
        
            tuple_sobek2 = find_sobek2_values(filename, his_data, parameter, location)
            if tuple_sobek2 == None : 
                missing_params2.add(parameter.lower() + " (" + filename.lower() + ")")
                continue
            sobek2_values, location_names = tuple_sobek2
        
            # In case the location was '*', find_sobek3_values() needs to know which locations to pick up (instead of 'all').
            # Also, the gridpoints/segments including spatial information (chainage, x, y) is needed sometimes 
            # to figure out which data belongs to which sobek2 ID. 
            sobek3_values = find_sobek3_values(netcdf_data, filename, parameter, location_names, gridpoints_info, segments_info)
            if sobek3_values == None : 
                missing_params3.add(parameter.lower() + " (" + filename.lower() + ")")
                continue

            result = compare_values (sobek2_values, sobek3_values, abs_tolerance, rel_tolerance)
            if result == False :   # Remove this condition? Maybe Thieu is then happy enough. 
                log_parameter_filename = parameter.replace(" ", "_").replace(".", "_")
                log_filename = "%s-%s-%s-%s.txt" % (testcase_name, filename, log_parameter_filename, location)
                log_filename = log_filename.replace("*", "all")
                log_path = os.path.join(root_target, log_filename)
                log_differences(log_path, sobek2_values, sobek3_values)

        # Close all netcdf files. 
        for netcdf_dataset in netcdf_data.values() : 
            netcdf_dataset.close()

        print "\n"
            
    # Indicate missing parameters in dictionaries (temporary)
    if len(missing_params2) > 0 :
        print "Missing in dict_translate_sobekcnf_his:"
        for item in missing_params2:
            print item

    print '\n'
    if len(missing_params3) > 0 :
        print "Missing in dict_translate_sobekcnf_netcdf:"
        for item in missing_params3:
            print item


main()