# coding=utf-8

import os
import os.path
import subprocess

# This executable converts the .HIS file to a .TXT file. 
path_reahis = "d:\\temp\\bin\\reahis.exe"

def convert_his_to_txt (path_hisfile) : 
    
    basedir, hisfile = os.path.split(path_hisfile)
    basefilename = hisfile.split('.')[0]
    path_txtfile = os.path.join (basedir, basefilename + ".txt") 
    subprocess.call (["cmd.exe","/C",path_reahis, path_hisfile,">", path_txtfile])
    
    return path_txtfile



def read_his_txt_file (path_txtfile) : 
    # Read a file that has been converted from HIS to TXT. 
    f = open (path_txtfile)
    
    columns = []  # List of (parameter, location) tuples, based on the header information in the .txt file. 
    data = []     # The actual data. The meaning of column i is given in columns[i]. 
    read_data = False
    current_parameter = None
    
    for line in f :
        if line.strip() == "" : 
            continue 
        if not read_data:
            # Read column data
            stripped_line = line.strip()
            if stripped_line.startswith("Parameter") : 
                split_line = stripped_line.split(":")    #  Infiltration [m3/s]
                current_parameter = split_line[1].strip() 
            elif stripped_line.startswith("Location") : 
                split_line = stripped_line.split(":")
                location = split_line[1].strip()
                columns.append((current_parameter,location))
            elif stripped_line.startswith ("Date") and stripped_line.find("Time") != -1 : 
                # End of column definitions. 
                read_data = True
                # Create empty arrays in data. 
                data = [ [] for x in columns ]
                
        elif read_data:
            split_line = [ x for x in line.split(" ")if x != '' ][2:]
            if len(split_line) != len(columns) : 
                print "WARNING: number of values (%i) does not equal number of expected columns (%i)" % (len(split_line), len(columns))
                print "WARNING: " + line
            for column_index, value in enumerate(split_line) : 
                data[column_index].append(float(value))
    
    return columns, data



def get_data_from_his (path_hisfile) :
    # Convert to text file (e.g. upflowdt.his -> upflowdt.txt)
    path_txtfile = convert_his_to_txt (path_hisfile)

    # Parse text file
    columns, data = read_his_txt_file (path_txtfile)

    # And return the parsed data from the text file. 
    return columns, data

def get_data_from_his_files (root_dir, hisfiles) : 
    # For each hisfile in root_dir, get the data. 
    d = {}
    for hisfile in hisfiles : 
        abspath = os.path.join(root_dir, hisfile)
        d[hisfile] = get_data_from_his(abspath)
    return d