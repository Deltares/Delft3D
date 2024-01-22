
"""

         <testCase name="e07_f10_c01_sediment_transport_Engelund_Hansen" ref="sobek3_default">
             <path>e07_sobek/f10_morphology/c01_sediment_transport_Engelund_Hansen</path>
             <programs><program ref="sobek3">
                 <arguments>
                    <argument>-p=test001b.dsproj</argument>
                    <argument>-r="Integrated Model"</argument>
                 </arguments>
             </program></programs>
             <checks>
                 <file name="test001b.dsproj_data\Water level-0aa37e2d-0478-4473-ba33-1737e0fc2205.nc" type="netCDF"><parameters><parameter name="Water level" tolerance="0.0001"/></parameters></file>
                 <file name="test001b.dsproj_data\Water depth-5be64de5-106c-4175-a63a-cc97c6a32e3a.nc" type="netCDF"><parameters><parameter name="Water depth" tolerance="0.0001"/></parameters></file>
                 <file name="test001b.dsproj_data\water_flow_1d_output\work\morph-gr.his" type="HIS"><parameters><parameter name="bla" tolerance="0.0001"/></parameters></file>
             </checks>
         </testCase>
"""

import os
import os.path

def find_all_test_cases (path) :
    return [ (p, os.path.join(path, p)) for p in os.listdir(path) if os.path.isdir(os.path.join(path, p)) and p.startswith("Test") ]

def find_all_nc_files (path) :
    data_dir_rel = [ p for p in os.listdir(path) if os.path.isdir(os.path.join(path, p)) and ".dsproj_data" in p ][0]
    data_dir_abs = os.path.join(path, p)
    nc_files = [ p for p in os.listdir(data_dir_abs) if p.endswith(".nc") ]
    return nc_files

def get_varname(nc_filename) :
    varname = nc_filename[:-40]
    if varname.endswith('(s)') or varname.startswith("Water system") or varname == "Discharge (l)" :
        # structure output and global variables: uses 'value' as variable, instead of variable name. 
        return 'value'
    else :
        return varname
    
def create_testbench_config(path) :
    l = []
    test_cases_paths = find_all_test_cases(path)
    for testcasename, test_case_path in test_cases_paths :
        testcasenumber = filter(str.isdigit, testcasename)
        l.append('         <testCase name="e07_f02_c%s" ref="sobek3_default">' % testcasenumber)
        l.append('             <path>e07_sobek/f02_sobek2_import/%s</path>' % testcasename)
        l.append('             <programs><program ref="sobek3">')
        l.append('                 <arguments>')
        l.append('                    <argument>-p=%s.dsproj</argument>' % testcasename)
        l.append('                    <argument>-r="integrated model"</argument>')
        l.append('                 </arguments>')
        l.append('             </program></programs>')
        l.append('             <checks>')
        for ncfilename in find_all_nc_files(test_case_path) :
            if not "(rtc)" in ncfilename :
                # These are copies anyway.
                l.append('                 <file name="%s.dsproj_data\%s" type="netCDF"><parameters><parameter name="%s" tolerance="0.01"/></parameters></file>' % (testcasename, ncfilename, get_varname(ncfilename)))
        l.append('             </checks>')
        l.append('         </testCase>')

    return "\n".join(l)
    
root_path = "D:\\cramer_bt\\svn\\testbench\\e07_sobek\\f02_sobek2_import"
print create_testbench_config(root_path)