# This script needs to run from within Delta Shell scripting environment. 

import os
import os.path
from DeltaShell.Plugins.ImportExport.Sobek import SobekHydroModelImporter

def find_testcases (root) :
    l = []
    # for f1 in folders/directories with testcases (e.g. Test_254b)
    for f1 in [ x for x in os.listdir(root) if os.path.isdir(os.path.join(root, x)) ] : 
        f1_abs = os.path.join(root, f1)
        # for f2 in FIXED / *.lit folders (e.g. 254b_000.lit)
        for f2 in [ x for x in os.listdir(f1_abs) if os.path.isdir(os.path.join(f1_abs, x)) ] :
            f2_abs = os.path.join(f1_abs, f2)
            # for f3 in folders/directories with indices for SOBEK cases (e.g. 2)
            # in SOBEK2 testbank, only one SOBEK case per testcase
            for f3 in [ x for x in os.listdir(f2_abs) if os.path.isdir(os.path.join(f2_abs, x)) ] :
                f3_abs = os.path.join(f2_abs, f3)
                if "network.tp" in [ x.lower() for x in os.listdir(f3_abs) ] :
                    cnf = os.path.join(f1_abs, "sobek.cnf")
                    l.append((f1, f3_abs, cnf))
    
    return l

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


def import_run_save (source_dir, target_root_dir, testcase_name) : 
    settingsDatPath = os.path.join(source_dir, "settings.dat")
    networkTpPath = os.path.join(source_dir, "network.tp")
    
    useWaq = None
    useRR = None
    useFlowRTC = None
    with open(settingsDatPath) as settingsFile :
        settingsData = settingsFile.read().lower()
        settingsDataROI = settingsData[0:settingsData.index("[restart]")]
        useWaq = "delwaq=-1" in settingsDataROI
        useRR = "3b=-1" in settingsDataROI
        useFlowRTC = "channel=-1" in settingsDataROI or "river=-1" in settingsDataROI
    
    # Import the SOBEK2 case into SOBEK3. Output settings are automatically recognised by the C# code. 
    print "Importing " + source_dir + "..."
    importer = SobekHydroModelImporter(useWaq, useRR, useFlowRTC, useFlowRTC)
    model = importer.ImportItem(networkTpPath)
    
    Application.CreateNewProject()
    CurrentProject.RootFolder.Add(model)
    
    print "Saving application..."
    project_path = os.path.join(target_root_dir, testcase_name, testcase_name + ".dsproj")
    Application.SaveProjectAs(project_path)
    
    print "Running application..."
    Application.ActivityRunner.RunActivity(model)
    
    print "Saving application (2)..."
    Application.SaveProject()
    
    Application.CloseProject()
    
def main() :
    target_root_dir = "d:\\temp\\output"
    testcases_root_dir = "D:\\temp\\testcases"

    testcases_lst = find_testcases(testcases_root_dir)
    testcases_lst = filter_testcases (testcases_lst)
    
    for testcase_name, testcase_absdir, testcase_cnf in testcases_lst : 
        import_run_save (testcase_absdir, target_root_dir, testcase_name)
        
    print "Import script finished."

main()