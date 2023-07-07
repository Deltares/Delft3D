from DelftTools.Shell.Core.Workflow import ActivityRunner
import os
import os.path

# Find the first mentioned model in this project. 
# This funny for-break construction is necessary, because RootFolder.Models is a C# IEnumerable, which does not allow subscripting. 
model_name = ""
for x in RootFolder.Models : 
    model_name = str(x)
    break

# Find the model itself, and run it. 
model = RootFolder[model_name].Activities[0]
ActivityRunner.RunActivity(model)

# Find all output variables ('coverages') and the respective paths
l = []
for c in model.OutputCoverages : 
    l.append( (c.Components[0].Name, c.Store.Path) )

# Free access to the netCDF files. 
Application.CloseProject()

# Rename the output files to the name of the variables. 
# This works around both GUIDs and the bug where large parts of the filenames disappear when the variable name contains a slash. 
# Basically, this breaks the project. 
for i, (variable, path) in enumerate(l) :
    dest = os.path.join(os.path.dirname(path), variable+'.nc')
    if os.path.exists(dest) : 
        os.remove(dest)
    os.rename (path, dest)