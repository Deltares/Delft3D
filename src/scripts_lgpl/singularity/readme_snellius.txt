Running Delft3dfm in an Apptainer (Singularity) container on Snellius:

1. Copy the script "submit_singularity_snellius.sh" into your working folder. 
   It is a script for executing Apptainer (Singularity) computations on Snellius.
   "Working folder" is the location of your dimr configuration file.
   
2. Modify "submit_singularity_snellius.sh" according to your own requirements.
   The comments in "submit_singularity_snellius.sh" will provide guidance.
   The number of nodes, and number of tasks per node, can be specified within the SBATCH directives at the top of the file.  
   
3. Run the script with SBATCH from your working folder: 
   sbatch ./submit_singularity_snellius.sh

4. Note: Some keywords are obsolete in Delft3dfm version 2022.03 and above.
   Where applicable, error messages will indicate which keywords should be removed from your mdu file.

5. Note: The mapmerge feature is not available via the "submit_singularity_snellius.sh" run script.
   If mapmerge is required, the user should copy the results to a local computer and run mapmerge there. 