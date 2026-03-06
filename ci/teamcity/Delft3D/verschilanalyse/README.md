# Verschilanalyse TeamCity Project
Contains Kotlin code for the 'VerschilAnalyse' TeamCity project. This project 
contains the following build configurations:

1. Start verschilanalyse models
2. Report verschilanalyse

The aim of this project is to do a weekly run of the 'verschilanalyse' models,
compare the output to the output of a previous (reference) run, and to generate a report. The
report should roughly contain:
- A summary of the numerical differences between the new run and the reference run.
- What models have run successfully and which models have crashed.
- Whether or not the numerical differences are within some pre-defined tolerance value.
- The total computation times of each model run.

There is a collection of verschilanalyse models that we run weekly. This
collection is maintained in our MinIO bucket: 'devops-test-verschilanalyse'.
You can [browse it online](https://s3-console.deltares.nl/browser/devops-test-verschilanalyse) 
if you have access to it. Each model is a collections of files that may
be subject to change over time. We have turned on 'versioning' in our bucket
to make sure that we can always recover the state of the models at a certain
point in time.

The output of each model is uploaded to the same bucket as the model input.
Since the output of a model can be quite large (several gigabytes) we
compress the output before uploading it to the bucket. The output is not
versioned. We also do not keep the output of the weekly runs forever: There's
a retention period after which the output files of the weekly runs are 
automatically deleted. The output of release versions of the d-hydro software
can be kept for longer periods.

In addition to the MinIO bucket, we also have access to some storage space on the 
P-drive, found at `/p/devops-dsc/verschilanalyse`. Because the d-hydro software
needs to read input data and write output data from the filesystem. We use this
verschilanalyse directory to temporarily store the input and output data of the
model runs. The output is automatically 'archived' to MinIO for longer term 
storage.

All of the model runs are scheduled on the [H7](https://publicwiki.deltares.nl/display/Deltareken/Deltares+Rekenfaciliteit+Home).
Most of the models run for several hours and some may take more than a day.
These models are not suitable to run on the TeamCity agents, because the agents
have limited resources and we don't want to occupy the agents for as long as the
models run. 

## Start verschilanalyse models
This build configuration schedules the verschilanalyse models to run on H7. This process consists of the following steps, which are all run on the H7:

- Synchronize the input data for all of the models from the MinIO bucket to the P-drive.
- Synchronize the reference data from the MinIO bucket for the comparison.
- Schedule all the models to run on H7.
- Archive the output of the models to the MinIO bucket.
- Run the verschillentool between the reference output and the newly generated output.
- Trigger the 'Report verschilanalyse' build configuration on TeamCity. This
 indicates that the verschilanalyse has been completed. The logs of the model runs and the output of the verschillentool must be made available to the 'Report verschilanalyse' build.

All of these steps are scheduled by submitting jobs to the [Slurm](https://publicwiki.deltares.nl/display/Deltareken/Basic+Slurm+commands) 
queue on the H7. There are scripts that perform these steps in the `bundle` 
directory. The 'bundle' contains not only scripts, but also the 
config files used by the 'verschillentool'. 

This build configuration only has two steps:
- Archive the contents of the `bundle` directory, and upload it to the H7.
- Log into the H7 using ssh, unpack the `bundle`, and execute the start_verschilanalyse.sh script.

There is a 'webhook' trigger for the 'Start verschilanalyse run'. The idea is 
that the verschilanalyse run is triggered when a new 'apptainer' is pushed to 
Harbor. After this project is loaded in TeamCity, a webhook needs to be set up
Harbor. The webhook needs to pass a 'token' in the authorization header. This 
token can be created in the TeamCity console. Only the 'Run build' permission on
the verschilanalyse project is required.

Note: We need quite a few credentials to run all of these steps. In TeamCity, we 
need credentials for the user on H7 to log in via ssh. Then, on the H7, we need 
access to the MinIO bucket, we need credentials to pull docker/apptainer images 
from Harbor. Finally, to trigger the 'Report verschilanalyse models' build, we
also need TeamCity credentials on the H7. Instead of passing all of these 
credentials from TeamCity to the H7, we instead store credentials in files in 
the H7 user's home directory. These files have restricted permissions, so only 
the H7 user can read and write them. If we're uncomfortable with this 
solution we should think of a different solution.

## Report verschilanalyse models
Currently, all this build configuration does is download the `report.zip` with 
the logs of the simulations and the output of the verschillentool. This can be 
used to report 'test results' on TeamCity and to send an emails. 
This work is picked up in a different JIRA issue.
