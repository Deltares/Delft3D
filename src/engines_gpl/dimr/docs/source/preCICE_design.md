## How to turn preCICE on or off in participant CLI
We will add command line arguments to the individual command line executables, for example dflowfm-cli or wave.
The DFlow-FM CLI already has command line options for changing the mode of running.
For example, it has the --no-display option to turn off the interacter or --partition to do a model partitioning.
We can add a --precice or --couple option to turn on model coupling through preCICE.
This option could also receive the path to the precice_config.xml as a value.
Then, a model runner can pass this flag as an option to ensure coupled model runs.
Similarly, the wave CLI already has an integer command line option for different model runs:
stand_alone(0), flow_online(1) and flow_mud_online(2).
We can improve this interface (keeping backward compatible numbers) and adding a similar --precice or --couple flag.

Other options that we prefer less:
1. Add keywords to the inputs of the programs that are coupling.\
This makes it more difficult for a coupler program to pass this info to the participants,\
and a user that wants to turn it on or off has to change the input for all participants in different ways.
2. Turn on preCICE automatically if a precice_config.xml is detected.\
This is fragile since it depends on the location of the xml and may break if a user that is unaware of preCICE accidentally throws away the xml\
(then no error can be given, since we use its presence to toggle preCICE).\
Further, users may want to run participants on their own sometimes,\
and if the config is thrown away it may be difficult to retrieve it.
