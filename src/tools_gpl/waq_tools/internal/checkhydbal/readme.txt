Checkhydbal
===========

NOTE: this tool is for internal development and testing use only

Tool to check for water balance errors in a hyd-file data set, and optionaly extract the full
water balance per segment for the whole model or for a subset of segments to trace errors.

Usage:
checkhydbal -hydfile <input_hyd_file> [-fullbalance] [-subset <subsetfile>]
            [-starttime <starttime>] [-endtime <endtime>]

-hydfile <input hyd file> Name of the hyd-file to check
-fullbalance              Write full water balance output (optional).
-subsetfile <subset file> File to limit the full balance output to a subset of segments (optional).
-starttime <starttime>    Start of period to check in seconds since reference time (optional).
-endtime <endtime>        End of period to check in seconds since reference time (optional).

-h, --help, /?            Print help about Checkhydbal.

Input:
It is possible to supply a <subset file> to limit the full balance output to a subset of segments.
The file is in plain text, and the format of the subset file is:
<number of segments>
<segment #1>
...
<segment #n>

An example of a <subset file> is:
 3
 1
 5
 6

Output:
Important remark: please be aware that precipitation and evaporation are not explicit terms
in the water balance. That means that when there is precipitation and/or evaporation in the
hydrodynamic model, there will be water balance errors in the top layer.

The output of checkhydbal is provided in a number of files described below.

Output files:
<input hyd file>-checkhydbal.rep
Report file that contains:
* an overview of exchanges and connected segments (only when -fullbalance is on)
  The pointer file (poi-file) only contains from-to relations. Having an overview per
  segment of the exchanges, whether they are from or to the segment, and to or from what
  segments they are connected. This is very useful to identify water balance issues.
* Overview of maximum absolute and relative volume errors, discharge errors and minimum
  residence time (in seconds) and their locations, for all time steps.
* Summary of the maximum absolute and relative errors for the whole model.

<input hyd file>-checkhydbal-err.map
* Overview of absolute and relative volume errors, discharge errors and minimum residence
  time (in seconds) for all segments in time in a binary map-file

<input hyd file>-checkhydbal-sum.map
* Summary of maximum absolute and relative volume errors, discharge errors and minimum
  residence time (in seconds) for all segments in a binary map-file

<input hyd file>-relerrvmax.txt
<input hyd file>-relerrvavg.txt
* Plain text files of maximum absolute and relative volume errors for all segments for 
  comparison in the testbench.

<input hyd file>-checkhydbal-errfb.map, and
<input hyd file>-checkhydbal-errfb.his
One of these files only appears when -fullbalance is specified. The map-file appears by
default, and will contain the full balance per segment for the whole model. this can
sometimes be inconvenient for large models, so it is possible to limit this output in time
using a -starttime and -endtime setting. And it is posible to limit the output in space by
specifying a -subsetfile. 

Both files contain the following data per segment:
- Volume(t)            x water volume at the beginning of a time step (m3)
- Volume(t+dt)         x water volume at the end of a time step (m3)
- dVolume              x change in water volume during the time step (m3)
- horexch01-horexchnn  x amount of water from/to segment over horizontal exchange (Q*dt, m3)
- verexch01-verexch02  x amount of water from/to segment over vertical exchange (Q*dt, m3)
- V-error-m3           x absolute volume error during timestep (m3)
- V-rel-error          x volume error during timestep, relative to highest volume at t or t+dt (-)
- Q-error-m3/s         x absolute discharge error during timestep (m3/s)
- Q-rel-error          x discharge error during timestep, relative to the highest discharge (-)
- Res-tim-s            x residence time
