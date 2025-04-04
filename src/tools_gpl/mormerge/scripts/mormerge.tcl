#! /usr/bin/tclsh

# DOC
#
# Getting engine to run tcl-scripts:
# See http://tcl.activestate.com
#
# mormerge.tcl - Script to start parallel FLOW runs
#                and a morphological merging executable
#
# Copyright (C)  Stichting Deltares, 2011-2013.
#
# Authors: Adri Mourits
#
# General information:
#
#
# ENDDOC
#
#

global version
set version "2.30"

global timelimit
set timelimit "01:00:00"

global queue
#set queue "normal-e3"
set queue "16vcpu"

global debug
set debug 0

global processes
set processes {}

global processhandles
set processhandles {}

global scriptscreated
set scriptscreated {}

# platform: linux or windows
global platform
set platform "unknown"

global D3D_HOME
set D3D_HOME "unknown"

# OperatingSystem: Needed to distinct XP and Vista when platform=win32
global OS
set OS "unknown"

global clusterName
set clusterName "h7"

# platform=linux: slash=/
# platform=win32: slash=\
# although the / is also accepted on Windows, it is NOT accepted in all directory related commands in batch files.
# the \ is necessary on Windows
global slash
set slash "unknown"

global datetime
set datetime [clock format [clock seconds] -format "%Y%m%d_%H%M%S"]

global jobid
set jobid "local"

global idstring
set idstring " "

# waittime is used to wait for files to appear (1000 = 1 sec)
# default: 500
global waittime
set waittime 500

# maximum trials to wait for a file to appear before continuing
# default: 100
global maxWaitCount
set maxWaitCount 100

# processStarttime is used to wait between the start of two processes (1000 = 1 sec)
# default: 10000
global processStarttime
set processStarttime 10000

global queuesys
set queuesys "slurm"

global DELTAQ_LocalTempDir
set DELTAQ_LocalTempDir " "

global remoteShell
set remoteShell "ssh"

global flowNames
set flowNames {trisim delftflow deltares_hydro d_hydro}

global localhost
set localhost 0


# --------------------------------------------------------------------
#   Procedure: debug-prompt
# --------------------------------------------------------------------
#
proc debug-prompt {} {
   set cmd ""
   set level [expr {[info level]-1}]
   set prompt "DEBUG\[$level\]% "
   while 1 {
      puts -nonewline $prompt
      flush stdout
      gets stdin line
      append cmd $line\n
      if {[info complete $cmd]} {
         set code [catch {uplevel #$level $cmd} result]
         if {$code == 0 && [string length $result]} {
            puts stdout $result
         } elseif {$code == 3} {
            # break
            error "aborted debugger"
         } elseif {$code == 4} {
            # continue
            return
         } else {
            puts stderr $result
         }
         set prompt "DEBUG\[$level\]% "
         set cmd ""
      } else {
         set prompt " "
      }
   }
}


# --------------------------------------------------------------------
#   Procedure: putsDebug
#   Author:    Adri Mourits
#   Purpose:   Only write output when in debug mode
#   Context:   Uses global variable debug
#   Summary:
#
#   Arguments:
#      message      Message to be displayed
#   Return value:
#              none
#   Note:
#
# --------------------------------------------------------------------
proc putsDebug { message } {
   global debug
   if {$debug} {
      puts $message
   }
}


# --------------------------------------------------------------------
#   Procedure: printUsage
#   Author:    Adri Mourits
#   Purpose:   Prints the usage of this script
#   Context:
#   Summary:
#
#   Arguments:
#              none
#   Return value:
#              none
#   Note:
#
# --------------------------------------------------------------------
proc printUsage { } {
   #
   # Do not mention the -n flag! It is used internally only!
   #
   puts "USAGE : mormerge.tcl -s <script> -i <infile> \[-t -d -A\]"
   puts "    -s <script> : Name of this script, including path"
   puts "    -i <infile> : Name of the input file"
   puts "    -t          : Used in the Delft3D testbank"
   puts "    -d          : Switch debug on"
   puts "    -A          : Do not run tdatom"
}


# --------------------------------------------------------------------
#   Procedure: setPlatform
#   Author:    Adri Mourits
#   Purpose:   Detect the platform and set related global parameters
#   Context:
#   Summary:
#
#   Arguments:
#              none
#   Return value:
#              none
#   Note:
#
# --------------------------------------------------------------------
proc setPlatform { } {
   global env
   global platform
   global slash
   global OS
   global clusterName

   if {$::tcl_platform(platform) == "windows"} {
      set platform "windows"
   } else {
      set platform "linux"
   }
   if {[info exists env(SGE_CLUSTER_NAME)]} {
      set clusterName $env(SGE_CLUSTER_NAME)
   }
   set mainVersion [string range $::tcl_platform(osVersion) 0 0]
   if {$platform == "windows"} {
      set slash "\\"
      if {$mainVersion == "5"} {
         set OS "XP"
      } elseif {$mainVersion == "6"} {
         set OS "Vista"
      } else {
         puts "WARNING: This Windows version is not recognised as XP or Vista. This application is not tested on this platform."
      }
   } else {
      # linux
      set slash "/"
   }
}


# --------------------------------------------------------------------
#   Author:    Arjen Markus
# runcmd --
#     Run an external command and catch the output to stdout/stderr
# Arguments:
#     command     The command to invoke
#     tag         The tag to use for the output in the output window
# Returns:
#     Nothing
# Side effect:
#     Command started, output will be redirected to the output window
# --------------------------------------------------------------------
proc runcmd {command tag} {
    global processes
    global processhandles

    set name [format "|\"%s\"" $command]
    regsub -all {\\} $name {\\\\} name
    set infile [open $name "r"]

    fconfigure $infile -buffering none
    #fconfigure $infile -buffering line
    fileevent $infile readable [list getInput $infile $tag]

    lappend processhandles $infile
    lappend processes [pid $infile]
    return [pid $infile]
}


# --------------------------------------------------------------------
#   Author:    Arjen Markus
# getInput --
#     Get the text that an external program writes to stdout/stderr
# Arguments:
#     channel        Channel to the external program
#     tag            The tag that identifies the program (different colours)
# Returns:
#     Nothing
# --------------------------------------------------------------------
proc getInput {channel tag} {
    global processes

    if { [gets $channel line] >= 0 } {
        puts "$tag : $line"
    } else {
        set idx [lsearch $processes [pid $channel]]
        set processes [lreplace $processes $idx $idx]
        puts "$tag : Finished"
        if { [catch {close $channel} errmsg] } {
            puts "\nERROR: tag=$tag : $errmsg"
        }
        if { [llength $processes] == 0 } {
            set ::finished 1
        } else {
            putsDebug "Waiting for [llength $processes] processes to finish..."
        }
    }
}


# --------------------------------------------------------------------
#   Procedure: readNodeFile
#   Author:    Adri Mourits
#   Purpose:   Read File containing the names of the nodes on which
#              to run
#   Context:   Environment parameter PBS_NODEFILE is used, also for
#              local runs
#   Summary:
#
#   Arguments:
#      nodefilename   Name of the file to be scanned
#      count          Number of node names found
#      nodes          List of the node names
#   Return value:
#              count
#              nodes
#   Note:
#
# --------------------------------------------------------------------
proc readNodeFile { nodefilename count nodes } {
   global clusterName
   upvar $count numnodes
   upvar $nodes nodelist

   putsDebug "Reading file $nodefilename..."
   set success 0
   set trycnt  0
   while {! $success} {
      incr trycnt
      set nodelist {}
      set nodefile [open "$nodefilename" "r"]
      while {[gets $nodefile aline] >= 0} {
         if {$clusterName == "h5" || $clusterName == "h6"} {
            set words [split $aline]
            set aline [lindex $words 0]
         }
         putsDebug "line:$aline"
         lappend nodelist [string trim $aline]
      }
      close $nodefile
      if {$numnodes == [llength $nodelist]} {
         set success 1
      } else {
         if {$trycnt <= 6} {
            puts "Not able to read nodefile $nodefilename completely"
            puts "Trying again after 5 seconds"
            after 5000
         } else {
            puts "ERROR: Number of nodes specified in input file: $numnodes"
            puts "       Number of nodes specified in nodefile  : [llength $nodelist]"
            exit 1
         }
      }
   }
}



# --------------------------------------------------------------------
#   Procedure: readArguments
#   Author:    Adri Mourits
#   Purpose:   Read command line arguments
#   Context:   Exedir and inputfilename must be specified
#   Summary:
#
#   Arguments:
#      nodefilename   Name of the file to be scanned
#      count          Number of node names found
#      nodes          List of the node names
#   Return value:
#              count
#              nodes
#   Note:
#
# --------------------------------------------------------------------
proc readArguments { argv alist } {
   global debug
   global platform
   global slash
   upvar $alist arglist

   set inputfilename " "
   set scriptfilename " "
   set availablenodes 0
   set testbank 0
   set runtdatom 1
   set argcount  [llength $argv]
   set argnumber 0
   while { $argnumber < $argcount } {
      set arg [lindex $argv $argnumber ]
      switch -- $arg {
         "-A" {
            set runtdatom 0
         }
         "-debug" -
         "-D" -
         "-d" {
            set debug 1
         }
         "-input" -
         "-I" -
         "-i" {
            incr argnumber
            if {$argnumber >= $argcount} {
               puts "ERROR : On the command line:"
               puts "        Missing input file name behind flag -i"
               printUsage
               exit 1
            }
            set inputfilename [file nativename [lindex $argv $argnumber]]
         }
         "-nodes" -
         "-N" -
         "-n" {
            incr argnumber
            if {$argnumber >= $argcount} {
               puts "ERROR : On the command line:"
               puts "        Missing number of nodes behind flag -n"
               printUsage
               exit 1
            }
            set availablenodes [lindex $argv $argnumber]
         }
         "-script" -
         "-S" -
         "-s" {
            incr argnumber
            if {$argnumber >= $argcount} {
               puts "ERROR : On the command line:"
               puts "        Missing script file name behind flag -s"
               printUsage
               exit 1
            }
            set scriptfilename [lindex $argv $argnumber]
         }
         "-testbank" -
         "-T" -
         "-t" {
            set testbank 1
         }
         default {
            puts "WARNING: Skipping commandline argument $arg"
         }
      }
      incr argnumber
   }
   #if {$exedir == " "} {
   #   puts "ERROR: Expecting exedir specification on the commandline, example:"
   #   puts "       mormerge.tcl -x /delft3d/wlinux/flow/bin"
   #   exit 1
   #}
   if {$inputfilename == " "} {
      puts "ERROR: Expecting inputfile specification on the commandline, example:"
      puts "       mormerge.tcl -i basin.mm"
      printUsage
      exit 1
   }
   if {$scriptfilename == " "} {
      puts "ERROR: Expecting scriptfile specification on the commandline, example:"
      puts "       mormerge.tcl -s \$exedir/../../scripts/mormerge.tcl"
      printUsage
      exit 1
   }
   set arglist(availablenodes) $availablenodes
   set arglist(testbank)       $testbank
   set arglist(infile)         $inputfilename
   set arglist(runtdatom)      $runtdatom
   set curdir [file nativename [pwd] ]
   #
   # reset script filename directory, to use pwd notation
   #
   set ftail [file tail $scriptfilename]
   set fdir  [file dirname $scriptfilename]
   cd $fdir
   if { [file exists $ftail] } {
      set arglist(mmscriptfile) [file nativename [file join [pwd] $ftail] ]
   } else {
      puts "ERROR: Expecting file $ftail in directory $fdir"
      exit 1
   }
   cd $curdir
}



# --------------------------------------------------------------------
#   Procedure: checkFil
#   Author:    Adri Mourits
#   Purpose:   check whether a directory/executable exists
#   Context:   -
#   Summary:   -
#
#   Arguments:
#      fildir    "dir" : filnam is a directory
#                "exe" : filnam is an executable
#      filnam    Name of the directory/executable to check
#   Return value:
#              None
#   Note:
#
# --------------------------------------------------------------------
proc checkFil { fildir filnam } {
   switch -- $fildir {
      "dir" {
         if { [file isdirectory $filnam] == 0 } {
            puts "ERROR: Directory  $filnam  not found"
            exit 1
         }
      }
      "exe" {
         if { [file exists $filnam] == 0 } {
            puts "ERROR: Executable  $filnam  not found"
            exit 1
         }
      }
      default {
         puts "ERROR: Procedure checkFil can only check dir and exe"
         exit 1
      }
   }
}


# --------------------------------------------------------------------
#   Procedure: setD3DHOME
#   Author:    Adri Mourits
#   Purpose:   Set internal parameter D3D_HOME, based on a path to a
#              executable or script
#   Context:   -
#   Summary:
#
#   Arguments:
#      aPath   Path to an executable or script inside the binary set
#   Return value:
#              On Linux:
#                  The executable/script is assumed to be located in 
#                  D3D_HOME/bin
#                  Return aPath minus the last component
#              On Windows:
#                  The executable/script is assumed to be located in 
#                  D3D_HOME/<kernel>/bin
#                  Return aPath minus the last two components
#   Note:
#       1. This procedure is introduced to replace the usage of 
#          environment parameter "ARCH"
#       2. This internal parameter D3D_HOME intentionally contains
#          ARCH (e.g. "path/to/installation/lnx64" or "path/to/installation/x64")
#          to be able to replace the usage of "ARCH"
# --------------------------------------------------------------------
proc setD3DHOME { aPath } {
    global platform
    global D3D_HOME

    set pathSplit [file split $aPath]
    if { $platform == "linux" } {
        set D3D_HOME [file join {*}[lrange $pathSplit 0 end-1] ]
    } else {
        set D3D_HOME [file join {*}[lrange $pathSplit 0 end-2] ]
    }
    putsDebug "\nInternal parameter D3D_HOME is set to: $D3D_HOME"
}


# --------------------------------------------------------------------
#   Procedure: readInputFile
#   Author:    Adri Mourits
#   Purpose:   Read the specified input file
#   Context:   -
#   Summary:
#
#   Arguments:
#      inputfilename    Name of the file to be scanned
#      other arguments  Arguments to be read
#   Return value:
#              all arguments except inputfilename
#   Note:
#
# --------------------------------------------------------------------
proc readInputFile { inputfilename iflist } {
   upvar $iflist  infillist
   global rootdir
   global debug
   global platform
   global queuesys
   global queue
   global flowNames
   global localhost
   global timelimit

   set runscript       " "
   set runscriptargs   " "
   set dimrexedir      " "
   set dimrexename     "run_dimr"
   set dimrargs        " "
   set flowexedir      " "
   set flowexename     "d_hydro"
   set flowargs        " "
   set waveexedir      " "
   set waveargs        " "
   set swanbatdir      " "
   set mormergeexedir  " "
   set workdir         " "
   set numconditions   0
   set conditions      {}
   set weights         {}
   set totalweight     0
   set localrun        0
   set numnodes        1

   set inputfile [open "$inputfilename" "r"]
   while {[gets $inputfile aline] >= 0} {
      set words [split $aline "="]
      set keyword [string tolower [string trim [lindex $words 0]]]
      set value   [string trim [lindex $words 1]]
      switch -- $keyword {
         "runscript" {
            set runscript $value
         }
         "runscriptargs" {
            set runscriptargs $value
         }
         "dimrexedir" {
            set dimrexedir $value
         }
         "dimrargs" {
            set dimrargs $value
         }
         "flowexedir" {
            set flowexedir $value
         }
         "flowexename" {
            set flowexename [string tolower $value]
         }
         "flowargs" {
            set flowargs $value
         }
         "waveexedir" {
            set waveexedir $value
         }
         "waveargs" {
            set waveargs $value
         }
         "swanbatdir" {
            set swanbatdir $value
         }
         "mormergeexedir" {
            set mormergeexedir $value
         }
         "nodes" {
            if {$value == "local"} {
               set queuesys "none"
               set numnodes 1
               # needed to check consistency with queuesys:
               set localhost 1
            } else {
               set numnodes $value
            }
         }
         "condition:weight" {
            set valparts  [split $value ":"]
            set condition [string trim [lindex $valparts 0]]
            set weight    [string trim [lindex $valparts 1]]
            if { [lsearch $conditions $condition] >= 0 } {
               puts "ERROR: In file $inputfilename"
               puts "       condition $condition is specified twice"
               close $inputfile
               exit 1
            }
            lappend conditions $condition
            lappend weights $weight
            set totalweight [expr $totalweight + $weight]
         }
         "workdir" {
            set workdir $value
         }
         "debug" {
            if { $debug == 1 && $value == 0 } {
               puts "WARNING: Conflict on specifying debug flag"
               puts "         \"-d\" specified on command line and \"debug = 0\" in input file"
               puts "         debug flag is switched on"
            } else {
               set debug $value
            }
         }
         "queuesystem" {
            set queuesys [string tolower $value]
         }
         "queue" {
            set queue [string tolower $value]
         }
         "timelimit" {
            set timelimit [string tolower $value]
         }
         default {
            puts "WARNING: In file $inputfilename"
            puts "         Skipping line $aline"
         }
      }
   }
   set numconditions [llength $conditions]
   close $inputfile

   if {$numnodes != 1 && $queuesys == "slurm"} {
      puts "ERROR: For SLURM only nodes = 1 has been implemented (not $nodes)"
      exit 1
   }
   # First: OR flowexedir, OR dimrexedir must be defined
   if {$flowexedir == " " && $dimrexedir == " " && $runscript == " "} {
      puts "ERROR: In file $inputfilename"
      puts "       Expecting executable direction specification, examples:"
      puts "       dimrexedir         = /opt/delft3dfm/2.0.2_54830/lnx64/bin"
      puts "       flowexedir         = /opt/delft3d4/Delft3D-FLOW_WAVE/6.02.13.7545/lnx64/flow2d3d/bin"
      puts "       runscript          = /opt/delft3d4/Delft3D-FLOW_WAVE/6.02.13.7545/lnx64/bin/run.sh"
      exit 1
   }
   if {$runscript == " " && $dimrexedir == " " && $flowexename ni $flowNames} {
      puts "ERROR: In file $inputfilename"
      puts "       Expecting executable name being \"deltares_hydro\" (default), \"trisim\" (default) or \"delftflow\" or \"runscript\", example:"
      puts "       flowexename         = delftflow"
      exit 1
   }

   # Then: an argument must be defined
   if {$flowexedir !=  " " && $flowargs == " "} {
      puts "ERROR: In file $inputfilename"
      puts "       Expecting flow arguments specification, examples:"
      puts "       flowargs         = config_d_hydro.xml"
      exit 1
   }
   if {$dimrexedir != " " && $dimrargs == " "} {
      puts "ERROR: In file $inputfilename"
      puts "       Expecting flow arguments specification, examples:"
      puts "       dimrargs         = dimr_config.xml"
      exit 1
   }
   if {$mormergeexedir == " "} {
      puts "WARNING: In file $inputfilename"
      puts "       No mormerge executable direction specification, example:"
      puts "       mormergeexedir         = /delft3d/flow/bin/wlinux"
      puts "       mormergeexedir is set to $flowexedir"
      set mormergeexedir $flowexedir
   }
   if {$waveexedir == " "} {
      puts "WARNING: In file $inputfilename"
      puts "       No wave executable direction specification, example:"
      puts "       waveexedir         = /delft3d/flow/bin/wlinux"
      if {$flowexedir != " "} {
         set waveexedir $flowexedir
      } else {
         set waveexedir $dimrexedir
      }
      puts "       waveexedir is set to $waveexedir"
   }
   if {$numnodes == 0} {
      puts "WARNING: In file $inputfilename"
      puts "       Number of nodes not specified; assuming run on local host"
      puts "       2 Example specification line:"
      puts "       nodes         = 2"
      puts "       nodes         = local"
      set numnodes 1
      set queuesys "none"
      # needed to check consistency with queuesys:
      set localhost 1
   }
   if {$numconditions < 2} {
      puts "ERROR: In file $inputfilename"
      puts "       Expecting at least two condition specifications, example:"
      puts "       condition:weight =  0deg : 0.2"
      exit 1
   }
   if {$workdir == " "} {
      set workdir $rootdir
   }
   if {$queuesys != "none"} {
      if {$queuesys != "torque" && $queuesys != "sge" && $queuesys != "slurm"} {
         puts "ERROR: In file $inputfilename"
         puts "       Expecting queueSystem to be SGE, TORQUE, SLURM, example:"
         puts "       queueSystem         = slurm"
         exit 1
      }
      if {$queuesys == "torque"} {
         puts "WARNING: In file $inputfilename"
         puts "         queueSystem TORQUE is outdated; use SGE if possible, example:"
         puts "         queueSystem         = SGE"
      } else {
         if {[string compare -nocase [string range $workdir 1 3] "var"] == 0 || [string compare -nocase [string range $workdir 1 3] "tmp"] == 0} {
            # set workdir "sgestage"
            # puts "WARNING: copying to $workdir is done by SGE (StageIn, StageOut)"
         }
         puts "INFO: using queue $queue"
      }
      if {$localhost} {
         set queuesys "none"
         puts "WARNING: In file $inputfilename"
         puts "         Ignoring queueing system specification; running on local system"
      }
   }

   set curdir [file nativename [pwd] ]

   if {$workdir == "sgestage"} {
      set localrun 1
      puts "WARNING: copying to and from $workdir is done by SGE (StageIn, StageOut)"
   } else {
      # reset workdir, to use pwd notation
      #
      cd $workdir
      set workdir [file nativename [pwd] ]
      if { [string equal $workdir $rootdir] } {
         set localrun 0
      } else {
         set localrun 1
      }
   }
   #
   # reset runscriptdir, to use pwd notation
   #
   cd $curdir
   if { ! [string equal $runscript " "] } {
      cd [file dirname $runscript]
      set runscript [file join [file nativename [pwd] ] [file tail $runscript] ]
      setD3DHOME [file dirname $runscript]
   }
   #
   # reset dimrexedir, to use pwd notation
   #
   cd $curdir
   if { ! [string equal $dimrexedir " "] } {
      cd $dimrexedir
      set dimrexedir [file nativename [pwd] ]
      setD3DHOME $dimrexedir
   }
   #
   # reset flowexedir, to use pwd notation
   #
   cd $curdir
   if { ! [string equal $flowexedir " "] } {
      cd $flowexedir
      set flowexedir [file nativename [pwd] ]
      setD3DHOME $flowexedir
   }
   #
   # reset mormergeexedir, to use pwd notation
   #
   cd $curdir
   if { ! [string equal $mormergeexedir " "] } {
      cd $mormergeexedir
      set mormergeexedir [file nativename [pwd] ]
   }
   #
   # reset waveexedir, to use pwd notation
   #
   cd $curdir
   if { ! [string equal $waveexedir " "] } {
      cd $waveexedir
      set waveexedir [file nativename [pwd] ]
   }
   #
   # reset swanbatdir, to use pwd notation
   #
   cd $curdir
   if { ! [string equal $swanbatdir " "] } {
      cd $swanbatdir
      set swanbatdir [file nativename [pwd] ]
   }

   cd $curdir

   set infillist(runscript)      $runscript
   set infillist(runscriptargs)  $runscriptargs
   set infillist(dimrexedir)     $dimrexedir
   set infillist(dimrexename)    $dimrexename
   set infillist(dimrargs)       $dimrargs
   set infillist(flowexedir)     $flowexedir
   set infillist(flowexename)    $flowexename
   set infillist(flowargs)       $flowargs
   set infillist(waveexedir)     $waveexedir
   set infillist(waveargs)       $waveargs
   set infillist(swanbatdir)     $swanbatdir
   set infillist(mormergeexedir) $mormergeexedir
   set infillist(workdir)        $workdir
   set infillist(numconditions)  $numconditions
   set infillist(conditions)     $conditions
   set infillist(weights)        $weights
   set infillist(totalweight)    $totalweight
   set infillist(localrun)       $localrun
   set infillist(numnodes)       $numnodes
}



# --------------------------------------------------------------------
#   Procedure: scanDDBoundFile
#   Author:    Adri Mourits
#   Purpose:   Read ddbound file to obtain the runids
#   Context:   For each runid, tdatom must be executed
#              For each runid, mergeexe must be started
#   Summary:
#
#   Arguments:
#      ddbfilnam    Name of the ddbound file to be scanned
#      resultlist   list of resulting runids
#   Return value:
#              resultlist
#   Note:
#
# --------------------------------------------------------------------
proc scanDDBoundFile { ddbfilnam inputdir resultlist } {
   upvar $resultlist runids

   set runids {}
   set inputfilename [file nativename [file join $inputdir $ddbfilnam] ]
   set inputfile [open "$inputfilename" "r"]
   set firstline 1
   set xml 0
   while {[gets $inputfile aline] >= 0} {
      if { $firstline } {
         if { [string first "<?xml" $aline] >= 0 } {
            set xml 1
         }
         set firstline 0
      }
      #puts "ddbline:$aline"
      if { $xml } {
         set searchstring "<domain name"
         set domnamestart [string first $searchstring $aline]
         if { $domnamestart >= 0 } {
            set domnamestart [string first "\"" $aline $domnamestart]
            incr domnamestart
            set domnameend [string first "\"" $aline [expr $domnamestart + 1]]
            incr domnameend -1
            set runid [string range $aline $domnamestart $domnameend]
            if { [lsearch $runids $runid] == -1 } {
               lappend runids $runid
            }
         }
      } else {
         # The space character is going to be used for splitting
         # Replace all sequences of spaces by one single space
         regsub -all {[ ]+} [string trim $aline] " " aline
         #puts "$aline:"
         set words [split $aline]
         set runid1 [file rootname [lindex $words 0]]
         set runid2 [file rootname [lindex $words 5]]
         #puts "runids: $runid1 $runid2"
         if { [lsearch $runids $runid1] == -1 } {
            lappend runids $runid1
         }
         if { [lsearch $runids $runid2] == -1 } {
            lappend runids $runid2
         }
      }
   }
   close $inputfile
}



# --------------------------------------------------------------------
#   Procedure: checkWaveOnline
#   Author:    Adri Mourits
#   Purpose:   Check whether one of the related mdf-files
#              has flag waveol switched on
#   Context:   Needed to know whether to start wave or not
#   Summary:
#
#   Arguments:
#      inputdir
#      runids
#      inputarray
#   Return value:
#              0 waveol is false for all subdomains
#              1 waveol is true for all subdomains
#              exit with error when a conflict occurs between subdomains
#   Note:
#
# --------------------------------------------------------------------
proc checkWaveOnline { inputdir inputfilename runids inflist} {
   upvar $inflist    infillist

   set waveol 0
   set curdir [file nativename [pwd] ]
   cd $inputdir

   if { $infillist(dimrexedir) != " " } {
      # The script run_dimr is taking care of wave
      return $waveol
   }

   set checkFinished 0
   foreach runid $runids {
      set mdffile [open "$runid.mdf" "r"]
      while {[gets $mdffile aline] >= 0} {
         if { [string match -nocase "*waveol*" $aline] } {
            set keyval [split $aline "="]
            set value [lindex $keyval 1]
            set firsthash [string first "#" $value]
            set secondhash [string first "#" $value [expr $firsthash + 1]]
            set value [string range $value $firsthash $secondhash]
            if { [string match -nocase "*y*" $value]   || \
                 [string match -nocase "*true*" $value]    } {
               set waveol 1
               set checkFinished 1
               break
            }
         }
      }
      close $mdffile
      if { $checkFinished } { break }
   }
   cd $curdir

   if { $waveol } {
      if { $infillist(waveexedir) == " " } {
         puts "ERROR: In file $inputfilename"
         puts "       No wave executable directory specified"
         puts "       Needed for a wave OnLine calculation, example:"
         puts "       waveexedir = /delft3d/wave/intel/bin"
         exit 1
      }
      if { $infillist(waveargs) == " " } {
         puts "ERROR: In file $inputfilename"
         puts "       No wave arguments specified"
         puts "       Needed for a wave OnLine calculation, example:"
         puts "       waveargs = bas.mdw 1"
         exit 1
      }
      if { $infillist(swanbatdir) == " " } {
         puts "ERROR: In file $inputfilename"
         puts "       No directory specified containing the relevant swan.bat file"
         puts "       Needed for a wave OnLine calculation, example:"
         puts "       swanbatdir = /delft3d/wave/default"
         exit 1
      }
   }
   return $waveol
}



# --------------------------------------------------------------------
#   Procedure: getRunids
#   Author:    Adri Mourits
#   Purpose:   Obtain the runids
#   Context:   Either the runid is directly in fargs,
#              or fargs contains a ddbound file to be scanned
#   Summary:
#
#   Arguments:
#      fargs    Flow argument list, to be used to get the runids
#      numdoms  number of runids
#      rids     list, going to contain the runids
#   Return value:
#              numdoms
#              rids
#   Note:
#
# --------------------------------------------------------------------
proc getRunids { flowargs dimrargs runscript inputdir numdoms rids} {
   upvar $numdoms numdomains
   upvar $rids    runids

   set waveonline 0
   set numdomains 0
   set runids {}
   if {$dimrargs != " " || $runscript != " "} {
      lappend runids "singledomain"
   } else {
      if {[string first "<ddbFile>" $flowargs] >=0} {
         regsub -all "<ddbFile>" $flowargs "" fargs
         regsub -all "</ddbFile>" $fargs "" fargs
         set fargs [string trim $fargs]
         scanDDBoundFile $fargs $inputdir runids
      } elseif {[string first "<mdfFile>" $flowargs] >=0} {
         regsub -all "<mdfFile>" $flowargs "" fargs
         regsub -all "</mdfFile>" $fargs "" fargs
         regsub -all {.mdf} $fargs "" runid
         set runid [string trim $runid]
         lappend runids $runid
      } else {
         set fargs [split $flowargs]
         set argcount [llength $fargs]
         if {$argcount == 1} {
            #the argument is a ddbound file
            scanDDBoundFile $fargs $inputdir runids
         }
         set words [split $flowargs "="]
         if {[llength $words] == 2} {
            # the argument has the shape "keyword = value" (from a config.ini file)
            set keyword [string tolower [string trim [lindex $words 0]]]
            set value   [string trim [lindex $words 1]]
            switch -- $keyword {
               "mdffile" {
                     set runid $value
                     regsub -all {.mdf} $runid "" runid
                     lappend runids $runid
               }
               "ddbfile" {
                     scanDDBoundFile $value $inputdir runids
               }
            }
         } else {
            # the arguments are guided with flags like -r or -c
            set argnumber 0
            while { $argnumber < $argcount } {
               set arg [lindex $fargs $argnumber ]
               switch -- $arg {
                  "-R" -
                  "-r" {
                     incr argnumber
                     set runid [lindex $fargs $argnumber]
                     regsub -all {.mdf} $runid "" runid
                     lappend runids $runid
                  }
                  "-C" -
                  "-c" {
                     incr argnumber
                     set ddboundfilename [lindex $fargs $argnumber]
                     scanDDBoundFile $ddboundfilename $inputdir runids
                  }
                  default {
                     # skip; just pass through to trisim
                  }
               }
               incr argnumber
            }
         }
      }
   }
   set numdomains [llength $runids]
   if {$numdomains < 1} {
      puts "ERROR: In flow arguments \"$flowargs\":"
      puts "       Expecting at least one runid to be specified by flowargs"
      exit 1
   }
}



# --------------------------------------------------------------------
#   Procedure: waitForNodes
#   Author:    Adri Mourits
#   Purpose:   Create a Linux scriptfile in which qsub is called
#              Executes the script and finishes
#   Context:   Mormerge will be called by qsub when the requested
#              number of nodes is available
#   Summary:
#
#   Arguments:
#      numnodes    Number of nodes requested
#      mergedir    Directory in which the scriptfile is placed and executed
#   Return value:
#              none
#   Note:
#
# --------------------------------------------------------------------
proc waitForNodes { mergedir alist inflist } {
   global version
   global scriptscreated
   global waittime
   global queuesys
   global queue
   global clusterName
   global timelimit
   upvar $inflist infillist
   upvar $alist   arglist

   cd $mergedir
   set scriptnamewait "d3d-mormerge_qsubwait.sh"
   set scriptnamego   "d3d-mormerge_qsubgo.sh"
   lappend scriptscreated $scriptnamewait
   lappend scriptscreated $scriptnamego
   set sfilnamewait [file nativename [file join $mergedir $scriptnamewait] ]
   set sfilnamego   [file nativename [file join $mergedir $scriptnamego]   ]

   # Create wait script
   set scriptfile [open "$sfilnamewait" "w"]
   puts $scriptfile "#!/bin/bash"
   if {$queuesys == "slurm"} {
      puts $scriptfile "\n# This script file is automatically generated by mormerge.tcl, version $version\n"
      puts $scriptfile "sbatch $scriptnamego"
   } else {
      puts $scriptfile "#\$ -V"
      puts $scriptfile "#\$ -j yes"
      puts $scriptfile "#\$ -cwd"
      puts $scriptfile "\n# This script file is automatically generated by mormerge.tcl, version $version\n"
      if {$queuesys == "sge"} {
         if {$clusterName != "h5" && $clusterName != "h6"} {
            puts $scriptfile "\n. /opt/sge/InitSGE\n"
         }
         puts $scriptfile "qsub -V -q $queue -pe distrib $infillist(numnodes) $scriptnamego"
      }
      else {
         puts $scriptfile "qsub -l nodes=$infillist(numnodes) $scriptnamego"
      }
   }
   puts $scriptfile "\n# End of script\n"
   close $scriptfile
   if { [catch {exec chmod u+x $scriptnamewait} errmsg] } {
      putsDebug "\nWARNING : Unable to chmod file $scriptnamewait:"
      putsDebug   "          $errmsg"
   }
   #
   # Create go script
   set scriptfile [open "$sfilnamego" "w"]
   puts $scriptfile "#!/bin/bash"
   if {$queuesys == "slurm"} {
      puts $scriptfile "#SBATCH --time $timelimit"
      puts $scriptfile "#SBATCH --partition=$queue"
      puts $scriptfile "export I_MPI_FABRICS=ofi"
      puts $scriptfile "export FI_PROVIDER=tcp"
      puts $scriptfile "export I_MPI_OFI_PROVIDER=tcp"
      puts $scriptfile "\n# This script file is automatically generated by mormerge.tcl, version $version\n"
      puts $scriptfile "\n# See also: https://publicwiki.deltares.nl/display/Deltareken/Compute+nodes."
      puts $scriptfile "\nmodule load intelmpi/2021.9.0"
   } else {
      puts $scriptfile "#\$ -V"
      puts $scriptfile "#\$ -j yes"
      puts $scriptfile "#\$ -cwd"
      puts $scriptfile "\n# This script file is automatically generated by mormerge.tcl, version $version\n"
      if {$queuesys == "sge"} {
         if {$clusterName != "h5" && $clusterName != "h6"} {
            puts $scriptfile "\n. /opt/sge/InitSGE\n"
         }
         # This seems to be the only position where DELTAQ_LocalTempDir can be read.
         # Put it in SGE_LocalTempDir and read it from the main loop
         puts $scriptfile "\nexport SGE_LocalTempDir=\$DELTAQ_LocalTempDir\n"
      }
   }
   if {$infillist(workdir) == "sgestage"} {
      if {$clusterName == "h7"} {
         puts "ERROR: sgestage is not implemented for h7"
         exit 1
      }
      puts $scriptfile "cd .."
      puts $scriptfile "StageIn -v\n"
      puts $scriptfile "\n# Jump to run directory\n"
      puts $scriptfile "cd \$DELTAQ_LocalTempDir\n"
      puts $scriptfile "cd merge"
   }
   if {$queuesys == "slurm"} {
      puts $scriptfile "/usr/bin/tclsh $arglist(mmscriptfile) -i [file join $infillist(workdir) [file tail $arglist(infile)]] -s $arglist(mmscriptfile) -n $infillist(numnodes)"
   } else {
      puts $scriptfile "/usr/bin/tclsh $arglist(mmscriptfile) -i [file join \$DELTAQ_LocalTempDir "merge" [file tail $arglist(infile)]] -s $arglist(mmscriptfile) -n $infillist(numnodes)"
   }
   if {$infillist(workdir) == "sgestage"} {
      puts $scriptfile "cd .."
      puts $scriptfile "StageOut -v\n"
   }
   puts $scriptfile "\n# End of script\n"
   close $scriptfile
   if { [catch {exec chmod u+x $scriptnamego} errmsg] } {
      putsDebug "\nWARNING : Unable to chmod file $scriptnamego:"
      putsDebug   "          $errmsg"
   }

   puts "Waiting for qsub to assign $infillist(numnodes) nodes"
   puts "The mormerge script is started automatically by qsub when the nodes are available"
   puts "Finished normally"
   if {$queuesys == "slurm"} {
      runcmd $sfilnamewait "merge:sbatch"
   } else {
      runcmd $sfilnamewait "merge:qsub"
   }
   vwait finished
   set finished 0
   after $waittime
   if {$queuesys == "slurm"} {
      runcmd "squeue" "merge:squeue"
   } elseif {$queuesys == "sge"} {
      runcmd "qstat" "merge:qstat"
   } else {
      runcmd "qstat -n" "merge:qstat"
   }
   vwait finished
   exit
}



# --------------------------------------------------------------------
#   Procedure: cloneInputDir
#   Author:    Adri Mourits
#   Purpose:   Create a copy of inputdir for each condition
#              and replace the condition specific files
#   Context:   In the cloned directories, the FLOW calculations take place
#   Summary:
#
#   Arguments:
#      inputdir    Input directory to be cloned
#      conditions  List of conditions, for each a clone must be created
#   Return value:
#              none
#   Note:
#
# --------------------------------------------------------------------
proc cloneInputDir { inputdir conditions } {
   global rootdir

   puts "Cloning input directory for each condition...."
   foreach condition $conditions {
      set targetdir [file nativename [file join $rootdir $condition] ]
      file delete -force $targetdir
      file mkdir $targetdir
      # Copy input directory, excluding the condition-subdirectories
      cd $inputdir
      set filelist [glob *]
      foreach item $filelist {
         if { [lsearch $conditions $item] == -1 } {
            file copy $item $targetdir
         }
      }

      # Copy condition specific files over input files
      set condinputdir [file nativename [file join $inputdir $condition] ]
      if { [file isdirectory $condinputdir] } {
         cd $condinputdir
         set filelist [glob -nocomplain *]
         foreach item $filelist {
            if { [file isdirectory $item] } {
               cd $item
               set filelistsub [glob -nocomplain *]
               foreach itemsub $filelistsub {
                  file copy -force $itemsub [file join $targetdir $item]
               }
               cd ..
            } else {
               file copy -force $item $targetdir
            }
         }
      } else {
         puts "WARNING: Directory $condinputdir does not exist"
      }

      # create file named "streamfile", containing the name
      # of the file that flow must read to obtain the stream handle
      cd $targetdir
      set streamfile [open "streamfile" "w"]
      set basename [format "%s%s" $condition "stream"]
      puts $streamfile "[file nativename [file join $rootdir "merge" $basename]]"
      close $streamfile

   }
   cd $rootdir
}



# --------------------------------------------------------------------
#   Procedure: spaceSafe
#   Author:    Adri Mourits
#   Purpose:   Transform a string containg a directory path
#              into a string that is safe to use when the path contains
#              strange characters (spaces, brackets)
#   Context:   Used when building shell scripts
#   Summary:
#
#   Arguments:
#      apath       Original path string
#   Return value:
#      The path string with added quotes, slashes, etc.
#   Note:
#      Currently only implemented for Windows
#
# --------------------------------------------------------------------
proc spaceSafe { apath } {
   global platform

   set result ""
   if {$platform == "windows"} {
      #
      # d:/this\is a/path
      # is transformed into:
      # d:\"this"\"is a"\"path"
      # This is necessary for (Windows 7):
      # - the argument of the DOS command "start"
      # - the argument of the DOS command "mkdir"
      # This is not allowed for (Windows 7):
      # - usage in the DOS command "set"
      # - usage in executable statements in DOS scripts
      # This is solved by trial and error. This did not work:
      # - Surround all paths with double-quotes
      # - Put the character ^ in front of the character ) in paths
      #
      # First: Put double quotes (") around all slashes (backwards and forwards)
      regsub -all {\\} $apath {"\\"} result
      regsub -all {/} $result {"\\"} result
      # Then: Remove the first one and add one at the end
      regsub {"} $result {} result
      # To get the syntax highlighting working in Notepad: "
      set result "$result\""
   }
   return $result
}

# --------------------------------------------------------------------
#   Procedure: startMormerge
#   Author:    Adri Mourits
#   Purpose:   Start Delft3D-MorMerge
#   Context:
#   Summary:
#
#   Arguments:
#      inputfilename
#      workdir     Directory in which each cloned inputdir is placed
#      mergeexe    Executable to be started
#      localrun
#   Return value:
#              pid
#   Note:
#
# --------------------------------------------------------------------
proc startMormerge { inputfilename workdir mergeexe localrun runid node } {
   global rootdir
   global syncdir
   global version
   global scriptscreated
   global idstring
   global waittime
   global maxWaitCount
   global queuesys
   global debug
   global DELTAQ_LocalTempDir
   global platform
   global D3D_HOME
   global remoteShell
   global clusterName
   global localhost

   cd $rootdir
   if {$platform == "linux"} {
      set scriptname "d3d-mormerge_run$runid.sh"
   } else {
      set scriptname "d3d-mormerge_run$runid.bat"
   }
   lappend scriptscreated $scriptname
   set rundir [file nativename [file join $rootdir "merge"] ]
   # Collect some directories to be added to PATH/LD_LIBRARY_PATH
   set exedir [file dirname $mergeexe]
   set screenfile [format "mormerge_%s.scr" $runid]
   set logfile [format "mormerge_%s.log" $runid]
   file delete -force [file nativename [file join $rundir $screenfile] ]
   file delete -force [file nativename [file join $rundir $logfile] ]
   set sfilname [file nativename [file join $rundir $scriptname] ]
   set scriptfile [open "$sfilname" "w"]
   if {$platform == "linux"} {
      # Get lib directory
      set libdir [file join $D3D_HOME "lib"]
      #
      puts $scriptfile "#!/bin/bash"
      if {$queuesys != "slurm"} {
         puts $scriptfile "#\$ -V"
         puts $scriptfile "#\$ -j yes"
         puts $scriptfile "#\$ -cwd"
      }
      puts $scriptfile "\n# This script file is automatically generated by mormerge.tcl, version $version\n"
      if {$queuesys == "sge"} {
         if { $clusterName != "h5" && $clusterName != "h6"} {
            puts $scriptfile "\n. /opt/sge/InitSGE\n"
         }
      }
      puts $scriptfile "\n# Set some environment parameters\n"
      puts $scriptfile "export LD_LIBRARY_PATH=$libdir:\$LD_LIBRARY_PATH"
      # Needed when compiled with newer Gnu compiler than the default on the calculation machine:
      # puts $scriptfile  "# TEMPORARY SOLUTION: setting LD_PRELOAD"
      # puts $scriptfile "export LD_PRELOAD=[file join $exedir libgfortran.so.3]"

      if { $localrun } {
         set worksubdir $idstring
         set worksubsubdir "merge_$runid"
         puts $scriptfile "\n# Running locally\n# Copy modeldir to rundir\n"
         if {$workdir == "sgestage"} {
            puts $scriptfile "\n# Jump to run directory\n"
            puts $scriptfile "cd $DELTAQ_LocalTempDir\n"
         } else {
            puts $scriptfile "cd $workdir"
            puts $scriptfile "mkdir -p $worksubdir"
            puts $scriptfile "cd $worksubdir"
         }
         puts $scriptfile "cp -fr \"[file nativename [file join $rootdir "merge"] ]\" $worksubsubdir"
         puts $scriptfile "cd $worksubsubdir"
      } else {
         puts $scriptfile "\n# Jump to run directory\n"
         puts $scriptfile "cd [file nativename [file join $rootdir "merge"] ]"
      }
      puts $scriptfile "\n# Start mormerge\n"
      puts $scriptfile "\"$mergeexe\" -i [file tail $inputfilename] -w $rundir -r $runid >$screenfile 2>&1"
      if { $localrun } {
         puts $scriptfile "\n# Copy rundir data back to modeldir\n"
         if {$workdir == "sgestage"} {
            # will be done by StageOut
         } else {
            puts $scriptfile "cp -f $screenfile $rundir"
            puts $scriptfile "cp -f $logfile $rundir"
            puts $scriptfile "cd .."
            puts $scriptfile "rm -fr $worksubsubdir"
            puts $scriptfile "cd .."
            puts $scriptfile "rmdir --ignore-fail-on-non-empty $worksubdir"
         }
      }
      puts $scriptfile "\n# End of script\n"
   } else {
      # win32
      # Get share directory
      set sharedir [string map {"bin" "share"} $exedir]
      set libdir [string map {"bin" "lib"} $exedir]
      #
      puts $scriptfile "@ echo off"
      puts $scriptfile "\nrem This script file is automatically generated by mormerge.tcl, version $version\n"
      #puts $scriptfile "\nrem Set some environment parameters\n"
      if { $localrun } {
         set worksubdir $idstring
         set worksubsubdir "merge_$runid"
         set rootmergedir "[file nativename [file join $rootdir "merge" "*"] ]"
         puts $scriptfile "\nrem Running locally\nrem Copy modeldir to rundir\n"
         puts $scriptfile "cd /D $workdir"
         puts $scriptfile "mkdir [spaceSafe $worksubdir]"
         puts $scriptfile "cd /D $worksubdir"
         puts $scriptfile "mkdir [spaceSafe $worksubsubdir]"
         puts $scriptfile "cd /D $worksubsubdir"
         puts $scriptfile "copy /Y $rootmergedir ."
      } else {
         set rootmergedir "[file nativename [file join $rootdir "merge"]]"
         puts $scriptfile "\nrem Jump to run directory\n"
         puts $scriptfile "cd /D $rootmergedir"
      }
      puts $scriptfile "\nrem Start mormerge\n"
      puts $scriptfile "set PATH=$exedir;$sharedir;$libdir;%PATH%"
      puts $scriptfile "start /b [spaceSafe $mergeexe] -i [file tail $inputfilename] -w [spaceSafe $rundir] -r $runid >$screenfile 2>&1"
      if { $localrun } {
         puts $scriptfile "\nrem Copy rundir data back to modeldir\n"
         puts $scriptfile "copy /Y $screenfile $rundir"
         puts $scriptfile "copy /Y $logfile $rundir"
         puts $scriptfile "cd .."
         puts $scriptfile "rmdir /S /Q $worksubsubdir"
         puts $scriptfile "cd .."
         puts $scriptfile "rmdir /Q $worksubdir"
      }
      puts $scriptfile "\nexit\n"
      puts $scriptfile "\nrem End of script\n"
   }
   close $scriptfile

   #
   # The usage of the following shellscript should not be necessary
   # But it solves problems when trying to run on more than one node,
   # via qsub
   #
   if {$platform == "linux"} {
      set shellscriptname "d3d-mormerge_shell$runid.sh"
   } else {
      set shellscriptname "d3d-mormerge_shell$runid.bat"
   }
   lappend scriptscreated $shellscriptname
   set shellfilname [file nativename [file join $rundir $shellscriptname] ]
   set shellscriptfile [open "$shellfilname" "w"]
   if {$queuesys == "slurm"} {
      puts $shellscriptfile "#!/bin/bash"
      puts $shellscriptfile "\n# This script file is automatically generated by mormerge.tcl, version $version\n"
      puts $shellscriptfile "\"$sfilname\" &\n"
   } elseif {$platform == "linux"} {
      puts $shellscriptfile "#!/bin/bash"
      puts $scriptfile "#\$ -V"
      puts $scriptfile "#\$ -j yes"
      puts $scriptfile "#\$ -cwd"
      puts $shellscriptfile "\n# This script file is automatically generated by mormerge.tcl, version $version\n"
      if {$localhost} {
          puts $shellscriptfile "\"$sfilname\" &\n"
      } else {
          puts $shellscriptfile "$remoteShell -n $node \"$sfilname\" &\n"
      }
   } else {
      # win32
      puts $shellscriptfile "@ echo off"
      puts $shellscriptfile "\nrem This script file is automatically generated by mormerge.tcl, version $version\n"
      puts $shellscriptfile "start /b [spaceSafe $sfilname]\n"
   }
   close $shellscriptfile
   after $waittime

   cd $rundir
   if {$platform == "linux"} {
      if { [catch {exec chmod u+x $scriptname} errmsg] } {
         putsDebug "\nWARNING : Unable to chmod file $scriptname:"
         putsDebug   "          $errmsg"
      }
      if { [catch {exec chmod u+x $shellscriptname} errmsg] } {
         putsDebug "\nWARNING : Unable to chmod file $shellscriptname:"
         putsDebug   "          $errmsg"
      }
   }

   putsDebug "mormergeruncommand:$shellscriptname"
   after $waittime
   set returnval [runcmd $shellfilname "merge:$runid"]
   #after [expr 10000 + $waittime]
   set syncfile [file nativename [file join $syncdir "merge$runid"] ]
   set fileexists 0
   set waitcount 0
   while {! $fileexists} {
      incr waitcount
      putsDebug "waiting for file $syncfile to occur ($waitcount/$maxWaitCount)"
      set fileexists [file exists $syncfile]
      after $waittime
      if { $waitcount >= $maxWaitCount } {
         puts "Maximum wait time reached for file $syncfile to occur"
         puts "Continuing..."
         set fileexists 1
      }
   }
   putsDebug "startMormerge:$runid finished"
   return $returnval
}



# --------------------------------------------------------------------
#   Procedure: startFlow
#   Author:    Adri Mourits
#   Purpose:   Start Delft3D-FLOW for a given condition
#   Context:   Run tdatom for each runid, start trisim
#   Summary:
#
#   Arguments:
#      workdir     Directory in which each cloned inputdir is placed
#      condition   Condition for which flow must be started
#      runids      Runids for which tdatom must be run
#      tdatomexe
#      flowexe     trisim.exe or delftflow.exe, including path
#      waveexe     wave.exe, including path
#      node        node on which this FLOW run must be started
#   Return value:
#              pid
#   Note:
#
# --------------------------------------------------------------------
proc startFlow { inflist alist condition runids waveonline tdatomexe flowexe waveexe dimrexe runscript node } {
   global rootdir
   global syncdir
   global version
   global scriptscreated
   global D3D_HOME
   global OS
   global idstring
   global waittime
   global maxWaitCount
   global queuesys
   global debug
   global DELTAQ_LocalTempDir
   global platform
   global remoteShell
   global clusterName
   global localhost
   upvar $inflist infillist
   upvar $alist   arglist

   cd $rootdir
   if {$platform == "linux"} {
      set scriptname "d3d-flow_run$condition.sh"
   } else {
      set scriptname "d3d-flow_run$condition.bat"
   }
   lappend scriptscreated $scriptname
   set rundir [file nativename [file join $rootdir "merge"] ]
   # Collect some directories to be added to PATH/LD_LIBRARY_PATH
   if { $infillist(dimrexedir) != " " } {
      set exedir $infillist(dimrexedir)
      set exename $infillist(dimrexename)
   } elseif { $infillist(flowexedir) != " " } {
      set exedir $infillist(flowexedir)
      set exename $infillist(flowexename)
   } else {
      set exedir "not defined"
      set exename "noet defined"
   }
   if { $waveonline } {
      set waveexedir [file dirname $waveexe]
      if {$platform == "linux"} {
         set swanexedir [file join $D3D_HOME "bin"]
      } else {
         set swanexedir [file join $D3D_HOME "swan" "bin"]
      }
      set swanbatdir $infillist(swanbatdir)
   }

   set sfilname [file nativename [file join $rundir $scriptname] ]
   set scriptfile [open "$sfilname" "w"]
   if {$platform == "linux"} {
      # Get lib directory
      set libdir [file join $D3D_HOME "lib"]
      #
      puts $scriptfile "#!/bin/bash"
      if {$queuesys != "slurm"} {
         puts $scriptfile "#\$ -V"
         puts $scriptfile "#\$ -j yes"
         puts $scriptfile "#\$ -cwd"
      }
      puts $scriptfile "\n# This script file is automatically generated by mormerge.tcl, version $version\n"
      if {$queuesys == "sge"} {
         if {$clusterName != "h5" && $clusterName != "h6"} {
            puts $scriptfile "\n. /opt/sge/InitSGE\n"
         }
      }
      # Needed when compiled with newer Gnu compiler than the default on the calculation machine:
      # puts $scriptfile "export LD_PRELOAD=[file join $exedir libgfortran.so.3]"
      # puts $scriptfile  "# TEMPORARY SOLUTION: setting LD_PRELOAD"
      # puts $scriptfile "export LD_PRELOAD=[file join $flowexedir libgfortran.so.3]"

      if { $infillist(localrun) } {
         set worksubdir $idstring
         puts $scriptfile "\n# Running locally\n# Copy modeldir to workdir\n"
         if {$infillist(workdir) == "sgestage"} {
            puts $scriptfile "\n# Jump to run directory\n"
            puts $scriptfile "cd $DELTAQ_LocalTempDir\n"
         } else {
            puts $scriptfile "cd $infillist(workdir)"
            puts $scriptfile "mkdir -p $worksubdir"
            puts $scriptfile "cd $worksubdir"
         }
         puts $scriptfile "cp -fr [file nativename [file join $rootdir $condition] ] ."
         puts $scriptfile "rm -fr [file nativename [file join $rootdir $condition] ]"
         puts $scriptfile "\n# Jump to run directory\n"
         puts $scriptfile "cd $condition"
      } else {
         puts $scriptfile "\n# Jump to run directory\n"
         puts $scriptfile "cd [file nativename [file join $rootdir $condition] ]"
      }
      if { $arglist(runtdatom) } {
         puts $scriptfile "\n# Run tdatom for each runid\n"
         foreach runid $runids {
            puts $scriptfile "\"$tdatomexe\" -r $runid >tdatom$runid.scr 2>&1"
         }
      }
      if { $infillist(dimrexedir) != " " } {
         puts $scriptfile "\n# Start $infillist(dimrexename)\n"
         puts $scriptfile "\"$dimrexe\" $infillist(dimrargs) >$infillist(dimrexename).scr 2>&1 &"
      } elseif { $infillist(runscript) != " " } {
         puts $scriptfile "\n# Start $infillist(runscript) $infillist(runscriptargs)\n"
         puts $scriptfile "\"$infillist(runscript)\" $infillist(runscriptargs) >runscript.scr 2>&1 &"
      } else {
         puts $scriptfile "\n# Start $infillist(flowexename)\n"
         puts $scriptfile "export LD_LIBRARY_PATH=$libdir:\$LD_LIBRARY_PATH"
         puts $scriptfile "\"$flowexe\" $infillist(flowargs) >$infillist(flowexename).scr 2>&1 &"
         if { $waveonline } {
            puts $scriptfile "\n# Start wave\n"
            puts $scriptfile "export PATH=$swanbatdir:\$PATH"
            puts $scriptfile "\"$waveexe\" $infillist(waveargs) >wave.scr 2>&1 &"
         }
      }
      if { $infillist(localrun) } {
         puts $scriptfile "\n# Copy rundir data back to modeldir\n"
         puts $scriptfile "cd .."
         if {$infillist(workdir) == "sgestage"} {
            # will be done by StageOut
         } else {
            puts $scriptfile "cp -fr \"$condition\" \"$rootdir\""
            puts $scriptfile "rm -fr \"$condition\""
            puts $scriptfile "cd .."
            puts $scriptfile "rmdir --ignore-fail-on-non-empty \"$worksubdir\""
         }
      }
      puts $scriptfile "\n    # Wait until all child processes are finished \n"
      puts $scriptfile "wait\n"
      puts $scriptfile "\n# End of script\n"
   } else {
      # win64
      # Get share directory
      set sharedir [string map {"bin" "share"} $exedir]
      set libdir [string map {"bin" "lib"} $exedir]
      #
      set rootconddir [file nativename [file join $rootdir $condition] ]
      puts $scriptfile "@ echo off"
      puts $scriptfile "\nrem This script file is automatically generated by mormerge.tcl, version $version\n"
      puts $scriptfile "\nrem Set some environment parameters\n"
      if { $infillist(localrun) } {
         set worksubdir $idstring
         puts $scriptfile "\nrem Running locally\nrem Copy modeldir to workdir\n"
         puts $scriptfile "cd /D $infillist(workdir)"
         puts $scriptfile "mkdir [spaceSafe $worksubdir]"
         puts $scriptfile "cd /D $worksubdir"
         puts $scriptfile "mkdir [spaceSafe $condition]"
         puts $scriptfile "cd /D $condition"
         puts $scriptfile "copy /Y [file nativename [file join $rootconddir "*"] ] ."
         puts $scriptfile "rmdir /S /Q $rootconddir"
      } else {
         puts $scriptfile "\nrem Jump to run directory\n"
         puts $scriptfile "cd /D $rootconddir"
      }
      if { $arglist(runtdatom) } {
         puts $scriptfile "\nrem Run tdatom for each runid\n"
         foreach runid $runids {
            puts $scriptfile "start /b [spaceSafe $tdatomexe] -r $runid >tdatom$runid.scr 2>&1"
         }
      }
      if { $infillist(dimrexedir) != " " } {
         puts $scriptfile "\nrem Start $infillist(dimrexename)\n"
         puts $scriptfile "\"$dimrexe\" --forceExit $infillist(dimrargs) >$infillist(dimrexename).scr 2>&1"
      } else {
         puts $scriptfile "\nrem Start $infillist(flowexename)\n"
         puts $scriptfile "set PATH=$exedir;$sharedir;$libdir;%PATH%"
         puts $scriptfile "start /b [spaceSafe $flowexe] $infillist(flowargs) >$infillist(flowexename).scr 2>&1"
         if { $waveonline } {
            puts $scriptfile "\nrem Start wave\n"
            puts $scriptfile "set PATH=$swanbatdir;$swanexedir;$waveexedir;$sharedir;%PATH%"
            puts $scriptfile "start /b [spaceSafe $waveexe] $infillist(waveargs) >wave.scr 2>&1"
         }
      }
      if { $infillist(localrun) } {
         puts $scriptfile "\nrem Copy rundir data back to modeldir\n"
         puts $scriptfile "mkdir [spaceSafe $rootconddir]"
         puts $scriptfile "copy /Y * $rootconddir"
         puts $scriptfile "cd .."
         puts $scriptfile "rmdir /S /Q $condition"
         puts $scriptfile "cd .."
         puts $scriptfile "rmdir /Q $worksubdir"
      }
      puts $scriptfile "\nexit\n"
      puts $scriptfile "\nrem End of script\n"
   }
   close $scriptfile

   #
   # The usage of the following shellscript should not be necessary
   # But it solves problems when trying to run on more than one node,
   # via qsub
   #
   if {$platform == "linux"} {
      set shellscriptname "d3d-flow_shell$condition.sh"
   } else {
      set shellscriptname "d3d-flow_shell$condition.bat"
   }
   lappend scriptscreated $shellscriptname
   set shellfilname [file nativename [file join $rundir $shellscriptname] ]
   set shellscriptfile [open "$shellfilname" "w"]
   if {$queuesys == "slurm"} {
      puts $shellscriptfile "#!/bin/bash"
      puts $shellscriptfile "\n# This script file is automatically generated by mormerge.tcl, version $version\n"
      puts $shellscriptfile "\"$sfilname\" &\n"
   } elseif {$platform == "linux"} {
      puts $shellscriptfile "#!/bin/bash"
      puts $shellscriptfile "#\$ -V"
      puts $shellscriptfile "#\$ -j yes"
      puts $shellscriptfile "#\$ -cwd"
      puts $shellscriptfile "\n# This script file is automatically generated by mormerge.tcl, version $version\n"
      if {$localhost} {
          puts $shellscriptfile "\"$sfilname\" &\n"
      } else {
          puts $shellscriptfile "$remoteShell -n $node \"$sfilname\" &\n"
      }
   } else {
      # win32
      puts $shellscriptfile "@ echo off"
      puts $shellscriptfile "\nrem This script file is automatically generated by mormerge.tcl, version $version\n"
      puts $shellscriptfile "start /b [spaceSafe $sfilname]\n"
      puts $shellscriptfile "\nexit\n"
   }
   close $shellscriptfile
   after $waittime

   cd $rundir
   if {$platform == "linux"} {
      if { [catch {exec chmod u+x $scriptname} errmsg] } {
         putsDebug "\nWARNING : Unable to chmod file $scriptname:"
         putsDebug   "          $errmsg"
      }
      if { [catch {exec chmod u+x $shellscriptname} errmsg] } {
         putsDebug "\nWARNING : Unable to chmod file $scriptname:"
         putsDebug   "          $errmsg"
      }
      after $waittime
   }

   putsDebug "Flow Run command:$shellscriptname"
   set returnval [runcmd $shellfilname "flow:$condition"]
   #after [expr 10000 + $waittime]
   set fileexists 0
   set waitcount 0
   while {! $fileexists} {
      incr waitcount
      set fileexists 1
      foreach runid $runids {
         set basename [format "%s%s%s" $condition "flow" $runid]
         set syncfile [file nativename [file join $syncdir $basename] ]
         if {! [file exists $syncfile] } {
            putsDebug "waiting for file $syncfile to occur ($waitcount/$maxWaitCount)"
            set fileexists 0
         }
      }
      after $waittime
      if { $waitcount >= $maxWaitCount } {
         puts "Maximum wait time reached for file $syncfile to occur"
         puts "Continuing..."
         set fileexists 1
      }
   }
   putsDebug "startFlow:$condition finished"
   return $returnval
}



# --------------------------------------------------------------------
#   Procedure: removeCreatedScripts
#   Author:    Adri Mourits
#   Purpose:   -
#   Context:   -
#   Summary:
#
#   Arguments:
#      None
#   Return value:
#      None
#   Note:
#
# --------------------------------------------------------------------
proc removeCreatedScripts { mergedir } {
   global scriptscreated

   set scriptnamewait "d3d-mormerge_qsubwait.sh"
   set scriptnamego   "d3d-mormerge_qsubgo.sh"
   lappend scriptscreated $scriptnamewait
   lappend scriptscreated $scriptnamego

   cd $mergedir
   foreach script $scriptscreated {
      catch {[file delete -force $script]}
   }
}



# --------------------------------------------------------------------
#   MAIN CODE
#   Author:    Adri Mourits
#   Purpose:   Read all information and start the executables
#   Context:   --
# --------------------------------------------------------------------
global env
global argv
global platform
global clusterName
global rootdir
global syncdir
global processes
global processhandles
global datetime
global processStarttime
global jobid
global idstring
global queuesys
global DELTAQ_LocalTempDir

putsDebug "Waiting 1 second before doing anything"
after 1000

setPlatform

if {$platform == "linux"} {
   if { $queuesys == "slurm" } {
      if { [info exists env(SLURM_JOB_ID)] } {
         catch {set jobid $env(SLURM_JOB_ID)}
      }
   } elseif { $queuesys == "sge" } {
      if { [info exists env(DELTAQ_JobID)] } {
         catch {set jobid $env(DELTAQ_JobID)}
      }
   } else {
      if { [info exists env(PBS_JOBID)] } {
         catch {set jobid $env(PBS_JOBID)}
      }
   }
   if {$clusterName == "h5" || $clusterName == "h6"} {
      if { [info exists env(JOB_ID)] } {
         catch {set jobid $env(JOB_ID)}
      }
   }
} else {
   set queuesys "none"
}
set idstring [format "%s_%s" $jobid $datetime]



#
#
#
# Arguments
#
array set arglist {}
readArguments $argv arglist
putsDebug "mormerge.tcl start"
putsDebug "arguments             : $argv"
putsDebug "identification string : $idstring"
putsDebug "inputfile             : $arglist(infile)"
putsDebug "mmscriptfile          : $arglist(mmscriptfile)"
putsDebug "availablenodes        : $arglist(availablenodes)"
putsDebug "run in testbank       : $arglist(testbank)"
putsDebug "run tdatom            : $arglist(runtdatom)"


#
#
#
# Define some directories
# Do not use env(PBS_O_WORKDIR) when running in the testbank
#
set startdir [file nativename [pwd] ]
set rootdir $startdir
if { $arglist(testbank)==0 && $platform=="linux"} {
   if { $queuesys == "slurm" } {
      if { [info exists env(SLURM_SUBMIT_DIR)] } {
         catch {set rootdir $env(SLURM_SUBMIT_DIR)}
      }
   } elseif { $queuesys == "sge" } {
      if { [info exists env(DELTAQ_JobDir)] } {
         catch {set rootdir $env(DELTAQ_JobDir)}
      }
   } else {
      if { [info exists env(PBS_O_WORKDIR)] } {
         catch {set rootdir $env(PBS_O_WORKDIR)}
      }
   }
   if { $clusterName == "h5" || $clusterName == "h6"} {
      if { [info exists env(SGE_O_WORKDIR)] } {
         catch {set rootdir $env(SGE_O_WORKDIR)}
      }
   }
}
if {[file tail $rootdir] == "merge"} {
   set rootdir [file dirname $rootdir]
}

cd $rootdir
# reset rootdir, to use pwd notation
set rootdir   [file nativename [pwd] ]
set inputdir  [file nativename [file join $rootdir "input"] ]
set mergedir  [file nativename [file join $rootdir "merge"] ]
set syncdir   [file nativename [file join $rootdir "merge" "sync"] ]
checkFil dir $inputdir
checkFil dir $mergedir

putsDebug "startdir          : $startdir"
putsDebug "rootdir           : $rootdir"



#
#
# reset inputfilename directory, to use pwd notation
#
set arglist(infile) [file tail $arglist(infile)]
cd $mergedir
if { [file exists $arglist(infile)] } {
   set arglist(infile) [file nativename [file join [pwd] $arglist(infile)] ]
} else {
   puts "ERROR: Expecting file $arglist(infile) in directory $mergedir"
   puts "       Check command line arguments"
   exit 1
}



#
#
#
# Input file
#
array set infillist {}
readInputFile $arglist(infile) infillist
putsDebug "dimrexedir        : $infillist(dimrexedir)"
putsDebug "dimrexename       : $infillist(dimrexename)"
putsDebug "dimrargs          : $infillist(dimrargs)"
putsDebug "flowexedir        : $infillist(flowexedir)"
putsDebug "flowexename       : $infillist(flowexename)"
putsDebug "flowargs          : $infillist(flowargs)"
putsDebug "waveexedir        : $infillist(waveexedir)"
putsDebug "waveargs          : $infillist(waveargs)"
putsDebug "swanbatdir        : $infillist(swanbatdir)"
putsDebug "runscript         : $infillist(runscript)"
putsDebug "runscriptargs     : $infillist(runscriptargs)"
putsDebug "mormergeexedir    : $infillist(mormergeexedir)"
putsDebug "$infillist(numconditions) conditions:"
for {set i 0} {$i < $infillist(numconditions)} {incr i} {
   putsDebug "   name : [lindex $infillist(conditions) $i] weight : [lindex $infillist(weights) $i]"
}
putsDebug "total weight      : $infillist(totalweight)"
putsDebug "workdir           : $infillist(workdir)"
if { $infillist(localrun) } {
   puts "Running locally in directory $infillist(workdir)"
}
putsDebug "numnodes          : $infillist(numnodes)"

# No separate tdatom when using delftflow/deltares_hydro executable
if { $infillist(flowexename) in {delftflow deltares_hydro d_hydro} } {
   set arglist(runtdatom) 0
}


#
#
#
# Runids, wave OnLine
#
if { $infillist(runscript) != " " } {
   putsDebug "Runid set to singledomain for runscript ..."
   getRunids $infillist(flowargs) $infillist(dimrargs) $infillist(runscript) $inputdir numdomains runids
} elseif { $infillist(dimrexedir) != " " } {
   putsDebug "Runid set to singledomain for dimr ..."
   getRunids $infillist(flowargs) $infillist(dimrargs) $infillist(runscript) $inputdir numdomains runids
} elseif { $infillist(flowexename) == "trisim" } {
   putsDebug "Scanning commandline arguments for trisim ..."
   getRunids $infillist(flowargs) $infillist(dimrargs)  $infillist(runscript) $inputdir numdomains runids
} elseif { $infillist(flowexename) == "delftflow" } {
   set d3dfilnam [file nativename [file join $rootdir "input" [lindex $infillist(flowargs) 0] ] ]
   putsDebug "Scanning input file $d3dfilnam ..."
   set d3dinfile [open "$d3dfilnam" "r"]
   while {[gets $d3dinfile aline] >= 0} {
      if { [string length $aline] > 3 } break
   }
   close $d3dinfile
   getRunids $aline $infillist(dimrargs) $infillist(runscript) $inputdir numdomains runids
} elseif { $infillist(flowexename) == "deltares_hydro" || $infillist(flowexename) == "d_hydro" } {
   set d3dfilnam [file nativename [file join $rootdir "input" [lindex $infillist(flowargs) 0] ] ]
   putsDebug "Scanning input file $d3dfilnam ..."
   set d3dinfile [open "$d3dfilnam" "r"]
   while {[gets $d3dinfile aline] >= 0} {
      if { [string match -nocase "*mdffile*" $aline] || [string match -nocase "*ddbfile*" $aline] } break
   }
   close $d3dinfile
   getRunids $aline $infillist(dimrargs) $infillist(runscript) $inputdir numdomains runids
}
putsDebug "$numdomains domain(s):"
foreach runid $runids {
   putsDebug "   runid : $runid"
}

# Unless a run script is given, check for Delft3D-WAVE
if { $infillist(runscript) eq " " } {
    set waveonline [checkWaveOnline $inputdir $arglist(infile) $runids infillist]
} else {
    set waveonline 0
}

#
#
#
# qsub
#
set nodelist {}
if { $queuesys != "none" } {
   #
   # qsub/sbatch is used to get the nodes
   #
   if { $arglist(availablenodes) == 0 } {
      waitForNodes $mergedir arglist infillist
   }
   if {$queuesys == "slurm"} {
      set nodelist $env(SLURM_JOB_NODELIST)
   } elseif {$queuesys == "sge"} {
      if { $clusterName == "h5" || $clusterName == "h6"} {
         set nodefilename $env(PE_HOSTFILE)
         putsDebug "nodefilename      : $nodefilename"
         readNodeFile $nodefilename infillist(numnodes) nodelist
      } else {
         set nodelist [split $env(DELTAQ_NodeList)]
         if { [llength $nodelist] != $infillist(numnodes) } {
            puts "ERROR: Number of nodes specified in input file: $infillist(numnodes)"
            puts "       Number of nodes specified in \$DELTAQ_NodeList  : [llength $nodelist]"
            exit 1
         }
         if { [info exists env(SGE_LocalTempDir)] } {
            set DELTAQ_LocalTempDir $env(SGE_LocalTempDir)
            putsDebug "SGE workdir (via env(SGE_LocalTempDir):$DELTAQ_LocalTempDir"
         } else {
            puts "ERROR: Unable to obtain the SGE working directory via \$DELTAQ_LocalTempDir"
            exit 1
         }
      }
   } else {
      set nodefilename $env(PBS_NODEFILE)
      putsDebug "nodefilename      : $nodefilename"
      readNodeFile $nodefilename infillist(numnodes) nodelist
   }
} else {
   if {$platform == "linux"} {
      #
      # local run
      #
      # nodelist still has to be filled
      # First try env(PBS_NODEFILE); this works in the testbank
      # Else use env(HOST); replace "x123.deltares.nl" by "x123"
      # Else use "local"
      #
      if { [info exists env(PBS_NODEFILE)] } {
         set nodefilename $env(PBS_NODEFILE)
         readNodeFile $nodefilename infillist(numnodes) nodelist
      } else {
         if { [info exists env(HOSTNAME)] } {
            set host $env(HOSTNAME)
            set dotpos [string first "." $host]
            if { $dotpos > -1 } {
               set host [string range $host 0 [expr $dotpos - 1]]
            }
            set nodelist $host
         } else {
            set nodelist "local"
         }
      }
   } else {
      set nodelist "local"
   }
}
putsDebug "number of nodes   : $infillist(numnodes)"
foreach anode $nodelist {
   putsDebug "   nodename : $anode"
}


#
#
#
# Executables
#
set flowexe " "
set dimrexe " "
set runscript " "

if {$platform == "linux"} {
   set mergeexe     [file nativename [file join $infillist(mormergeexedir) "mormerge"] ]
   set tdatomexe    [file nativename [file join $infillist(flowexedir) "tdatom"] ]
   if { $infillist(dimrexedir) != " " } {
      set dimrexe      [file nativename [file join $infillist(dimrexedir) "run_dimr.sh"] ]
   } elseif { $infillist(flowexedir) != " " } {
      if { $infillist(flowexename) in $flowNames } {
          set flowexe   [file nativename [file join $infillist(flowexedir) $infillist(flowexename)] ]
      }
   }
} else {
   set mergeexe     [file nativename [file join $infillist(mormergeexedir) "mormerge.exe"] ]
   set tdatomexe    [file nativename [file join $infillist(flowexedir) "tdatom.exe"] ]
   if { $infillist(dimrexedir) != " " } {
      set dimrexe      [file nativename [file join $infillist(dimrexedir) "run_dimr.bat"] ]
   } elseif { $infillist(flowexedir) != " " } {
      if { $infillist(flowexename) in $flowNames } {
          set flowexe   [file nativename [file join $infillist(flowexedir) $infillist(flowexename).exe] ]
      }
   }
}
if { $infillist(runscript) ne " " } {
   set runscript $infillist(runscript)
}

checkFil exe $mergeexe
if { $arglist(runtdatom) } {
   checkFil exe $tdatomexe
}
if { $dimrexe != " " } {
   checkFil exe $dimrexe
}
if { $flowexe != " " } {
   checkFil exe $flowexe
}
if { $runscript != " " } {
   checkFil exe $runscript
}

if { $waveonline } {
   if {$platform == "linux"} {
      set waveexe   [file nativename [file join $infillist(waveexedir) "wave"] ]
   } else {
      set waveexe   [file nativename [file join $infillist(waveexedir) "wave.exe"] ]
   }
   checkFil exe $waveexe
   if {$platform == "windows"} {
      set swanbat   [file nativename [file join $infillist(swanbatdir) "swan.bat"] ]
      checkFil exe $swanbat
   } else {
      set swanbat   [file nativename [file join $infillist(swanbatdir) "swan.sh"] ]
      if { [file exists $swanbat] == 0 } {
         puts "WARNING: File $swanbat not found. Trying with swan.bat"
         set swanbat   [file nativename [file join $infillist(swanbatdir) "swan.bat"] ]
         checkFil exe $swanbat
      }
   }
} else {
   set waveexe " "
}



#
#
#
# Clone input directory
#
cloneInputDir $inputdir $infillist(conditions)



#
#
#
# Initialize synchronisation
#
catch {file mkdir $syncdir}
cd $syncdir
set filelist [glob -nocomplain *]
foreach item $filelist {
   if { [file isfile $item] } {
      if { [catch {file delete $item} errmsg] } {
         putsDebug "\nWARNING : Unable to delete file [file nativename [file join $syncdir $item] ]:"
         putsDebug   "          $errmsg"
      }
   }
}



#
#
#
# Start processes
#
array set pidlist {}

set inode -1
foreach runid $runids {
   putsDebug "Starting merge for runid $runid after [expr $processStarttime / 1000] seconds ..."
   after $processStarttime
   incr inode
   set inode [expr round(fmod($inode,$infillist(numnodes)))]
   set node [lindex $nodelist $inode]
   set pidlist($runid) [startMormerge $arglist(infile)     \
                                      $infillist(workdir)  \
                                      $mergeexe            \
                                      $infillist(localrun) \
                                      $runid               \
                                      $node]
   puts "merge:$runid started, pid: $pidlist($runid)"
}

after 1000

set inode -1
foreach condition $infillist(conditions) {
   putsDebug "Starting flow for condition $condition after [expr $processStarttime / 1000] seconds ..."
   after $processStarttime
   incr inode
   set inode [expr round(fmod($inode,$infillist(numnodes)))]
   set node [lindex $nodelist $inode]
   set pidlist($condition) [startFlow infillist            \
                                      arglist              \
                                      $condition           \
                                      $runids              \
                                      $waveonline          \
                                      $tdatomexe           \
                                      $flowexe             \
                                      $waveexe             \
                                      $dimrexe             \
                                      $runscript           \
                                      $node]
   puts "flow:$condition started, pid: $pidlist($condition)"
}



#
#
#
# Wait for termination
#
puts "Waiting for [llength $processes] processes to finish..."

vwait finished


#
#
#
# Clean up
#
if { ! $debug } {
   removeCreatedScripts $mergedir
}

puts "mormerge.tcl : Finished"
cd $startdir

