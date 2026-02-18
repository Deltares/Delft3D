#! /usr/bin/tclsh

# DOC
#
# cleantemp_d3dflow.tcl - Script to clean temp directories
# belonging to Delft3D-FLOW on specified hosts
#
# Copyright (C)  Stichting Deltares, 2011-2026.
#
# Authors: Adri Mourits
#
# General information:
#
#
# ENDDOC
#
#  $Author$
#  $Date$
#  $Source$
#

global processes
set processes {}

global processhandles
set processhandles {}

global hosts
set hosts {}

global exes
set exes {}


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

    set infile [open "|$command" "r"]

    #fconfigure $infile -buffering none
    fconfigure $infile -buffering line
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
    global killids
    global argv

    if { [gets $channel line] >= 0 } {
       if { $tag == "ps" } {
          # The space character is going to be used for splitting
          # Replace all sequences of spaces by one single space
          regsub -all {[ ]+} [string trim $line] " " line
          #puts "$aline:"
          set words [split $line]
          foreach amask $argv {
             if {[string first $amask [lindex $words 3]] >= 0 } {
                lappend killids [lindex $words 0]
             }
          }
       } else {
          puts "$tag : $line"
       }
    } else {
        set idx [lsearch $processes [pid $channel]]
        set processes [lreplace $processes $idx $idx]
        puts "$tag : Finished"
        if { [catch {close $channel} errmsg] } {
            puts "\n$tag : $errmsg"
        }
        if { [llength $processes] == 0 } {
            set ::finished 1
        } else {
            puts "Waiting for [llength $processes] processes to finish..."
        }
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

set dirs {"/temp/run-d3d-mormerge*"}

set command "rm -r $dirs"

if { [llength $argv] == 0} {
   puts "Usage:\nkill_d3dflow.tcl <hosts>"
   puts "   <hosts> : space separated list of machines"
   puts "             on which the following directories will be removed:"
   for each adir $dirs {
   puts "                $adir"
   }
   puts "             use \"local\" for local host"
   exit 1
}
puts "Cleaning the temp directories on the following machines:"
foreach arg $argv {
   puts "   $arg"
}
foreach arg $argv {
   if {$arg == "local"} {
      runcmd "$command" "$arg"
   } else {
      runcmd "rsh -n $arg $command" "$arg"
   }
}

vwait finished

puts "cleantemp_d3dflow.tcl : Finished"


