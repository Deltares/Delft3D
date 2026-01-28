#! /usr/bin/tclsh

# DOC
#
# kill.tcl - Script to kill running processes, identified with mask strings
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

global killids
set killids {}

global masks
set masks {}

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
    global masks

    if { [gets $channel line] >= 0 } {
       if { $tag == "ps" } {
          # The space character is going to be used for splitting
          # Replace all sequences of spaces by one single space
          regsub -all {[ ]+} [string trim $line] " " line
          #puts "$aline:"
          set words [split $line]
          foreach amask $masks {
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
        #puts "$tag : Finished"
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

if { [llength $argv] == 0} {
   puts "Usage:\nkill.tcl <masks>"
   puts "   <masks> : space separated masks to identify process names to kill"
   exit 1
}

if { [lindex $argv 0] == "-u" } {
   set user [lindex $argv 1]
   set masks [lrange $argv 2 end]
} else {
   set masks [lrange $argv 0 end]
   puts "No username specified; trying to obtain env(LOGNAME)"
   if { [catch {set user $env(LOGNAME)} errmsg] } {
       puts "\nUnable to get environment parameter LOGNAME :\n $errmsg"
       exit 1
   }
}
puts "User: $user"

puts "Killing all processes, containing one of the following strings in their name:"
foreach amask $masks {
   puts "   $amask"
}

runcmd "ps -u $user" "ps"

vwait finished

if { [llength $killids] == 0} {
   puts "Nothing to kill"
   exit 0
}
set killids [lsort -unique $killids]
foreach anid $killids {
   puts "Trying to kill: $anid"
   runcmd "kill -9 $anid" "kill"
}



puts "kill.tcl : Finished"


