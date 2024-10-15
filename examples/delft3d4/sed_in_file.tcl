#! /usr/bin/tclsh

global argv

set filename [lindex $argv 0]
set searchstring  [lindex $argv 1]
set replacestring  [lindex $argv 2]

set infile [open $filename]
set wholefile [read $infile]
close $infile
set newfile {}
foreach line [split $wholefile "\n"] {
   regsub -all -- "$searchstring" $line "$replacestring" line
   lappend newfile $line
}
set outfile [open $filename "w"]
puts -nonewline $outfile [join $newfile "\n"] 
close $outfile

