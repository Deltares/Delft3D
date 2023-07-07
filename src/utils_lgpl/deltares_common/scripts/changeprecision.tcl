#! /usr/bin/tclsh

if {[llength $argv] != 1} {
   puts "ERROR: wrong arguments."
   puts "usage:"
   puts "changeprecion.tcl option"
   puts "   option: single"
   puts "           double"
   exit
} else {
   if {![string equal [lindex $argv 0] "single"] && ![string equal [lindex $argv 0] "double"]} {
      puts "ERROR: argument must be 'single' or 'double' (is [lindex $argv 0])"
      exit
   }
   if {[string equal [lindex $argv 0] "single"]} {
      set mode "single"
      puts "This script changes Delft3D-FLOW code to produce SINGLE PRECISION binaries."
   } else {
      set mode "double"
      puts "This script changes Delft3D-FLOW code to produce DOUBLE PRECISION binaries."
   }
}

set curdir [pwd]

set scriptpos [string first "scripts" $curdir]
if { $scriptpos >= 0 } {
   set rootdir [string range $curdir 0 [expr $scriptpos - 2]]
} else {
   set rootdir $curdir
}
puts "rootdir:$rootdir"

set files {}
lappend files [file join $rootdir "packages" "deltares_common" "src" "precision.f90"]
lappend files [file join $rootdir "packages" "deltares_common" "include" "tri-dyn.igd"]
lappend files [file join $rootdir "packages" "deltares_common_c" "include" "precision.h"]
lappend files [file join $rootdir ".." ".." "engines_gpl" "flow2d3d" "packages" "flow2d3d" "flow2d3d.vcxproj"]
# parse flow2d3d/Makefile.am twice for two strings to be replaced
lappend files [file join $rootdir ".." ".." "engines_gpl" "flow2d3d" "packages" "flow2d3d" "src" "Makefile.am"]
lappend files [file join $rootdir ".." ".." "engines_gpl" "flow2d3d" "packages" "flow2d3d" "src" "Makefile.am"]
lappend files [file join $rootdir ".." ".." "engines_gpl" "flow2d3d" "packages" "flow2d3d" "CMakeLists.txt"]
lappend files [file join $rootdir ".." ".." "cmake" "install_flow2d3d" "install_and_bundle.cmake"]

# file types:
# f: fortran
# c: C/C++
# o: other
set filetypes {}
lappend filetypes "f"
lappend filetypes "f"
lappend filetypes "c"
# flow2d3d vcproj/vcxproj:
lappend filetypes "o"
# flow2d3d/Makefile.am:
lappend filetypes "o"
lappend filetypes "o"
# CMakeLists.txt:
lappend filetypes "o"
# install_and_bundle.cmake:
lappend filetypes "o"


set hplines {}
lappend hplines "integer, parameter :: fp=hp"
lappend hplines "equivalence ( r(0),  dbuf(0))"
lappend hplines "#define FLOW_DOUBLE_PRECISION"
# flow2d3d vcproj/vcxproj:
lappend hplines "flow2d3d.dll"
# flow2d3d/Makefile.am:
lappend hplines "libflow2d3d.la"
lappend hplines "libflow2d3d_la"
# CMakeLists.txt:
lappend hplines "library_name flow2d3d "
# install_and_bundle.cmake:
lappend hplines "libflow2d3d.so"



set splines {}
lappend splines "integer, parameter :: fp=sp"
lappend splines "equivalence ( r(0),  rbuf(0))"
lappend splines "#undef FLOW_DOUBLE_PRECISION"
# flow2d3d vcproj/vcxproj:
lappend splines "flow2d3d_sp.dll"
# flow2d3d/Makefile.am:
lappend splines "libflow2d3d_sp.la"
lappend splines "libflow2d3d_sp_la"
# CMakeLists.txt (WARNING: the space at the end is crucial):
lappend splines "library_name flow2d3d_sp "
# install_and_bundle.cmake:
lappend splines "libflow2d3d_sp.so"



puts "The following files are going to be changed:"
foreach f $files {
   puts "   $f"
}
puts " "
puts "Waiting 5 seconds before continuing"
after 5000

foreach f $files {
   if {! [file exists $f]} {
      puts "ERROR: file $f does not exist"
      exit
  }
}

for {set i 0} {$i<[llength $files]} {incr i} {
   puts "Processing file [lindex $files $i] ..."
   if {[lindex $filetypes $i] == "f"} {
      set comchar "!"
   } else {
      set comchar "//"
   }
   set infile [open [lindex $files $i]]
   set wholefile [read $infile]
   close $infile
   set newfile {}
   foreach line [split $wholefile "\n"] {
      set hpindex [string first [lindex $hplines $i] $line]
      set spindex [string first [lindex $splines $i] $line]
      if { $hpindex>=0 && $hpindex<=2 && [lindex $filetypes $i]!="o" } {
         # hp line found
         if { $hpindex == 0 } {
            # hp line is activated
            if { $mode == "single" } {
               puts "   hp line deactivated."
               lappend newfile "$comchar$line"
            } else {
               puts "   hp line already activated; not changed."
               lappend newfile $line
            }
         } elseif { [string first $comchar $line] == 0 } {
            # hp line is deactivated
            if { $mode == "single" } {
               puts "   hp line already commented out; not changed."
               lappend newfile $line
            } else {
               puts "   hp line activated."
               lappend newfile [string range $line $hpindex end]
            }
         } else {
            # unknown character in front of the line
            if { $mode == "single" } {
               puts "   hp line deactivated."
               lappend newfile "$comchar$line"
            } else {
               puts "   hp line activated."
               lappend newfile [string range $line $hpindex end]
            }
         }
      } elseif { $hpindex >= 0 } {
         # hp line found inside a line
         if {[lindex $filetypes $i] == "o"} {
            if { $mode == "single" } {
               puts "   hp word deactivated."
               regsub -all [lindex $hplines $i] $line [lindex $splines $i] line 
               lappend newfile $line
            } else {
               puts "   hp word already activated; not changed."
               lappend newfile $line
            }
         }
      } elseif { $spindex>=0 && $spindex<=2 && [lindex $filetypes $i]!="o" } {
         # sp line found
         if { $spindex == 0} {
            # sp line is activated
            if { $mode == "double" } {
               puts "   sp line deactivated."
               lappend newfile "$comchar$line"
            } else {
               puts "   sp line already activated; not changed."
               lappend newfile $line
            }
         } elseif { [string first $comchar $line] == 0 } {
            # sp line is deactivated
            if { $mode == "double" } {
               puts "   sp line already commented out; not changed."
               lappend newfile $line
            } else {
               puts "   sp line activated."
               lappend newfile [string range $line $spindex end]
            }
         } else {
            # unknown character in front of the line
            if { $mode == "double" } {
               puts "   sp line deactivated."
               lappend newfile "$comchar$line"
            } else {
               puts "   sp line activated."
               lappend newfile [string range $line $spindex end]
            }
         }
      } elseif { $spindex >= 0 } {
         # sp line found inside a line
         if {[lindex $filetypes $i] == "o"} {
            if { $mode == "double" } {
               puts "   sp word deactivated."
               regsub -all [lindex $splines $i] $line [lindex $hplines $i] line
               lappend newfile $line
            } else {
               puts "   sp word already activated; not changed."
               lappend newfile $line
            }
         }
      } else {
         # all other lines
         lappend newfile $line
      }
   }
   set outfile [open [lindex $files $i] "w"]
   puts -nonewline $outfile [join $newfile "\n"] 
   close $outfile
}



puts "... Finished"
