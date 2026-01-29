subroutine soaux (juresi, jufrou, juresd, justrd, jusold )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOAUX (SObek AUXilliary output)
!
! Module description: Routine writes the headers of the output
!                     files residu, froude, dumpres, dumpstr and
!                     dumpsol. These files can be used to investigate
!                     extraordinairy situation like supercritical flow
!                     and crashes of the simulation.
!
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 juresi            O  Unit number of file residu
!  2 jufrou            O  Unit number of file froude
!  3 juresd            O  Unit number of file dumpres
!  4 justrd            O  Unit number of file dumpstr
!  5 jusold            O  Unit number of file dumpsol
!-----------------------------------------------------------------------
!
!
!
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: soaux.pf,v $
! Revision 1.7  1998/02/13  13:23:44  kuipe_j
! Adapt to CMT
!
! Revision 1.6  1997/01/23  08:30:04  kuipe_j
! Make flow module robust
!
! Revision 1.5  1996/04/12  13:05:53  kuipe_j
! headers, minor changes
!
! Revision 1.4  1996/01/17  14:47:30  kuipe_j
! header update
!
! Revision 1.3  1995/11/21  11:09:05  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.2  1995/10/18  10:51:28  hoeks_a
! Some small changes
!
! Revision 1.1  1995/10/18  09:00:53  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
!***********************************************************************
!
   include '..\include\filsim.i'
!
   integer    juresi, jufrou, juresd, justrd, jusold
!
   juresi = 96
   jufrou = 97
   juresd = 98
   justrd = 99
   jusold = 95
!
!     Open file with residues and write header
!
   open (juresi,file=fresid)
   write(juresi,100)&
   &'Residues in case of continuing with no convergence'
   write(juresi,100)&
   &'Each record contains:'
   write(juresi,100)&
   &' - Time step number'
   write(juresi,100)&
   &' - Maximum waterlevel residue with corresponding position'
   write(juresi,100)&
   &' - Maximum discharge residue with corresponding position'
   write(juresi,100)&
   &' - Waterlevel convergence error'
   write(juresi,100)&
   &'   (if waterlevel criterion specified)'
   write(juresi,100)&
   &' - Waterlevel convergence error factor'
   write(juresi,100)&
   &'   (if waterlevel criterion specified)'
   write(juresi,100)&
   &' - Discharge convergence error'
   write(juresi,100)&
   &'   (if any discharge criterion specified)'
   write(juresi,100)&
   &' - Discharge convergence error factor'
   write(juresi,100)&
   &'   (if any discharge criterion specified)'
   write(juresi,100) ' '
!
!     Open file with Froude numbers and write header
!
   open (jufrou,file=ffroud)
   write(jufrou,100)&
   &'Froude numbers higher than 0.8. Each record contains:'
   write(jufrou,100)&
   &' - Time step number'
   write(jufrou,100)&
   &' - Iteration step number'
   write(jufrou,100)&
   &' - Branch name'
   write(jufrou,100)&
   &' - Coordinate'
   write(jufrou,100)&
   &' - Froude number gridpoint ( *** = supercritical flow )'
   write(jufrou,100)&
   &' - Froude number next cel ( *** = supercritical flow )'
   write(jufrou,100) ' '
!
!     Open dump file with residues and write header
!
   open (juresd,file=fdmprs)
   write(juresd,100)&
   &'Last residues before crash or continuation without'
   write(juresd,100)&
   &'convergence. Each record contains:'
   write(juresd,100)&
   &' - Time step number'
   write(juresd,100)&
   &' - Iteration step number'
   write(juresd,100)&
   &' - Maximum waterlevel residue with corresponding position'
   write(juresd,100)&
   &' - Maximum discharge residue with corresponding position'
   write(juresd,100) ' '
!
!     Open dump file with structure data and write header
!
   open (justrd,file=fdmpst)
   write(justrd,100)&
   &'Structure data of last steps before crash'
   write(justrd,100)&
   &'or continuation without convergence.'
   write(justrd,100)&
   &'Each record contains:'
   write(justrd,100)&
   &' - Time step number'
   write(justrd,100)&
   &' - Iteration step number'
   write(justrd,100)&
   &' - Per structure:'
   write(justrd,100)&
   &' - Structure name'
   write(justrd,100)&
   &' - Discharge'
   write(justrd,100)&
   &' - Head'
   write(justrd,100) ' '
!
!     Open dump file with solutions and write header
!
   open (jusold,file=fdmpsl)
   write(jusold,100)&
   &'Solution of last steps before crash'
   write(jusold,100)&
   &'or continuation without convergence.'
   write(jusold,100)&
   &'Each record contains:'
   write(jusold,100)&
   &' - Time step number'
   write(jusold,100)&
   &' - Iteration step number'
   write(jusold,100)&
   &' - Per gridpoint:'
   write(jusold,100)&
   &' - Location'
   write(jusold,100)&
   &' - Waterlevel'
   write(jusold,100)&
   &' - Discharge'
   write(jusold,100)&
   &' - Waterlevel referred to the top of the Preismann slot'
   write(jusold,100)&
   &'   (negative value means waterlevel inside of slot or'&
   &//' waterlevel below bottom)'
   write(jusold,100)&
   &' - Flow width'
   write(jusold,100)&
   &' - Total width'
   write(jusold,100)&
   &' - 1/C/C/R'
   write(jusold,100) ' '
   return
!
100 format (1x,a)
end
