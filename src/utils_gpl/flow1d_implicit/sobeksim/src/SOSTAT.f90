subroutine sostat ( istep, nstep, lustat, lwstat )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOSTAT (SObek write STATus file)
!
! Module description: Write status information about calculation
!                     to status file
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 istep             I  Current time step number (t(n+1)).
!  3 lustat            I  LU number of status file
!  4 lwstat            I  True if status file must be written
!  2 nstep             I  Last time step number in simulation.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! socnam  SObek Create NAMe
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
! $Log: sostat.pf,v $
! Revision 1.7  1998/02/13  13:23:48  kuipe_j
! Adapt to CMT
!
! Revision 1.6  1995/12/06  08:33:00  kuipe_j
! Declaration added
!
! Revision 1.5  1995/10/17  10:12:39  hoeks_a
! Some small changes especially to makefiles, found by porting to
! a DEC-ALPHA station.
!
! Revision 1.4  1995/09/22  10:04:37  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:57:08  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:10:03  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:28  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:39:46  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   include '..\include\filsim.i'
!
!     Parameters
!
   integer       istep, nstep, lustat
   logical       lwstat
!
!     Local variables
!
   integer       perc5
!
!
!     Check if status file must be written
!
   if (lwstat) then
!
!        Write information every five percent of calculation
!
      perc5 = max(nstep/20,1)
!
      if (mod(istep,perc5) .eq. 0 .or. istep .eq. nstep) then
!
         write(lustat,'(F6.2,A)') real(istep)/real(nstep)*100. , '%'
!
!
!           flush file, so close and open in append mode
!
         close ( lustat )
!
         open ( unit = lustat, file = statfl, access = 'APPEND' )
!
      endif
   endif
!
end
