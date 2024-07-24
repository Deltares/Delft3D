      subroutine sostat ( istep, nstep, lustat, lwstat )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOSTAT (SObek write STATus file)
c
c Module description: Write status information about calculation
c                     to status file
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 istep             I  Current time step number (t(n+1)).
c  3 lustat            I  LU number of status file
c  4 lwstat            I  True if status file must be written
c  2 nstep             I  Last time step number in simulation.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c socnam  SObek Create NAMe
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sostat.pf,v $
c Revision 1.7  1998/02/13  13:23:48  kuipe_j
c Adapt to CMT
c
c Revision 1.6  1995/12/06  08:33:00  kuipe_j
c Declaration added
c
c Revision 1.5  1995/10/17  10:12:39  hoeks_a
c Some small changes especially to makefiles, found by porting to
c a DEC-ALPHA station.
c
c Revision 1.4  1995/09/22  10:04:37  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:57:08  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:10:03  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:28  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:39:46  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      include '..\include\filsim.i'
c
c     Parameters
c
      integer       istep, nstep, lustat
      logical       lwstat
c
c     Local variables
c
      integer       perc5
c
c
c     Check if status file must be written
c
      if (lwstat) then
c
c        Write information every five percent of calculation
c
         perc5 = max(nstep/20,1)
c
         if (mod(istep,perc5) .eq. 0 .or. istep .eq. nstep) then
c
            write(lustat,'(F6.2,A)') real(istep)/real(nstep)*100. , '%'
c
c
c           flush file, so close and open in append mode
c
            close ( lustat )
c
            open ( unit = lustat, file = statfl, access = 'APPEND' )
c
         endif
      endif
c
      end
