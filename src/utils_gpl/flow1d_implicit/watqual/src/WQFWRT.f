      subroutine wqfwrt ( lu, time, nrar, rar )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQFWRT (Water Quality File WRiTe)
c
c Module description: Append information to binary output file
c
c                     This routine will append information to the output
c                     file. The array containing reals will be accepted
c                     and written to the output file. The real values
c                     will be preceded by one integer usually indicating
c                     time.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 lu                I  Logical unitnumber associated with file.
c  3 nrar              I  Number of elements in array RA.
c  4 rar(nrar)         I  Real array containing information to be writ-
c                         ten to file.
c  2 time              I  Actual time level tn+1. in sec.
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
c $Log: wqfwrt.pf,v $
c Revision 1.3  1995/10/18  09:00:49  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:08:33  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:55  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:26  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer        lu,  time, nrar
      real           rar(nrar)
c
c     Variables
c
      integer        j
c
      write(lu) time, (rar(j), j=1,nrar)
c
      return
      end
