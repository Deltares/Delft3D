      subroutine wqffcr ( lu, filnam, read ,ok )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         Jan Kuipers
c
c Module:             WQFFCR (Water Quality File Formatted CReate)
c
c Module description: Create or open formatted file
c
c                     This routine will create a formatted file. First a
c                     name is constructed by appending the extension
c                     after the file name. After this the file is ope-
c                     ned.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 filnam            I  File name without extension.
c  1 lu                I  Logical unitnumber associated with file.
c  5 ok                O  File to be read is present.
c  4 read              I  File wil be read
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
c $Log: wqffcr.pf,v $
c Revision 1.4  1999/03/15  15:53:54  kuipe_j
c tabs removed
c
c Revision 1.3  1998/02/13  12:13:01  kuipe_j
c Adapt to CMT
c
c Revision 1.2  1996/11/01  11:25:40  kuipe_j
c Update of Delwaq input file added
c
c Revision 1.1  1996/10/31  09:51:45  kuipe_j
c Calculation of exchanges added
c
c
c
c***********************************************************************
c
c     Parameters
c
      character*(*)  filnam
      integer        lu
      logical        read,   ok
c
c     Variables
c
      logical        fexist
c
c     Check if file exists, delete if this is the case
c
      inquire ( file = filnam, exist = fexist )
      if (fexist .and. .not.read) then
         open  ( unit = lu, file = filnam )
         close ( unit = lu, status = 'DELETE' )
      endif

      if (.not.fexist .and. read) then
         ok = .false.
      else
         ok = .true.
      endif
c
c     Open the file
c
      if (ok) then
         open ( unit = lu, file = filnam, form = 'FORMATTED' )
      endif
c
      end
