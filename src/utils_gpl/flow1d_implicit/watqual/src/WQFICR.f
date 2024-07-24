      subroutine wqficr ( lu, filnam )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQFICR (Water Quality FIle CReate)
c
c Module description: Create binary file
c
c                     This routine will create a binary file. First a
c                     name is constructed by appending the extension
c                     after the file name. After this the file is ope-
c                     ned.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 filnam            I  File name without extension.
c  1 lu                I  Logical unitnumber associated with file.
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
c $Log: wqficr.pf,v $
c Revision 1.5  1998/02/13  12:13:02  kuipe_j
c Adapt to CMT
c
c Revision 1.4  1996/05/30  09:59:23  kuipe_j
c open for binaries
c
c Revision 1.3  1995/10/18  09:00:48  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:08:28  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:49  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:29  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      character*(*)  filnam
      integer        lu
c
c     Variables
c
      logical        fexist
c
c     Check if file exists, delete if this is the case
c
      inquire ( file = filnam, exist = fexist )
      if (fexist) then
         open  ( unit = lu, file = filnam )
         close ( unit = lu, status = 'DELETE' )
      endif
c
c     Open the binary file
c
c
c     Binary files for unix
c
#if defined (USE_MSWINDOWS)
      open ( unit = lu, file = filnam, form = 'BINARY' )
#else
      open ( unit = lu, file = filnam, form = 'UNFORMATTED' )
#endif
c
      return
      end
