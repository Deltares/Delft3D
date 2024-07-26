      subroutine SOCNAM ( filnam, reqnam, ext )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOCNAM (SObek Create NAMe)
c
c Module description: Create a new file name.
c
c                     Create a new file name using the model file name
c                     without an extension and the new extension which
c                     is passed by the calling routine.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 ext               I  Extension to be appended to filename
c  1 filnam            I  Filename of model to be processed without
c                         extension
c  2 reqnam            O  Request name (=filname + extension)
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
c $Log: socnam.pf,v $
c Revision 1.5  1995/10/18  09:00:55  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.4  1995/09/22  10:03:27  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:56:42  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:34  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:00  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:39:40  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Parameters
c
      character*(*) filnam, reqnam
      character*4   ext
c
c     Variables
c
      integer       lnam  , lennam, i
      character     lflnam*261

      lflnam        = ' '
      lflnam(1:256) = filnam

      lnam = index (lflnam, ' ')
c      if (lnam .eq. 0) then
c         lnam = 9
c      endif
c
c     Create request name
c
      reqnam(1:lnam+3) = lflnam (1:lnam-1) // ext(1:4)
c
c     Pad with spaces (needed for SG-machines)
c
      lennam = len ( reqnam )
c
      do 100 i = lnam+4, lennam
         reqnam(i:i) = ' '
 100  continue
c
      return
      end
