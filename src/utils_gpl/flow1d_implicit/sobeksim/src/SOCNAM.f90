subroutine SOCNAM ( filnam, reqnam, ext )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOCNAM (SObek Create NAMe)
!
! Module description: Create a new file name.
!
!                     Create a new file name using the model file name
!                     without an extension and the new extension which
!                     is passed by the calling routine.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 ext               I  Extension to be appended to filename
!  1 filnam            I  Filename of model to be processed without
!                         extension
!  2 reqnam            O  Request name (=filname + extension)
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
! $Log: socnam.pf,v $
! Revision 1.5  1995/10/18  09:00:55  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.4  1995/09/22  10:03:27  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:56:42  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:34  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:00  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:39:40  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!
!     Parameters
!
   character*(*) filnam, reqnam
   character*4   ext
!
!     Variables
!
   integer       lnam  , lennam, i
   character     lflnam*261

   lflnam        = ' '
   lflnam(1:256) = filnam

   lnam = index (lflnam, ' ')
!      if (lnam .eq. 0) then
!         lnam = 9
!      endif
!
!     Create request name
!
   reqnam(1:lnam+3) = lflnam (1:lnam-1) // ext(1:4)
!
!     Pad with spaces (needed for SG-machines)
!
   lennam = len ( reqnam )
!
   do 100 i = lnam+4, lennam
      reqnam(i:i) = ' '
100 continue
!
   return
end
