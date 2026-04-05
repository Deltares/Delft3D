subroutine wqficr ( lu, filnam )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQFICR (Water Quality FIle CReate)
!
! Module description: Create binary file
!
!                     This routine will create a binary file. First a
!                     name is constructed by appending the extension
!                     after the file name. After this the file is ope-
!                     ned.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 filnam            I  File name without extension.
!  1 lu                I  Logical unitnumber associated with file.
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
! $Log: wqficr.pf,v $
! Revision 1.5  1998/02/13  12:13:02  kuipe_j
! Adapt to CMT
!
! Revision 1.4  1996/05/30  09:59:23  kuipe_j
! open for binaries
!
! Revision 1.3  1995/10/18  09:00:48  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:08:28  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:49  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:29  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   character*(*)  filnam
   integer        lu
!
!     Variables
!
   logical        fexist
!
!     Check if file exists, delete if this is the case
!
   inquire ( file = filnam, exist = fexist )
   if (fexist) then
      open  ( unit = lu, file = filnam )
      close ( unit = lu, status = 'DELETE' )
   endif
!
!     Open the binary file
!
!
!     Binary files for unix
!
#if defined (USE_MSWINDOWS)
   open ( unit = lu, file = filnam, form = 'BINARY' )
#else
   open ( unit = lu, file = filnam, form = 'UNFORMATTED' )
#endif
!
   return
end
