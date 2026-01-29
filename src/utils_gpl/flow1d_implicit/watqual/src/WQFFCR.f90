subroutine wqffcr ( lu, filnam, read ,ok )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         Jan Kuipers
!
! Module:             WQFFCR (Water Quality File Formatted CReate)
!
! Module description: Create or open formatted file
!
!                     This routine will create a formatted file. First a
!                     name is constructed by appending the extension
!                     after the file name. After this the file is ope-
!                     ned.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 filnam            I  File name without extension.
!  1 lu                I  Logical unitnumber associated with file.
!  5 ok                O  File to be read is present.
!  4 read              I  File wil be read
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
! $Log: wqffcr.pf,v $
! Revision 1.4  1999/03/15  15:53:54  kuipe_j
! tabs removed
!
! Revision 1.3  1998/02/13  12:13:01  kuipe_j
! Adapt to CMT
!
! Revision 1.2  1996/11/01  11:25:40  kuipe_j
! Update of Delwaq input file added
!
! Revision 1.1  1996/10/31  09:51:45  kuipe_j
! Calculation of exchanges added
!
!
!
!***********************************************************************
!
!     Parameters
!
   character*(*)  filnam
   integer        lu
   logical        read,   ok
!
!     Variables
!
   logical        fexist
!
!     Check if file exists, delete if this is the case
!
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
!
!     Open the file
!
   if (ok) then
      open ( unit = lu, file = filnam, form = 'FORMATTED' )
   endif
!
end
