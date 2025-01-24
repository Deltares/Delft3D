subroutine SODEWQ ( nposeg, juer,  ker)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SODEWQ (SObek DEclare Water Quality interface variables)
!
! Module description: Declare variables for usage in the water quality
!                     interface module.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 juer              P  -
!  4 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  2 nposeg            I  Number of positive segment numbers.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! mkrpnt  MaKe Real PoiNTer
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
! $Log: sodewq.pf,v $
! Revision 1.8  1999/03/15  15:06:47  kuipe_j
! devided segmentes added
!
! Revision 1.7  1996/12/02  10:03:46  kuipe_j
! avoid negative pointers
!
! Revision 1.6  1996/11/12  15:12:26  kuipe_j
! Declare auxill. arrays later
!
! Revision 1.5  1995/10/18  09:00:57  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.4  1995/09/22  10:04:03  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:56:49  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:42  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:08  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:09:42  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:42  kuipe_j
! Initial version
!
!
!***********************************************************************
!

!
!     Parameters
!
   integer          nposeg,&
   &juer,  ker

!
!     Variables
!
   integer          errcod, size, errno
   character*16     name
   character*80     txt
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     External functions
!
   integer          mkrpnt
   external         mkrpnt
!
!     Segment functions
!
   size   = nposeg * 4
   name   = 'SEGFUN'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Segment lengths
!
   size   = nposeg
   name   = 'SLEN'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Segment volumes
!
   size   = nposeg * 3
   name   = 'SVOL'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900

   return

!
!     Error handler
!
900 continue

   ker = fatal

   if (errcod .eq. -1) errno = evrdec
   if (errcod .eq. -2) errno = eoutds
   if (errcod .eq. -3) errno = eoutns

   txt = 'SODEWQ Memory error for @' // name // '@'

   call error ( juer, txt, errno, ker )

   return
end
