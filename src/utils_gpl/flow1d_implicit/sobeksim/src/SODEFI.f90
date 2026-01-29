subroutine SODEFI (lflow, lwqin, newres, juer, ker)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SODEFI (SObek DEclare FIle descriptors)
!
! Module description: Declare file descriptors
!
!                     Depending on the selected application one or more
!                     file descriptors must be declared in memory.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 juer              P  -
!  5 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  1 lflow             I  Switch to enable flow module
!  2 lwqin             I  Logical, = TRUE indicates the  water quality
!                         interface file must be written.
!  3 newres            I  true, if a new restart file will be made
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! mkipnt  MaKe Integer PoiNTer
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
! $Log: sodefi.pf,v $
! Revision 1.7  1999/03/15  15:19:38  kuipe_j
! tabs removed
!
! Revision 1.6  1996/01/17  14:47:34  kuipe_j
! header update
!
! Revision 1.5  1996/01/16  15:01:53  kuipe_j
! Restart improvements
!
! Revision 1.4  1995/09/22  10:03:55  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:56:44  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:37  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:02  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:09:33  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:41  kuipe_j
! Initial version
!
!
!***********************************************************************
!

!
!     Parameters
!
   logical         lflow, lwqin, newres
   integer         juer, ker
!
!     Variables
!
   integer         errcod, size, errno
   character*16    name
   character*80    txt
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     External functions
!
   integer         mkipnt
   external        mkipnt

!
!     NEFIS-File descriptor for results
!
   size   = 1
   name   = 'FD_NEFIS_RES'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     NEFIS-File descriptor for restart
!
   size   = 1
   name   = 'FD_NEFIS_RST'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     NEFIS-Filedescriptor for new restart
!
   size   = 1
   name   = 'FD_NEFIS_NEW'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900

!
!     NEFIS-File descriptor for WQ-Interface file
!
   size   = 1
   name   = 'FD_NEFIS_WAQ'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900

   return

!
!     Error handler
!
900 continue

   ker = fatal

   if (errcod .eq. -1) errno = evrdec
   if (errcod .eq. -2) errno = eoutds
   if (errcod .eq. -3) errno = eoutns

   txt = 'SODEFI Memory error for @' // name // '@'

   call error ( juer, txt, errno, ker )

   return
end
