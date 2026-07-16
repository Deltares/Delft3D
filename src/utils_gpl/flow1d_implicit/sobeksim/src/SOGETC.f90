subroutine SOGETC(fd_nefis, grpnam ,elmnam ,&
&nmax   ,nbytsg ,juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOGETC (SObek GET Character variable)
!
! Module description: Allocate memory and read contents of variable into
!                     memory (character pool). See chapter 6 of
!                     [S-DO-000.2SW] for an explanation of the memory
!                     management routines and pools.
!
!                     All variables used in sobek are stored in a pool.
!                     For each data type possible a pool will be decla-
!                     red. The variables are accessed with a 16 charac-
!                     ter name. This is the same name as is used on the
!                     nefis file.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 datfds            P  -
!  1 deffds            P  -
!  4 elmnam            I  Element name.
!  3 grpnam            P  -
!  7 juer              P  -
!  8 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  6 nbytsg            I  Number of bytes of one single nefis element
!  5 nmax              I  Number of elements of a variable
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! getcel  GET Character ELement from nefis file
! mkcpnt  MaKe Character PoiNTer
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
! $Log: sogetc.pf,v $
! Revision 1.5  1999/03/15  15:19:49  kuipe_j
! tabs removed
!
! Revision 1.4  1995/09/22  10:04:13  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:56:55  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:48  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:14  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:09:51  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:43  kuipe_j
! Initial version
!
!
!***********************************************************************
!

!
!       Parameters
!
   integer      fd_nefis
   character*16 grpnam, elmnam
   integer      nmax  , nbytsg
   integer      juer  , ker
!
!     Variables
!
   integer      start, stop, incr
   integer      uindex(3), usrord(1)
   integer      errcod, pntr, errno
   character*60 txt
   character*8  eno
   parameter    (start=1, stop=2, incr=3)
!
!     External functions
!
   integer      getcel, mkcpnt
   external     getcel, mkcpnt
!
!     Include memory pool and error codes
!
   include '..\include\mempool.i'
   include '..\include\errcod.i'
!
!     Initialise usrord
!
   usrord (1) = 1
!
   uindex ( start ) = 1
   uindex ( stop  ) = 1
   uindex ( incr  ) = 1
!
!     Create data space
!
   pntr = mkcpnt ( elmnam, nmax )
   if (pntr .lt. 0) then
      if (pntr .eq. -1) errno = evrdec
      if (pntr .eq. -2) errno = eoutds
      if (pntr .eq. -3) errno = eoutns

      txt = 'SOGETC Memory error for @' // elmnam // '@'
      ker = fatal
      call error ( juer, txt, errno, ker )

   else
!
!        Read the data
!
      errcod = getcel(fd_nefis, grpnam, elmnam,&
      &uindex, usrord, nmax*nbytsg,&
      &cp (pntr)&
      &)
!
!        Check for error
!
      if (errcod .ne. 0) then
         write(eno,'(i8)') errcod
         txt   = 'SOGETC @' // elmnam // '@ @' // eno // '@'
         ker   = fatal
         errno = egetel
         call error ( juer, txt, errno, ker )
      endif
   endif

   return
end
