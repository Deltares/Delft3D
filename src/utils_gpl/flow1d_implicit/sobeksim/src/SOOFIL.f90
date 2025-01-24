subroutine SOOFIL ( coding     , fd_nefis ,&
&defnam     , datnam   ,&
&juer       , ker      )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOOFIL (SObek Open FILe)
!
! Module description: Open definition and data file set
!
!                     A nefis file set contains a defintion file and
!                     data file. This set is opened by this routine.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 coding            P  -
!  3 datfds            P  -
!  5 datnam            I  Data file name
!  2 deffds            P  -
!  4 defnam            I  Definition file name
!  6 juer              P  -
!  7 ker               IO Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! opndat  OPeNs a DATa file
! opndef  OPeNs a DEFinition file
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
! $Log: soofil.pf,v $
! Revision 1.5  1998/02/13  13:21:34  kuipe_j
! SRS BOS
!
! Revision 1.4  1995/09/22  10:04:26  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:57:02  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:56  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:22  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:10:02  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:44  kuipe_j
! Initial version
!
!
!***********************************************************************
!

!
!     Parameters
!
   character*(*)   coding
   integer       fd_nefis
   character*(*) defnam   ,datnam
   integer       juer     ,ker
!
!     Local variables
!
   integer       errcod   , errno
   character*80  txt
   character*8   eno
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     External functions
!
   integer, external     :: crenef
!
!     Open NEFIS-file
!
   errcod = crenef(fd_nefis, datnam, defnam, coding, 'u' )

   if (errcod .ne. 0) then
      write(eno,'(i8)') errcod
      txt   = 'SOOFIL crenef @' // defnam // '@ @' // eno // '@'
      ker   = fatal
      errno = eopndf
      call error (juer, txt, errno, ker )
   endif

end
