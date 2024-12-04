subroutine FLSEDR (ibr    ,h      ,zbmain ,zbsub1 ,wmain  ,wsub1 ,&
&juer   ,af     ,wf     ,o      ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLSEDR (FLow SEDRedge branch)
!
! Module description: Subroutine FLSEDR calculates for the actual water
!                     level the width, area and wetted perimeter.
!
!                     A sedredge branch is given as a small table with
!                     two bed levels and two widths. The main section
!                     will be the left channel and sub section 1 will be
!                     the right channel.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 af(ngrid)         O  Flow area at every grid point at time t(n+1)
!  2 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
!  1 ibr               I  Branch number.
!  7 juer              P  -
! 11 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 10 o(ngrid)          O  Wetted perimeter for total cross section.
!  9 wf(ngrid)         O  Actual flow width at every grid point.
!  5 wmain             I  Width of main section sedredge branch (ie left
!                         channel).
!  6 wsub1             I  Width of sub section 1 sedredge branch (ie
!                         right channel).
!  3 zbmain            I  Bed level Zb1 of main section sedredge branch
!                         (ie left channel).
!  4 zbsub1            I  Bed level Zb2 of sub section 1 sedredge branch
!                         (ie right channel).
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flsedr.pf,v $
! Revision 1.5  1999/06/01  13:42:21  kuipe_j
! names in messages substituted + message template
!
! Revision 1.4  1999/03/15  15:50:45  kuipe_j
! tabs removed
!
! Revision 1.3  1995/05/30  09:55:28  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:28  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:08  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:31:34  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:55  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   integer ibr, juer, ker
   real    wmain, wsub1, af, wf, o
   double precision h, zbmain, zbsub1
!
!     Declaration of local variables:
!
   character branam*40
   integer   lbrnam
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     Check if water level > bottom in main and subsection
!
   if ( h .lt. zbmain .or. h .lt. zbsub1 ) then
      ker = fatal
      call getbrn (ibr,branam,lbrnam)
      call ERROR (juer,'FLSEDR  branch @'//branam(:lbrnam)//&
      &'@',eflhse,ker)
      goto 1000
   else
      af = (h-zbmain)*wmain + (h-zbsub1)*wsub1
      wf = wmain + wsub1
      o  = wmain + wsub1 + (h-zbmain) + (h-zbsub1) +&
      &abs(zbmain-zbsub1)
   endif
!
1000 continue
end
