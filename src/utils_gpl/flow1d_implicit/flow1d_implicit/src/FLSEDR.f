      subroutine FLSEDR (ibr    ,h      ,zbmain ,zbsub1 ,wmain  ,wsub1 ,
     +                   juer   ,af     ,wf     ,o      ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLSEDR (FLow SEDRedge branch)
c
c Module description: Subroutine FLSEDR calculates for the actual water
c                     level the width, area and wetted perimeter.
c
c                     A sedredge branch is given as a small table with
c                     two bed levels and two widths. The main section
c                     will be the left channel and sub section 1 will be
c                     the right channel.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 af(ngrid)         O  Flow area at every grid point at time t(n+1)
c  2 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c  1 ibr               I  Branch number.
c  7 juer              P  -
c 11 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 10 o(ngrid)          O  Wetted perimeter for total cross section.
c  9 wf(ngrid)         O  Actual flow width at every grid point.
c  5 wmain             I  Width of main section sedredge branch (ie left
c                         channel).
c  6 wsub1             I  Width of sub section 1 sedredge branch (ie
c                         right channel).
c  3 zbmain            I  Bed level Zb1 of main section sedredge branch
c                         (ie left channel).
c  4 zbsub1            I  Bed level Zb2 of sub section 1 sedredge branch
c                         (ie right channel).
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flsedr.pf,v $
c Revision 1.5  1999/06/01  13:42:21  kuipe_j
c names in messages substituted + message template
c
c Revision 1.4  1999/03/15  15:50:45  kuipe_j
c tabs removed
c
c Revision 1.3  1995/05/30  09:55:28  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:28  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:08  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:31:34  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:55  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters:
c
      integer ibr, juer, ker
      real    wmain, wsub1, af, wf, o
      double precision h, zbmain, zbsub1
c
c     Include sobek error code file
c
      include '../include/errcod.i'
c
c     Check if water level > bottom in main and subsection
c
      if ( h .lt. zbmain .or. h .lt. zbsub1 ) then
c         ker = fatal
c         call getbrn (ibr,branam,lbrnam)
c         call sre_error (juer,'FLSEDR  branch @'//branam(:lbrnam)//
c     &               '@',eflhse,ker)
         goto 1000
      else
         af = (h-zbmain)*wmain + (h-zbsub1)*wsub1
         wf = wmain + wsub1
         o  = wmain + wsub1 + (h-zbmain) + (h-zbsub1) +
     +        abs(zbmain-zbsub1)
      endif
c
 1000 continue
      end
