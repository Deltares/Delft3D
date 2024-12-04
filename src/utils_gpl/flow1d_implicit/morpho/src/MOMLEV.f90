subroutine momlev ( igp,&
&ngrid,&
&maxlev,&
&nlev,&
&wft,&
&ws,&
&k,&
&wsact&
&)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOMLEV (MORPHology Morphodynamic LEVel)
!
! Module description: Find highest bed level in cross section which is
!                     morphodynamic active.
!
!                     First the actual transport width is calculated by
!                     comparing the actual flow width with the user
!                     defined transport width. If the flow width is
!                     smaller then the transport width the actual trans-
!                     port width will be adapted.
!
!                     If the actual transport width has been determined
!                     the highest bed level of the cross section is
!                     searched below or corresponding with the actual
!                     transport width. If a rectangular profile is used
!                     the lowest level will be chosen.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 igp               I  Gridpoint number
!  9 k                 IO Cross section level which is morphodynamic
!                         active
!  4 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  3 ngrid             I  Number of grid points in network.
!  5 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
!  7 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
!                                 point i.
!                         - For a circle cross section:
!                         (i,1) = Radius of the circle.
!                         - For a sedredge cross section:
!                         (i,1) = Width of main section (i.e. left chan-
!                                 nel).
!                         (i,2) = Width of sub section 1 (i.e. right
!                                 channel).
!  8 ws(ngrid)         I  Sediment transporting width for each grid
!                         point.
! 10 wsact             IO Actual sediment transporting width
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! epsequ  EQUal test with interval EPSilon
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: momlev.pf,v $
! Revision 1.5  1997/06/17  11:18:24  kuipe_j
! Remove undefined vars
!
! Revision 1.4  1995/10/18  09:00:02  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/09/22  10:03:13  kuipe_j
! variable dimensions, new headers
!
! Revision 1.2  1995/05/30  07:04:52  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:20  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:32:50  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:08  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer   igp,&
   &k,&
   &maxlev,&
   &ngrid,&
   &nlev (ngrid)

   real      wft  (ngrid,maxlev),&
   &ws   (ngrid),&
   &wsact

!
!     Variables
!
   integer   i
!
   logical   epsequ
   external  epsequ
!
!     Determine transport width
!
   wsact = ws(igp)
!
!     Find highest bed level below wf
!
   k = 1
   do 100 i = 1, nlev(igp)
      if (wft(igp,i) .le. wsact) then
         k = i
      else
         goto 200
      endif
100 continue
!
200 continue
!
!     Now check for rectangular profile
!
   if (k .gt. 1) then
      if (epsequ ( wft(igp,k-1), wsact, 1.0E-4 )) then
         k = k - 1
      endif
   endif
!
end
