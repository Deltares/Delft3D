subroutine moseci ( ngrid  ,igp    ,maxlev ,nlev   ,&
&wft    ,hlev   ,wsact  ,hws    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOSECI (MOrphology SECtion I)
!
! Module description: Calculate waterlevel for sed. transporting width
!
!                     Calculate waterlevel where sediment transporting
!                     width is defined. This level is used for the
!                     adaption cross section algoritm proportional
!                     to local water depth.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  8 hws               O  Water level for sediment transporting width
!  2 igp               I  Gridpoint number
!  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  1 ngrid             I  Number of grid points in network.
!  4 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
!  5 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
!                                 point i.
!                         - For a circle cross section:
!                         (i,1) = Radius of the circle.
!                         - For a sedredge cross section:
!                         (i,1) = Width of main section (i.e. left chan-
!                                 nel).
!                         (i,2) = Width of sub section 1 (i.e. right
!                                 channel).
!  7 wsact             I  Actual sediment transporting width
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
! $Id:
!
! History:
! $Log:
!
!
!***********************************************************************
!
!     Parameters
!
   integer   ngrid, igp, maxlev
   integer   nlev(ngrid)
   real      wft (ngrid,maxlev)
   real      wsact
   double precision hlev(ngrid,maxlev), hws
!
!     Local variables
!
   integer   ilev
   logical   epsequ, inner
   external  epsequ
   real      w1, w2
   double precision wght
!
!     Check for maximum level
!
   if (wsact .ge. wft(igp,nlev(igp))) then
!
      hws = hlev(igp,nlev(igp))
!
!        Check for minimum level
!
   elseif (wsact .lt. wft(igp,1)) then
!
      hws = hlev(igp,1)
!
   else
!
!        Search from top to bottom
!
      do 100 ilev = nlev(igp), 2, -1
!
!           Read table widths from level ilev and ilev-1
!
         w2 = wft(igp,ilev)
         w1 = wft(igp,ilev-1)
!
!           Check if width is inside range
!
         inner = wsact .ge. w1 .and. wsact .le. w2
!
         if (inner) then
            if (epsequ(w1,w2,1E-8)) then
!
!                 Equal widhts on two levels, take highest level
!
               hws = hlev(igp,ilev)
            else
!
!                 Calculate weigth factor
!
               wght = dble ( (wsact - w1) / (w2 - w1) )
!
!                 Determine water level for ws
!
               hws = (1.0D0 -wght) * hlev(igp,ilev-1) +&
               &wght  * hlev(igp,ilev  )
            endif
            goto 200
         endif
100   continue
   endif
!
!     Label: level found and calculated
!
200 continue
!
end
