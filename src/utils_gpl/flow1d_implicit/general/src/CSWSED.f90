subroutine cswsed ( ngrid  ,igp    ,maxlev ,nlev   ,&
&wft    ,hlev   ,wsec   ,hsec   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Cross Sectional Table Module
!
! Programmer:         J.Kuipers
!
! Module:             CSWSED (Cross Section Width SEDiment)
!
! Module description: Calculate height at sediment width
!
!-----------------------------------------------------------------------
! Parameters:
! NAME              IO DESCRIPTION
! hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!    maxlev)           - For a circle cross section:
!                      (i,1) = Reference level.
!                      - For a sedredge cross section:
!                      (i,1) = Bed level of main section (i.e. left
!                              channel).
!                      (i,2) = Bed level of sub section 1 (i.e. right
!                              channel).
! hsec              O  height of section i
! igp               I  -
! maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                      declarations
! ngrid             I  Number of grid points in network.
! nlev(ngrid)       I  Number of h-levels for every cross section.
!                      (For a circle cross section   : 1 ;
!                       For a sedredge cross section : 2 )
! wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
!                              point i.
!                      - For a circle cross section:
!                      (i,1) = Radius of the circle.
!                      - For a sedredge cross section:
!                      (i,1) = Width of main section (i.e. left chan-
!                              nel).
!                      (i,2) = Width of sub section 1 (i.e. right
!                              channel).
! wsec              I  sediment width
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! epsequ  EQUal test with interval EPSilon
!=======================================================================
!
!
!     Parameters
!
   integer   ngrid, igp, maxlev
   integer   nlev(ngrid)
   real      wft (ngrid,maxlev)
   real      hsec, wsec
   double precision hlev(ngrid,maxlev)
!
!     Local variables
!
   integer   ilev
   real      w1    , w2
   double precision wght
   logical   inner ,epsequ
   external  epsequ
!
!     Check for maximum level
!
   if (wsec .ge. wft(igp,nlev(igp))) then
      hsec = hlev(igp,nlev(igp))
   elseif (wsec .lt. wft(igp,1)) then
!
!        Check for minimum level
!
      hsec = hlev(igp,1)
   else
!
!        Search from top to bottom
!
      do 100 ilev = nlev(igp), 2, -1
!
!           Read table widths
!
         w2 = wft(igp,ilev)
         w1 = wft(igp,ilev-1)
!
!           Check if width is inside range
!
         inner = wsec .ge. w1 .and. wsec .le. w2
!
         if (inner) then
            if (epsequ(w1,w2,1E-8)) then
!
!                 Equal widhts on two levels, take highest level
!
               hsec = hlev(igp,ilev)
            else
!
!                 Calculate weigth factor
!
               wght = (wsec - w1) / (w2 - w1)
!
!                 Determine water level
!
               hsec = (1.0-wght) * hlev(igp,ilev-1) +&
               &wght  * hlev(igp,ilev  )
!
            endif
            goto 200
         endif
100   continue
   endif
!
200 continue
!
end
