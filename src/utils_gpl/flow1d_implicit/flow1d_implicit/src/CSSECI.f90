subroutine csseci ( ngrid  ,igp    ,maxlev ,nlev   ,&
&wft    ,hlev   ,aft    ,of     ,&
&wsec   ,hsec   ,asec   ,osec   ,&
&psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Cross Sectional Table Module
!
! Programmer:         J.Brouwer
!
! Module:             CSSECI (Cross Section SECtion I )
!
! Module description: In this subroutine the time-independent parameters
!                     Af(h0) = flow area in the main section for h=h0,
!                     and
!                     Af(h1) = flow area in sub section 1 for h=h1, will
!                     be computed.
!
!                     In subroutine CSSECI for each cross section the
!                     constant flow area Af(h0) and the wetted perimeter
!                     O0 are computed for the main section. The parame-
!                     ters Af(h1) and O1 are computed for sub section 1.
!                     Parameters computed in this subroutine are applied
!                     in the computation of Boussinesq's constant and
!                     the Chezy friction (See subroutine FLBOCH in flow
!                     module). Calls are made from subroutine CSSECT.
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  7 aft(ngrid,maxlev) I  (i,j) = flow area at h = hlev(i,j) for cross
!                                 section i.
! 11 asec              O  area of section i
!  6 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
! 10 hsec              O  height of section i
!  2 igp               I  -
!  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  1 ngrid             I  Number of grid points in network.
!  4 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
!  8 of(ngrid)         I  Actual wetted perimeter at every cross secti-
!                         on.
! 12 osec              O  wetted perimeter of section i
! 13 psltvr(7,ngrid)   P  -
!  5 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
!                                 point i.
!                         - For a circle cross section:
!                         (i,1) = Radius of the circle.
!                         - For a sedredge cross section:
!                         (i,1) = Width of main section (i.e. left chan-
!                                 nel).
!                         (i,2) = Width of sub section 1 (i.e. right
!                                 channel).
!  9 wsec              I  width of section i
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! epsequ  EQUal test with interval EPSilon
! flinaw  FLow INterpolate Area and Width
! flperi  FLow PERImeter
! indwgh  Compute INDex and WeiGHt factor
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: csseci.pf,v $
! Revision 1.5  1997/01/23  08:28:46  kuipe_j
! Make flow module robust
!
! Revision 1.4  1996/05/30  09:59:53  kuipe_j
! comment char
!
! Revision 1.3  1995/10/18  08:58:51  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  06:55:31  hoeks_a
! files changed from dos-file to unix-files
!
! Revision 1.1  1995/04/13  06:58:33  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/12/02  13:09:09  kuipe_j
! Improvement in width calculation (Wmain < width(0))
!
! Revision 1.2  1993/11/26  15:29:53  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:41  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer   ngrid, igp, maxlev
   integer   nlev(ngrid)
   real      aft (ngrid,maxlev),&
   &wft (ngrid,maxlev),&
   &of  (ngrid,maxlev),&
   &psltvr (7,ngrid)
   real      hsec, asec, osec, wsec, w1, w2
!
   double precision hsec0, hlev(ngrid,maxlev)
!
!     Local variables
!
   integer   ilev  , ilevel
   logical   epsequ, inner, lslot
   real      wdummy
   double precision wght
   external  epsequ
!
!     No slot has been inserted yet
!
   lslot = .false.
!
!     Check for maximum level
!
   if (wsec .ge. wft(igp,nlev(igp))) then
      hsec0 = hlev(igp,nlev(igp))
      asec  = aft (igp,nlev(igp))
      osec  = of  (igp,nlev(igp))
   elseif (wsec .lt. wft(igp,1)) then
!
!        Check for minimum level
!
      hsec0 = hlev(igp,1)
      asec  = aft (igp,1)
      osec  = wsec
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
               hsec0 = hlev(igp,ilev)
               asec  = aft (igp,ilev)
               osec  = of  (igp,ilev)
            else
!
!                 Calculate weigth factor
!
               wght = (wsec - w1) / (w2 - w1)
!
!                 Determine water level
!
               hsec0 = (1.0-wght) * hlev(igp,ilev-1) +&
               &wght  * hlev(igp,ilev  )
!
!                 Compute index and weight factor
!
               call INDWGH (ngrid  ,igp    ,&
               &maxlev ,nlev   ,hlev   ,&
               &hsec0  ,ilevel ,wght   )
!
!                 Compute A0
!
               call FLINAW (ngrid  ,igp    ,lslot  ,&
               &maxlev ,nlev   ,hlev   ,&
               &hsec0  ,ilevel ,wght   ,&
               &wft    ,aft    ,&
               &wdummy ,asec   ,psltvr )
!
!                 Compute O0
!
               call FLPERI (ngrid  ,igp    ,lslot  ,&
               &maxlev ,nlev   ,hlev   ,&
               &hsec0  ,ilevel ,&
               &wft    ,wsec   ,&
               &of     ,osec   ,psltvr )
!
            endif
            goto 200
         endif
100   continue
   endif
!
200 continue
!
   hsec = real(hsec0, kind=kind(hsec))
!
end
