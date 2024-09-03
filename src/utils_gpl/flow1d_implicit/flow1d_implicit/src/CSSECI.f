      subroutine csseci ( ngrid  ,igp    ,maxlev ,nlev   ,
     +                    wft    ,hlev   ,aft    ,of     ,
     +                    wsec   ,hsec   ,asec   ,osec   ,
     +                    psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Cross Sectional Table Module
c
c Programmer:         J.Brouwer
c
c Module:             CSSECI (Cross Section SECtion I )
c
c Module description: In this subroutine the time-independent parameters
c                     Af(h0) = flow area in the main section for h=h0,
c                     and
c                     Af(h1) = flow area in sub section 1 for h=h1, will
c                     be computed.
c
c                     In subroutine CSSECI for each cross section the
c                     constant flow area Af(h0) and the wetted perimeter
c                     O0 are computed for the main section. The parame-
c                     ters Af(h1) and O1 are computed for sub section 1.
c                     Parameters computed in this subroutine are applied
c                     in the computation of Boussinesq's constant and
c                     the Chezy friction (See subroutine FLBOCH in flow
c                     module). Calls are made from subroutine CSSECT.
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  7 aft(ngrid,maxlev) I  (i,j) = flow area at h = hlev(i,j) for cross
c                                 section i.
c 11 asec              O  area of section i
c  6 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c 10 hsec              O  height of section i
c  2 igp               I  -
c  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  1 ngrid             I  Number of grid points in network.
c  4 nlev(ngrid)       I  Number of h-levels for every cross section.
c                         (For a circle cross section   : 1 ;
c                          For a sedredge cross section : 2 )
c  8 of(ngrid)         I  Actual wetted perimeter at every cross secti-
c                         on.
c 12 osec              O  wetted perimeter of section i
c 13 psltvr(7,ngrid)   P  -
c  5 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
c                                 point i.
c                         - For a circle cross section:
c                         (i,1) = Radius of the circle.
c                         - For a sedredge cross section:
c                         (i,1) = Width of main section (i.e. left chan-
c                                 nel).
c                         (i,2) = Width of sub section 1 (i.e. right
c                                 channel).
c  9 wsec              I  width of section i
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c epsequ  EQUal test with interval EPSilon
c flinaw  FLow INterpolate Area and Width
c flperi  FLow PERImeter
c indwgh  Compute INDex and WeiGHt factor
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: csseci.pf,v $
c Revision 1.5  1997/01/23  08:28:46  kuipe_j
c Make flow module robust
c
c Revision 1.4  1996/05/30  09:59:53  kuipe_j
c comment char
c
c Revision 1.3  1995/10/18  08:58:51  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  06:55:31  hoeks_a
c files changed from dos-file to unix-files
c
c Revision 1.1  1995/04/13  06:58:33  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/12/02  13:09:09  kuipe_j
c Improvement in width calculation (Wmain < width(0))
c
c Revision 1.2  1993/11/26  15:29:53  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:41  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer   ngrid, igp, maxlev
      integer   nlev(ngrid)
      real      aft (ngrid,maxlev),
     +          wft (ngrid,maxlev),
     +          of  (ngrid,maxlev),
     +          psltvr (7,ngrid)
      real      hsec, asec, osec, wsec, w1, w2
c
      double precision hsec0, hlev(ngrid,maxlev)
c
c     Local variables
c
      integer   ilev  , ilevel
      logical   epsequ, inner, lslot
      real      wdummy
	double precision wght
      external  epsequ
c
c     No slot has been inserted yet
c
      lslot = .false.
c
c     Check for maximum level
c
      if (wsec .ge. wft(igp,nlev(igp))) then
         hsec0 = hlev(igp,nlev(igp))
         asec  = aft (igp,nlev(igp))
         osec  = of  (igp,nlev(igp))
      elseif (wsec .lt. wft(igp,1)) then
c
c        Check for minimum level
c
         hsec0 = hlev(igp,1)
         asec  = aft (igp,1)
         osec  = wsec
      else
c
c        Search from top to bottom
c
         do 100 ilev = nlev(igp), 2, -1
c
c           Read table widths
c
            w2 = wft(igp,ilev)
            w1 = wft(igp,ilev-1)
c
c           Check if width is inside range
c
            inner = wsec .ge. w1 .and. wsec .le. w2
c
            if (inner) then
               if (epsequ(w1,w2,1E-8)) then
c
c                 Equal widhts on two levels, take highest level
c
                  hsec0 = hlev(igp,ilev)
                  asec  = aft (igp,ilev)
                  osec  = of  (igp,ilev)
               else
c
c                 Calculate weigth factor
c
                  wght = (wsec - w1) / (w2 - w1)
c
c                 Determine water level
c
                  hsec0 = (1.0-wght) * hlev(igp,ilev-1) +
     +                        wght  * hlev(igp,ilev  )
c
c                 Compute index and weight factor
c
                  call INDWGH (ngrid  ,igp    ,
     +                         maxlev ,nlev   ,hlev   ,
     +                         hsec0  ,ilevel ,wght   )
c
c                 Compute A0
c
                  call FLINAW (ngrid  ,igp    ,lslot  ,
     +                         maxlev ,nlev   ,hlev   ,
     +                         hsec0  ,ilevel ,wght   ,
     +                         wft    ,aft    ,
     +                         wdummy ,asec   ,psltvr )
c
c                 Compute O0
c
                  call FLPERI (ngrid  ,igp    ,lslot  ,
     +                         maxlev ,nlev   ,hlev   ,
     +                         hsec0  ,ilevel ,
     +                         wft    ,wsec   ,
     +                         of     ,osec   ,psltvr )
c
               endif
               goto 200
            endif
 100     continue
      endif
c
 200  continue
c
      hsec = real(hsec0, kind=kind(hsec))
c
      end
