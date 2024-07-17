      subroutine moseci ( ngrid  ,igp    ,maxlev ,nlev   ,
     +                    wft    ,hlev   ,wsact  ,hws    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOSECI (MOrphology SECtion I)
c
c Module description: Calculate waterlevel for sed. transporting width
c
c                     Calculate waterlevel where sediment transporting
c                     width is defined. This level is used for the
c                     adaption cross section algoritm proportional
c                     to local water depth.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  8 hws               O  Water level for sediment transporting width
c  2 igp               I  Gridpoint number
c  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  1 ngrid             I  Number of grid points in network.
c  4 nlev(ngrid)       I  Number of h-levels for every cross section.
c                         (For a circle cross section   : 1 ;
c                          For a sedredge cross section : 2 )
c  5 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
c                                 point i.
c                         - For a circle cross section:
c                         (i,1) = Radius of the circle.
c                         - For a sedredge cross section:
c                         (i,1) = Width of main section (i.e. left chan-
c                                 nel).
c                         (i,2) = Width of sub section 1 (i.e. right
c                                 channel).
c  7 wsact             I  Actual sediment transporting width
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c epsequ  EQUal test with interval EPSilon
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id:
c
c History:
c $Log:
c
c
c***********************************************************************
c
c     Parameters
c
      integer   ngrid, igp, maxlev
      integer   nlev(ngrid)
      real      wft (ngrid,maxlev)
      real      wsact
      double precision hlev(ngrid,maxlev), hws
c
c     Local variables
c
      integer   ilev
      logical   epsequ, inner
      external  epsequ
      real      w1, w2 
	double precision wght
c
c     Check for maximum level
c
      if (wsact .ge. wft(igp,nlev(igp))) then
c
         hws = hlev(igp,nlev(igp))
c
c        Check for minimum level
c
      elseif (wsact .lt. wft(igp,1)) then
c
         hws = hlev(igp,1)
c
      else
c
c        Search from top to bottom
c
         do 100 ilev = nlev(igp), 2, -1
c
c           Read table widths from level ilev and ilev-1
c
            w2 = wft(igp,ilev)
            w1 = wft(igp,ilev-1)
c
c           Check if width is inside range
c
            inner = wsact .ge. w1 .and. wsact .le. w2
c
            if (inner) then
               if (epsequ(w1,w2,1E-8)) then
c
c                 Equal widhts on two levels, take highest level
c
                  hws = hlev(igp,ilev)
               else
c
c                 Calculate weigth factor
c
                  wght = dble ( (wsact - w1) / (w2 - w1) )
c
c                 Determine water level for ws
c
                  hws = (1.0D0 -wght) * hlev(igp,ilev-1) +
     +                      wght  * hlev(igp,ilev  )
               endif
               goto 200
            endif
 100     continue
      endif
c
c     Label: level found and calculated
c
 200  continue
c
      end
