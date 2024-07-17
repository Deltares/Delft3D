      subroutine cswsed ( ngrid  ,igp    ,maxlev ,nlev   ,
     +                    wft    ,hlev   ,wsec   ,hsec   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Cross Sectional Table Module
c
c Programmer:         J.Kuipers
c
c Module:             CSWSED (Cross Section Width SEDiment)
c
c Module description: Calculate height at sediment width 
c
c-----------------------------------------------------------------------
c Parameters:
c NAME              IO DESCRIPTION
c hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c    maxlev)           - For a circle cross section:
c                      (i,1) = Reference level.
c                      - For a sedredge cross section:
c                      (i,1) = Bed level of main section (i.e. left
c                              channel).
c                      (i,2) = Bed level of sub section 1 (i.e. right
c                              channel).
c hsec              O  height of section i
c igp               I  -
c maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                      declarations
c ngrid             I  Number of grid points in network.
c nlev(ngrid)       I  Number of h-levels for every cross section.
c                      (For a circle cross section   : 1 ;
c                       For a sedredge cross section : 2 )
c wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
c                              point i.
c                      - For a circle cross section:
c                      (i,1) = Radius of the circle.
c                      - For a sedredge cross section:
c                      (i,1) = Width of main section (i.e. left chan-
c                              nel).
c                      (i,2) = Width of sub section 1 (i.e. right
c                              channel).
c wsec              I  sediment width
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c epsequ  EQUal test with interval EPSilon
c=======================================================================
c
c
c     Parameters
c
      integer   ngrid, igp, maxlev
      integer   nlev(ngrid)
      real      wft (ngrid,maxlev)
      real      hsec, wsec 
	double precision hlev(ngrid,maxlev)
c
c     Local variables
c
      integer   ilev  
      real      w1    , w2
	double precision wght
      logical   inner ,epsequ
      external  epsequ
c
c     Check for maximum level
c
      if (wsec .ge. wft(igp,nlev(igp))) then
         hsec = hlev(igp,nlev(igp))
      elseif (wsec .lt. wft(igp,1)) then
c
c        Check for minimum level
c
         hsec = hlev(igp,1)
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
                  hsec = hlev(igp,ilev)
               else
c
c                 Calculate weigth factor
c
                  wght = (wsec - w1) / (w2 - w1)
c
c                 Determine water level
c
                  hsec = (1.0-wght) * hlev(igp,ilev-1) +
     +                        wght  * hlev(igp,ilev  )
c
               endif
               goto 200
            endif
 100     continue
      endif
c
 200  continue
c
      end
