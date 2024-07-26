      subroutine momlev ( igp,
     +                    ngrid,
     +                    maxlev,
     +                    nlev,
     +                    wft,
     +                    ws,
     +                    k,
     +                    wsact
     +                  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOMLEV (MORPHology Morphodynamic LEVel)
c
c Module description: Find highest bed level in cross section which is
c                     morphodynamic active.
c
c                     First the actual transport width is calculated by
c                     comparing the actual flow width with the user
c                     defined transport width. If the flow width is
c                     smaller then the transport width the actual trans-
c                     port width will be adapted.
c
c                     If the actual transport width has been determined
c                     the highest bed level of the cross section is
c                     searched below or corresponding with the actual
c                     transport width. If a rectangular profile is used
c                     the lowest level will be chosen.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 igp               I  Gridpoint number
c  9 k                 IO Cross section level which is morphodynamic
c                         active
c  4 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  3 ngrid             I  Number of grid points in network.
c  5 nlev(ngrid)       I  Number of h-levels for every cross section.
c                         (For a circle cross section   : 1 ;
c                          For a sedredge cross section : 2 )
c  7 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
c                                 point i.
c                         - For a circle cross section:
c                         (i,1) = Radius of the circle.
c                         - For a sedredge cross section:
c                         (i,1) = Width of main section (i.e. left chan-
c                                 nel).
c                         (i,2) = Width of sub section 1 (i.e. right
c                                 channel).
c  8 ws(ngrid)         I  Sediment transporting width for each grid
c                         point.
c 10 wsact             IO Actual sediment transporting width
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
c $Id$
c
c History:
c $Log: momlev.pf,v $
c Revision 1.5  1997/06/17  11:18:24  kuipe_j
c Remove undefined vars
c
c Revision 1.4  1995/10/18  09:00:02  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/09/22  10:03:13  kuipe_j
c variable dimensions, new headers
c
c Revision 1.2  1995/05/30  07:04:52  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:20  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:32:50  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:08  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer   igp,
     +          k,
     +          maxlev,
     +          ngrid,
     +          nlev (ngrid)

      real      wft  (ngrid,maxlev),
     +          ws   (ngrid),
     +          wsact

c
c     Variables
c
      integer   i
c
      logical   epsequ
      external  epsequ
c
c     Determine transport width
c
      wsact = ws(igp)
c
c     Find highest bed level below wf
c
      k = 1
      do 100 i = 1, nlev(igp)
         if (wft(igp,i) .le. wsact) then
            k = i
         else
            goto 200
         endif
 100  continue
c
 200  continue
c
c     Now check for rectangular profile
c
      if (k .gt. 1) then
         if (epsequ ( wft(igp,k-1), wsact, 1.0E-4 )) then
            k = k - 1
         endif
      endif
c
      end
