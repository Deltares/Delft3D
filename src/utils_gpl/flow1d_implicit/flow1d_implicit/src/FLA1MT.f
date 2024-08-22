      subroutine FLA1MT(i1     ,i2     ,ngrid  ,h1     ,h      ,
     +                  maxlev ,nlev   ,hlev   ,
     +                  wft    ,af     ,izwft  ,a1m    ,theta2 )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLA1MT (FLow A1M Table)
c
c Module description: Compute first order momentum cross section A1m for
c                     each grid point in a table branch.
c
c                     For a table cross section the first order momentum
c                     will be calculated by interpolation. In case the
c                     water level is below the preissmann level the
c                     first order momemtum will be set to zero. This re-
c                     sults in a density term of zero (No influence of
c                     salt on flow).
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 13 a1m               O  parameter a1m
c 11 af(ngrid)         I  Flow area at every grid point at time t(n+1)
c  5 h1(ngrid)         I  Water level in every grid point at time t(n).
c  6 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c  9 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  1 i1                I  Index of first grid point in actual branch.
c  2 i2                I  Index of last grid point in actual branch.
c 12 izwft(ngrid,      I  Table containing integrated widths.
c       maxlev)
c  7 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  4 ngrid             I  Number of grid points in network.
c  8 nlev(ngrid)       I  Number of h-levels for every cross section.
c                         (For a circle cross section   : 1 ;
c                          For a sedredge cross section : 2 )
c 14 theta2            I  parameter for the time level t(n)+theta2*dt on
c                         which hydraulic parameters are to be evaluated
c 10 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
c                                 point i.
c                         - For a circle cross section:
c                         (i,1) = Radius of the circle.
c                         - For a sedredge cross section:
c                         (i,1) = Width of main section (i.e. left chan-
c                                 nel).
c                         (i,2) = Width of sub section 1 (i.e. right
c                                 channel).
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
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
c $Log: fla1mt.pf,v $
c Revision 1.8  1998/06/11  11:46:54  kuipe_j
c Estuary special integrated
c
c Revision 1.7  1997/09/30  09:25:23  kuipe_j
c density term improved for Preisman slot
c
c Revision 1.6  1997/01/23  08:28:52  kuipe_j
c Make flow module robust
c
c Revision 1.5  1995/09/22  10:00:43  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/09/12  08:10:41  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.2  1993/11/26  15:30:23  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:45  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Declaration of Parameters:
c
      integer   i1, i2, ngrid, maxlev
      integer   nlev(ngrid)
      double precision      hlev(ngrid,maxlev)
      double precision      h1(ngrid), h(ngrid)
      real      wft  (ngrid,maxlev), af (ngrid)
      real      izwft(ngrid,maxlev), a1m(ngrid)
      real      theta2
c
c     Declaration of local variables:
c
      integer  i, ilev, ntmax
      real     ht1, ht2, wf1, wf2, z, term1, term2,
     +         hmax, wfmax, izwf
	double precision wght, hint
c
c     Do for each gridpoint in branch
c
      do 100 i = i1, i2
c       hint = ( h1(i) + h(i) ) / 2.
        hint = dble( theta2*h(i) + (1.-theta2)*h1(i) )
        if ( hint .le. hlev(i,1) ) then
           a1m(i) = 0.
        else
c
c           Compute index (ilev) and weight factor (wght) w.r.t.
c           tables of widths.
c
           call INDWGH (ngrid  ,i      ,
     +                   maxlev ,nlev   ,hlev   ,
     +                   hint   ,ilev   ,wght   )
c
c           Determine actual flow width Wfz
c           [ Doc. S-FO-001.5KV / Eq. 5-19 ]
c
            if (ilev .gt. 0) then
c
               ht1 = real(hlev(i,ilev  ), kind=kind(ht1))
               ht2 = real(hlev(i,ilev+1), kind=kind(ht2))
               wf1 = wft (i,ilev  )
               wf2 = wft (i,ilev+1)
c
c              Evaluation of right hand side term of eq. (5-19) for
c              z = h(t1) and h*.    (excl. integral itself)
c
c              1. For z = h(t1)
c
               z     = ht1
               term1 = ( (z*z*z)/3.0 - (ht1 * z*z)/2.0 ) /(ht2 - ht1)
     +                                      * (wf2 - wf1) + z*z * wf1/2.
c
c              2. For z = h* ( =h(n+theta2) )
c
               z     = hint
               term2 = ( (z*z*z)/3.0 - (ht1 * z*z)/2.0 ) /(ht2 - ht1)
     +                                      * (wf2 - wf1) + z*z * wf1/2.
c
c              Evaluation of actual integral [z * Wf(z) * dz]
c              for z = h*
c
               izwf   = izwft(i,ilev) + (term2 - term1)

            else
c
c              table overflow
c              [ Doc. S-FO-001.5KV / Eq. 5-18 ]
c
               ntmax = nlev(i)
               hmax  = real(hlev(i,ntmax), kind=kind(hmax))
               wfmax = wft (i,ntmax)
               izwf  = izwft(i,ntmax) + wfmax * (hint*hint-hmax*hmax)/2.

            endif
c
c           Evaluation of actual coefficient A1m
c           [ Doc. S-FO-001.5KV / Eq. 5-17 ]
c
            a1m(i) = hint * af(i) - izwf
         endif
 100  continue
c
      end
