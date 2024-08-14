      subroutine FLWIFR (nbran  ,ngrid  ,branch ,time   ,
     +                   maxtab ,ntabm  ,ntab   ,table  ,gangle ,
     +                   wndpar ,wfrict ,wshld  ,tauwi  ,dt1    ,theta2)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLWIFR (FLow WInd FRiction)
c
c Module description: In subroutine FLWIFR the wind friction term tauwi
c                     will be computed.
c
c                     Given the wind parameters
c
c                     -      density of air RHO-air
c                     -      wind coefficient Cwi
c                     -      wind coefficient tau-wi
c                     -      wind speed uwi and wind direction phi-wi
c                     -      orientation of the channel phi for each
c                            grid point
c
c                     the wind friction term follows from formulaes (5-
c                     12 and 5-13) in S-FO-001.5KV. The wind direction
c                     and wind speed can be defined differently for each
c                     branch.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 14 dt1               I  Time step.
c  9 gangle(ngrid)     I  Angle of each grid point (for wind).
c  5 maxtab            I  Maximum number of defined tables.
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  7 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
c                         to maxtab. For a specific table number k and
c                         function Y = f (X) the following definitions
c                         exist:
c                         (1,k) = Length of table k.
c                         (2,k) = Start address X in table.
c                         (3,k) = Start address Y in table.
c                         (4,k) = Access method and period control: xy
c                                 x = ctbnpf (0) : No period defined
c                                 x = ctbpfu (1) : Period defined
c                                 y = ctbico (0) : Continue interpltn
c                                 y = ctbidi (1) : Discrete interpltn
c  6 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  8 table             P  -
c 13 tauwi(ngrid)      O  Calculated wind friction for each gridpoint.
c 15 theta2            I  parameter for the time level t(n)+theta2*dt on
c                         which hydraulic parameters are to be evaluated
c  4 time              I  Actual time level tn+1. in sec.
c 11 wfrict(3,nbran)   I  Wind friction parameters in branch.
c                         (1,i) = Indicates wind defined for branch:
c                                 cnwndf (0) : No wind defined
c                                 cywndf (1) : Wind defined
c                         (2,i) = Table pointer for wind direction as a
c                                 function of time.
c                         (3,i) = Table pointer for wind velocity as a
c                                 function of time.
c 10 wndpar(3)         I  Wind parameters for whole model:
c                         (1) = ALPHA-wi,1
c                         (2) = ALPHA-wi,2
c                         (3) = RHO-air
c 12 wshld(ngrid)      I  Wind shielding factor for each grid point.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c inttab  INTerpolate in TABle
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flwifr.pf,v $
c Revision 1.5  1995/09/22  10:02:33  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/09/12  08:11:07  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.3  1994/11/28  08:37:58  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:31:50  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:57  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer maxtab, ntabm, ntab(4,maxtab),
     +        nbran, branch(4,nbran), wfrict(3,nbran),
     +        ngrid
      real    table(ntabm), gangle(ngrid), wndpar(3),
     +        wshld(ngrid), tauwi(ngrid),theta2
      double  precision     time,dt1
c
c     Declaration of local variables:
c
      integer          i, i1, i2, ibr, itab
      real             phiwbr, cwi, uwi, alpwi1, alpwi2, rhoair, phiwi
      double precision pi
      double precision timnew, timold, tnpth2
c
c     Include sobek constants
c
      include '../include/sobcon.i'
c
      pi = atan(1.D0) * 4.D0
c
      alpwi1 = wndpar(1)
      alpwi2 = wndpar(2)
      rhoair = wndpar(3)
c
c     Loop over branches
c
      do 100 ibr = 1, nbran
c
c        i1 = global grid point number at node n1 of branch
c        i2 = global grid point number at node n2 of branch
c
         i1 = branch (3,ibr)
         i2 = branch (4,ibr)
c
c       Wind defined for this branch
c
        if (wfrict(1,ibr) .eq. cywndf) then
c
c          Compute wind direction for branch at time level (n+theta2)
c
           timnew = time
           timold = time - dt1
           tnpth2 = dble (theta2*timnew + (1.-theta2)*timold)
           itab = wfrict(2,ibr)
            call INTTAB (ntab(1,itab), ntab(4,itab),
     +                   table(ntab(2,itab)),
     +                   table(ntab(3,itab)),
     +                   tnpth2, phiwbr )
c
c          Compute wind velocity for branch at time level (n+1/2)
c
           itab = wfrict(3,ibr)
            call INTTAB (ntab(1,itab), ntab(4,itab),
     +                   table(ntab(2,itab)),
     +                   table(ntab(3,itab)),
     +                   tnpth2, uwi )
c
           cwi = alpwi1 + alpwi2 * uwi
c
c           Loop over grid points in branch
c
           do 10 i = i1, i2
              phiwi = phiwbr - gangle(i)
              tauwi(i) = rhoair * cwi * wshld(i) * uwi * uwi *
     +                    real(cos(phiwi*pi/180.D0), kind=kind(tauwi))
   10      continue
c
c          No wind defined for this branch
c
        else
           do 20 i = i1, i2
              tauwi(i) = 0.
   20      continue
        endif

  100 continue
c
      end
