      subroutine FLBOUN(time   ,maxtab ,ntabm  ,ntab   ,table  ,nhstat ,
     +                  hstat  ,hbdpar ,nqstat ,qstat  ,qbdpar ,omboun ,
     +                  iter)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLBOUN (FLow BOUNdary conditions)
c
c Module description: In subroutine FLBOUN the boundary conditions htab
c                     and Qtab for the water flow will be computed for
c                     the actual time level tn+1.
c
c                     In subroutine FLBOUN for all water flow boundary
c                     stations the appropriate boundary conditions will
c                     be computed at time level tn+1. Depending on the
c                     boundary option the calculation is executed every
c                     iteration step or only at the first iteration
c                     step.
c
c                     1)     At the start of the computation of a new
c                     time step, iteration step 0.
c
c                     Here the boundary conditions for H and Q, imposed
c                     as time series, are computed if they are defined
c                     in time series. Values will be calculated by
c                     interpolation in the time tables.
c
c                     2)     At the start of a new iteration step.
c
c                     Here those boundary conditions will be computed
c                     which are given in h(Q)-tables or Q(h)-tables for
c                     h-boundaries resp. Q-boundaries. These evaluation
c                     will be done for the actual iteration dependent
c                     water flow.
c
c                     Remarks:
c
c                     The resulting boundary conditions htab and Qtab
c                     are to be applied in the boundary coefficients
c                     alfa, beta and gamma. (subroutine FLBNCO)
c
c                     If the hydrodynamic conditions has been defined as
c                     a water level as a function of a discharge the
c                     location does not have to be the boundary itself.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 hbdpar(3,nhstat)  I  Hydrodynamic conditions for H-stations:
c                         (1,i) = Location [grid point] for H-station.
c                         (2,i) = Type of condition
c                                 cbftim (1) : h = f(t)
c                                 cbfqoh (2) : h = h(Q)
c                                 cbfour (3) : h = fourier
c                                 cbtidl (4) : h = tidal components
c                         (3,i) = Table number for f(t), h(Q), fourier
c                                 or tidal components table.
c  7 hstat(nhstat)     O  Actual water level in every H-station.
c  2 maxtab            I  Maximum number of defined tables.
c  6 nhstat            I  Number of H-boundary stations.
c  9 nqstat            I  Number of Q-boundary stations.
c  4 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
c  3 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 12 omboun            I  Underrelaxation parameter omega for the
c                         boundary conditions
c 11 qbdpar(3,nqstat)  I  Hydrodynamic conditions for Q-stations:
c                         (1,i) = Location [grid point] for Q-station.
c                         (2,i) = Type of condition
c                                 cbftim (1) : Q = f(t)
c                                 cbfqoh (2) : Q = Q(h)
c                                 cbfour (3) : Q = fourier
c                                 cbtidl (4) : Q = tidal components
c                         (3,i) = Table number for f(t), Q(h), fourier
c                                 or tidal components table.
c 10 qstat(nqstat)     O  Actual discharge in every Q-station.
c  5 table             P  -
c  1 time              P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c epsequ  EQUal test with interval EPSilon
c inttab  INTerpolate in TABle
c series  SERIES of fourier or tidal component
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flboun.pf,v $
c Revision 1.5  1999/03/15  15:49:34  kuipe_j
c tabs removed
c
c Revision 1.4  1997/01/23  08:29:00  kuipe_j
c Make flow module robust
c
c Revision 1.3  1995/05/30  09:54:48  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:45  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:31  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:37:19  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:30:36  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:47  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c      !DEC$ IF DEFINED (_DLL)
c      use SobekRE_OpenMI
c      !DEC$ ENDIF
c
c     Declaration of parameters:
c
      integer maxtab, ntabm, nhstat, nqstat,
     +        ntab(4,maxtab), hbdpar(3,*), qbdpar(3,*)
      real    table(ntabm), hstat(*), qstat(nqstat), omboun
      double  precision     time
      integer iter
c
c     Declaration of local variables:
c
      integer iopt, istat, itab
      real    hbnd, qbnd
      logical EPSEQU
c
c     Include sobek constants
c
      include '../include/mempool.i'
      include '../include/sobcon.i'
c
c     ****************************************
c     * Computation of H-boundary conditions *
c     ****************************************
c
      do 10 istat = 1, nhstat
c
         iopt = hbdpar(2,istat)
         itab = hbdpar(3,istat)
c
c        type of boundary condition:
c
c        iopt = 1 : time series
c             = 2 : h(Q) table (processed in flbnco)
c             = 3 : serie of fourier
c             = 4 : serie of tidal components
c
         if (iopt .eq. cbftim) then
c
c           itab = TABLE number for h(t)-table
c           hbnd = h at boundary (from h(t)-table)
c
c           Interpolate h on n+1
c
            call INTTAB (ntab(1,itab), ntab(4,itab),
     +                   table(ntab(2,itab)),
     +                   table(ntab(3,itab)),
     +                   time , hbnd        )
c
            if (EPSEQU(omboun,0.0,1E-10)) then
               hstat(istat) = hbnd
            else
               hstat(istat) = omboun*hbnd+(1.-omboun)*hstat(istat)
            endif
c
         else if (iopt .eq. cbfour .or. iopt .eq. cbtidl) then
c
            call SERIES (time, iopt, table(ntab(2,itab)),
     +                   ntab(1,itab), hbnd )
c
            if (EPSEQU(omboun,0.0,1E-10)) then
               hstat(istat) = hbnd
            else
               hstat(istat) = omboun*hbnd+(1.-omboun)*hstat(istat)
            endif
c
         endif
c
   10 continue
c
c     ****************************************
c     * Computation of Q-boundary conditions *
c     ****************************************
c
      do 20 istat = 1, nqstat
c
         iopt = qbdpar(2,istat)
         itab = qbdpar(3,istat)
c
c        type of boundary condition:
c
c        iopt = 1 : time series
c             = 2 : Q(h) table (Processed in flbnco)
c             = 3 : serie of fourier
c             = 4 : serie of tidal components
c
         if (iopt .eq. cbftim) then
c
c           itab = TABLE number for Q(t)-table
c           qbnd = Q at boundary (from Q(t)-table)
c
            call INTTAB (ntab(1,itab), ntab(4,itab),
     +                   table(ntab(2,itab)),
     +                   table(ntab(3,itab)),
     +                   time , qbnd        )
c
            if (EPSEQU(omboun,0.0,1E-10)) then
               qstat(istat) = qbnd
            else
               qstat(istat) = omboun*qbnd+(1.-omboun)*qstat(istat)
            endif
c
         else if (iopt .eq. cbfour .or. iopt .eq. cbtidl) then
c
            call SERIES (time, iopt, table(ntab(2,itab)),
     +                   ntab(1,itab), qbnd           )
c
            if (EPSEQU(omboun,0.0,1E-10)) then
               qstat(istat) = qbnd
            else
               qstat(istat) = omboun*qbnd+(1.-omboun)*qstat(istat)

            endif
c
         endif
c
   20 continue
c
c Get openMI data for boundaries
c voorlopig overschrijven van de HStat en Qstat; dus geen rekening houden met onderrelaxatie
c Als onderrelaxatie gewenst is dan de call voorin de routine zetten, met HTemp en QTemp als tijdelijke arrays
c en later de onderrelaxatie inbouwen

c      !DEC$ IF DEFINED (_DLL)
c      if (OpenMIactive()) then
c         node   =     gtipnt ( 'NODE'  )
c         nodenm =     gtcpnt ( 'NODENM')
c         nnode  = ip (gtipnt ( 'NNODE' ))
c         ires = GetBoundaries(hstat, qstat, ip(node), cp(NodeNm),
c     +                        nhstat, nqstat, nnode)
c      endif
c      !DEC$ ENDIF

c
      Return
      end
