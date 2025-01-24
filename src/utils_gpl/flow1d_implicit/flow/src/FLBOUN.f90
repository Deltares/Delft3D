subroutine FLBOUN(time   ,maxtab ,ntabm  ,ntab   ,table  ,nhstat ,&
&hstat  ,hbdpar ,nqstat ,qstat  ,qbdpar ,omboun ,&
&iter)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLBOUN (FLow BOUNdary conditions)
!
! Module description: In subroutine FLBOUN the boundary conditions htab
!                     and Qtab for the water flow will be computed for
!                     the actual time level tn+1.
!
!                     In subroutine FLBOUN for all water flow boundary
!                     stations the appropriate boundary conditions will
!                     be computed at time level tn+1. Depending on the
!                     boundary option the calculation is executed every
!                     iteration step or only at the first iteration
!                     step.
!
!                     1)     At the start of the computation of a new
!                     time step, iteration step 0.
!
!                     Here the boundary conditions for H and Q, imposed
!                     as time series, are computed if they are defined
!                     in time series. Values will be calculated by
!                     interpolation in the time tables.
!
!                     2)     At the start of a new iteration step.
!
!                     Here those boundary conditions will be computed
!                     which are given in h(Q)-tables or Q(h)-tables for
!                     h-boundaries resp. Q-boundaries. These evaluation
!                     will be done for the actual iteration dependent
!                     water flow.
!
!                     Remarks:
!
!                     The resulting boundary conditions htab and Qtab
!                     are to be applied in the boundary coefficients
!                     alfa, beta and gamma. (subroutine FLBNCO)
!
!                     If the hydrodynamic conditions has been defined as
!                     a water level as a function of a discharge the
!                     location does not have to be the boundary itself.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 hbdpar(3,nhstat)  I  Hydrodynamic conditions for H-stations:
!                         (1,i) = Location [grid point] for H-station.
!                         (2,i) = Type of condition
!                                 cbftim (1) : h = f(t)
!                                 cbfqoh (2) : h = h(Q)
!                                 cbfour (3) : h = fourier
!                                 cbtidl (4) : h = tidal components
!                         (3,i) = Table number for f(t), h(Q), fourier
!                                 or tidal components table.
!  7 hstat(nhstat)     O  Actual water level in every H-station.
!  2 maxtab            I  Maximum number of defined tables.
!  6 nhstat            I  Number of H-boundary stations.
!  9 nqstat            I  Number of Q-boundary stations.
!  4 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
!                         to maxtab. For a specific table number k and
!                         function Y = f (X) the following definitions
!                         exist:
!                         (1,k) = Length of table k.
!                         (2,k) = Start address X in table.
!                         (3,k) = Start address Y in table.
!                         (4,k) = Access method and period control: xy
!                                 x = ctbnpf (0) : No period defined
!                                 x = ctbpfu (1) : Period defined
!                                 y = ctbico (0) : Continue interpltn
!                                 y = ctbidi (1) : Discrete interpltn
!  3 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 12 omboun            I  Underrelaxation parameter omega for the
!                         boundary conditions
! 11 qbdpar(3,nqstat)  I  Hydrodynamic conditions for Q-stations:
!                         (1,i) = Location [grid point] for Q-station.
!                         (2,i) = Type of condition
!                                 cbftim (1) : Q = f(t)
!                                 cbfqoh (2) : Q = Q(h)
!                                 cbfour (3) : Q = fourier
!                                 cbtidl (4) : Q = tidal components
!                         (3,i) = Table number for f(t), Q(h), fourier
!                                 or tidal components table.
! 10 qstat(nqstat)     O  Actual discharge in every Q-station.
!  5 table             P  -
!  1 time              P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! epsequ  EQUal test with interval EPSilon
! inttab  INTerpolate in TABle
! series  SERIES of fourier or tidal component
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flboun.pf,v $
! Revision 1.5  1999/03/15  15:49:34  kuipe_j
! tabs removed
!
! Revision 1.4  1997/01/23  08:29:00  kuipe_j
! Make flow module robust
!
! Revision 1.3  1995/05/30  09:54:48  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:45  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:31  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:37:19  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:30:36  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:47  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   !DEC$ IF DEFINED (_DLL)
   use SobekRE_OpenMI
   !DEC$ ENDIF
!
!     Declaration of parameters:
!
   integer maxtab, ntabm, nhstat, nqstat,&
   &ntab(4,maxtab), hbdpar(3,*), qbdpar(3,*)
   real    table(ntabm), hstat(*), qstat(nqstat), omboun
   double  precision     time
   integer iter, ires
!
!     Declaration of local variables:
!
   integer iopt, istat, itab, node, nodenm, nnode
   real    hbnd, qbnd
   logical EPSEQU
!
!     External functions
!
   integer gtipnt, gtcpnt

!     Include sobek constants
!
   include '..\include\mempool.i'
   include '..\include\sobcon.i'
!
!     ****************************************
!     * Computation of H-boundary conditions *
!     ****************************************
!
   do 10 istat = 1, nhstat
!
      iopt = hbdpar(2,istat)
      itab = hbdpar(3,istat)
!
!        type of boundary condition:
!
!        iopt = 1 : time series
!             = 2 : h(Q) table (processed in flbnco)
!             = 3 : serie of fourier
!             = 4 : serie of tidal components
!
      if (iopt .eq. cbftim) then
!
!           itab = TABLE number for h(t)-table
!           hbnd = h at boundary (from h(t)-table)
!
!           Interpolate h on n+1
!
         call INTTAB (ntab(1,itab), ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)),&
         &time , hbnd        )
!
         if (EPSEQU(omboun,0.0,1E-10)) then
            hstat(istat) = hbnd
         else
            hstat(istat) = omboun*hbnd+(1.-omboun)*hstat(istat)
         endif
!
      else if (iopt .eq. cbfour .or. iopt .eq. cbtidl) then
!
         call SERIES (time, iopt, table(ntab(2,itab)),&
         &ntab(1,itab), hbnd )
!
         if (EPSEQU(omboun,0.0,1E-10)) then
            hstat(istat) = hbnd
         else
            hstat(istat) = omboun*hbnd+(1.-omboun)*hstat(istat)
         endif
!
      endif
!
10 continue
!
!     ****************************************
!     * Computation of Q-boundary conditions *
!     ****************************************
!
   do 20 istat = 1, nqstat
!
      iopt = qbdpar(2,istat)
      itab = qbdpar(3,istat)
!
!        type of boundary condition:
!
!        iopt = 1 : time series
!             = 2 : Q(h) table (Processed in flbnco)
!             = 3 : serie of fourier
!             = 4 : serie of tidal components
!
      if (iopt .eq. cbftim) then
!
!           itab = TABLE number for Q(t)-table
!           qbnd = Q at boundary (from Q(t)-table)
!
         call INTTAB (ntab(1,itab), ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)),&
         &time , qbnd        )
!
         if (EPSEQU(omboun,0.0,1E-10)) then
            qstat(istat) = qbnd
         else
            qstat(istat) = omboun*qbnd+(1.-omboun)*qstat(istat)
         endif
!
      else if (iopt .eq. cbfour .or. iopt .eq. cbtidl) then
!
         call SERIES (time, iopt, table(ntab(2,itab)),&
         &ntab(1,itab), qbnd           )
!
         if (EPSEQU(omboun,0.0,1E-10)) then
            qstat(istat) = qbnd
         else
            qstat(istat) = omboun*qbnd+(1.-omboun)*qstat(istat)

         endif
!
      endif
!
20 continue
!
! Get openMI data for boundaries
! voorlopig overschrijven van de HStat en Qstat; dus geen rekening houden met onderrelaxatie
! Als onderrelaxatie gewenst is dan de call voorin de routine zetten, met HTemp en QTemp als tijdelijke arrays
! en later de onderrelaxatie inbouwen

   !DEC$ IF DEFINED (_DLL)
   if (OpenMIactive()) then
      node   =     gtipnt ( 'NODE'  )
      nodenm =     gtcpnt ( 'NODENM')
      nnode  = ip (gtipnt ( 'NNODE' ))
      ires = GetBoundaries(hstat, qstat, ip(node), cp(NodeNm),&
      &nhstat, nqstat, nnode)
   endif
   !DEC$ ENDIF

!
   Return
end
