subroutine KASTRU(i1     ,i2     ,g      ,nstru  ,strtyp ,strpar ,&
&ngrid  ,h2     ,h1     ,q2     ,af2    ,wf2    ,&
&maxtab ,ntabm  ,ntab   ,table  ,ngridm ,lsalt  ,&
&rho    ,strclo ,nnmu   ,pmua   ,scimu  ,lambda ,&
&dhstru ,a2     ,b2     ,c2     ,d2     ,m2     ,&
&ea2    ,eb2    ,ec2    ,ed2    ,em2    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KASTRU (KAlman abcde coefficients for STRUctures)
!
! Module description: Subroutine KASTRU calls the correct subroutine in
!                     order to compute the ABCDE coefficients for a
!                     specific structure.
!
!                     The set of ABCDE coefficients to be calculated are:
!                     A2-D2,M2 and AE2-AD2,EM2 for the stage-discharge
!                     equation.
!
!                     The ABCDE coefficients for the continuity equation
!                     are already computed in subroutine KANORM. The
!                     coefficients for the stage discharge equation are
!                     computed differently for each structure. In this
!                     routine the corresponding structure routine is
!                     selected to compute the A2.. coefficients.
!
!                     The following structures can be called:
!                     -    simple weir
!                     -    advanced weir
!                     -    general structure
!                     -    pump
!
!                     In the formulae for structures, described in
!                     S-FO-004 Chapter 7, it is assumed that x(i) is the
!                     upstream point. In case x(i+1) is the upstream
!                     point, the following transformation has to be
!                     applied for all types of structures and flows:
!
!                     Interchange:
!                       i <---> i+1
!                     A2i <---> C2i
!                     B2i <---> D2i
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 26 a2(ngridm)        IO A2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 11 af2               P  -
! 27 b2(ngridm)        IO B2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 28 c2(ngridm)        IO C2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 29 d2(ngridm)        IO D2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 25 dhstru            P  -
! 31 ea2(ngrid)        IO EA2 right hand side coefficient of momentum
! 32 eb2(ngrid)        IO EB2 right hand side coefficient of momentum
! 33 ec2(ngrid)        IO EC2 right hand side coefficient of momentum
! 34 ed2(ngrid)        IO ED2 right hand side coefficient of momentum
! 35 em2(ngrid)        IO EM2 right hand side coefficient of Q-h relation
!  3 g                 P  -
!  9 h1                P  -
!  8 h2                P  -
!  1 i1                I  Index of first grid point in actual branch.
!  2 i2                I  Index of last grid point in actual branch.
! 24 lambda            P  -
! 18 lsalt             P  -
! 30 m2(ngrid)         IO M2 coefficient of Q-h relation
! 13 maxtab            I  Maximum number of defined tables.
!  7 ngrid             I  Number of grid points in network.
! 17 ngridm            I  Maximum number of gridpoints in a branch.
! 21 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
!  4 nstru             I  Number of structures.
! 15 ntab              P  -
! 14 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 22 pmua(nnmu)        I  Uncertain energy loss parameters in case of
! 10 q2                P  -
! 19 rho               P  -
! 23 scimu(nstru)      I  Contains the number of the uncorrelated r.n.
!                         process for free gate flow in general structures
!                         (group nr.) of every structure, otherwise zero.
! 20 strclo            P  -
!  6 strpar            P  -
!  5 strtyp(10,nstru)  I  Structure definitions:
!                         (1,i) = Type of structure:
!                                 csweir (1) : Simple weir
!                                 caweir (2) : Advanced weir
!                                 csgate (3) : - not used, reserved for
!                                                simple gate -
!                                 cpump  (4) : Pump
!                                 cgenst (5) : General Structure
!                         (2,i) = Position of structure:
!                                 cstbra (1) : In branch
!                                 cstlat (2) : Lateral structure
!                         (3,i) = Left gridpoint.
!                         (4,i) = Right gridpoint.
!                         (5,i) = dummy
!                         (6,i) = dummy
!                         (7,i) = dummy
!                         (8,i) = dummy
!                         (9,i) = dummy
!                         (10,i)= dummy
! 16 table             P  -
! 12 wf2               P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! kaaw    KAlman Advanced Weir
! kags    KAlman General Structure
! kapp    KAlman structure PumP
! kasw    KAlman structure Simple Weir
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kastru.pf,v $
! Revision 1.7  1999/03/15  15:52:21  kuipe_j
! tabs removed
!
! Revision 1.6  1997/06/17  11:24:41  kuipe_j
! Initialize vars
!
! Revision 1.5  1997/05/05  14:31:28  kuipe_j
! Q and H coefficients are interchaged
!
! Revision 1.4  1996/12/05  10:00:07  kuipe_j
! Smoothing kgain,linearization,limit covariance,etc
!
! Revision 1.3  1996/09/03  14:54:29  kuipe_j
! frequency time hist,etc
!
! Revision 1.2  1996/04/12  13:05:29  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:25:03  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters:
!
   integer i1, i2, ngrid, ngridm, nstru, nnmu
   integer maxtab, ntabm, ntab(4,maxtab)
   integer strtyp(10,*), scimu(nstru)
   logical lsalt, strclo(*)
   real    lambda ,dhstru, g
   real    table(ntabm)
   real    strpar(dmstrpar,*), pmua(nnmu)
   real    af2(ngrid), wf2(ngrid)
   real    rho(ngrid)
!
   double precision h2(ngrid)  , h1(ngrid)  , q2(ngrid)
   double precision a2(ngridm) , b2(ngridm) , c2(ngridm),&
   &d2(ngridm) , m2(ngridm)
   double precision ea2(ngridm), eb2(ngridm), ec2(ngridm),&
   &ed2(ngridm), em2(ngridm)
!
!     Declaration of local variables:
!
   integer il, ir, istru, k
   real    teken, hulp, pmu
   real    a2s, b2s, c2s, d2s, m2s
   real    ea2s, eb2s, ec2s, ed2s, em2s
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Do for each structure in branch
!
   do 100 istru = 1, nstru
!
!        - left /right gridpoint -
!
      il = strtyp(3,istru)
      ir = strtyp(4,istru)
!
      if ( il .ge. i1 .and. il .le. i2 ) then
!
!           - structure is in this branch -
!
         if ( strtyp(2,istru) .eq. cstbra ) then
!
!              Normal structure in branch
!
            if      ( strtyp(1,istru) .eq. csweir ) then
!
!                 - simple weir -
!
               call KASW (g      ,il     ,ir     ,ngrid  ,istru  ,&
               &strclo ,strpar ,h2     ,h1     ,q2     ,&
               &af2    ,wf2    ,maxtab ,ntabm  ,ntab   ,&
               &table  ,rho    ,a2s    ,b2s    ,c2s    ,&
               &d2s    ,m2s    ,ea2s   ,eb2s   ,ec2s   ,&
               &ed2s   ,em2s   ,teken  )
!
            else if ( strtyp(1,istru) .eq. caweir ) then
!
!                 - advanced weir -
!
               call KAAW (g      ,il     ,ir     ,ngrid  ,istru  ,&
               &strclo ,strpar ,h2     ,h1     ,q2     ,&
               &af2    ,wf2    ,rho    ,a2s    ,b2s    ,&
               &c2s    ,d2s    ,m2s    ,ea2s   ,eb2s   ,&
               &ec2s   ,ed2s   ,em2s   ,teken  )
!
            else if ( strtyp(1,istru) .eq. cpump ) then
!
!                 - pump -
!
               call KAPP (il     ,ir     ,ngrid  ,istru  ,strclo ,&
               &strpar ,h2     ,maxtab ,ntabm  ,ntab   ,&
               &table  ,a2s    ,b2s    ,c2s    ,d2s    ,&
               &m2s    ,ea2s   ,eb2s   ,ec2s   ,ed2s   ,&
               &em2s   ,teken  )
!
            else if ( strtyp(1,istru) .eq. cgenst ) then
!
!                 - general structure -
!
!                 - determine correction parameter pmu in (Q,h)
!                   relation for free gate flow
!
               if ( scimu(istru) .eq. 0 ) then
                  pmu = 1.
               else
                  pmu = pmua(scimu(istru))
               endif
!
               call KAGS (g      ,il     ,ir     ,ngrid  ,istru  ,&
               &lambda ,dhstru ,strclo ,strpar ,h2     ,&
               &h1     ,q2     ,af2    ,wf2    ,lsalt  ,&
               &rho    ,pmu    ,a2s    ,b2s    ,c2s    ,&
               &d2s    ,m2s    ,ea2s   ,eb2s   ,ec2s   ,&
               &ed2s   ,em2s   ,teken  )
            endif
!
!              Interchange if teken < 0
!
            if ( strtyp(1,istru) .ne. cpump .and. teken .lt. 0.) then
               hulp = a2s
               a2s  = c2s
               c2s  = hulp
               hulp = b2s
               b2s  = d2s
               d2s  = hulp
            endif
!
!              Compute index for a2....e2
!
            k = il - i1 + 1
!
!              Add matrix coefficients
!
            a2(k) = a2(k) + dble(a2s)
            b2(k) = b2(k) + dble(b2s)
            c2(k) = c2(k) + dble(c2s)
            d2(k) = d2(k) + dble(d2s)
            m2(k) = m2(k) + dble(m2s)
!
            ea2(k) = ea2(k) + dble(ea2s)
            eb2(k) = eb2(k) + dble(eb2s)
            ec2(k) = ec2(k) + dble(ec2s)
            ed2(k) = ed2(k) + dble(ed2s)
            em2(k) = em2(k) + dble(em2s)
!
         endif
      endif
100 continue
!
end
