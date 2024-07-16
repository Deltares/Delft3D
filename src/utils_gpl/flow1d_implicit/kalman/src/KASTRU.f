      subroutine KASTRU(i1     ,i2     ,g      ,nstru  ,strtyp ,strpar ,
     +                  ngrid  ,h2     ,h1     ,q2     ,af2    ,wf2    ,
     +                  maxtab ,ntabm  ,ntab   ,table  ,ngridm ,lsalt  ,
     +                  rho    ,strclo ,nnmu   ,pmua   ,scimu  ,lambda ,
     +                  dhstru ,a2     ,b2     ,c2     ,d2     ,m2     ,
     +                  ea2    ,eb2    ,ec2    ,ed2    ,em2    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KASTRU (KAlman abcde coefficients for STRUctures)
c
c Module description: Subroutine KASTRU calls the correct subroutine in
c                     order to compute the ABCDE coefficients for a
c                     specific structure.
c
c                     The set of ABCDE coefficients to be calculated are:
c                     A2-D2,M2 and AE2-AD2,EM2 for the stage-discharge
c                     equation.
c
c                     The ABCDE coefficients for the continuity equation
c                     are already computed in subroutine KANORM. The
c                     coefficients for the stage discharge equation are
c                     computed differently for each structure. In this
c                     routine the corresponding structure routine is
c                     selected to compute the A2.. coefficients.
c
c                     The following structures can be called:
c                     -    simple weir
c                     -    advanced weir
c                     -    general structure
c                     -    pump
c
c                     In the formulae for structures, described in
c                     S-FO-004 Chapter 7, it is assumed that x(i) is the
c                     upstream point. In case x(i+1) is the upstream
c                     point, the following transformation has to be
c                     applied for all types of structures and flows:
c
c                     Interchange:
c                       i <---> i+1
c                     A2i <---> C2i
c                     B2i <---> D2i
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 26 a2(ngridm)        IO A2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 11 af2               P  -
c 27 b2(ngridm)        IO B2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 28 c2(ngridm)        IO C2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 29 d2(ngridm)        IO D2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 25 dhstru            P  -
c 31 ea2(ngrid)        IO EA2 right hand side coefficient of momentum
c 32 eb2(ngrid)        IO EB2 right hand side coefficient of momentum
c 33 ec2(ngrid)        IO EC2 right hand side coefficient of momentum
c 34 ed2(ngrid)        IO ED2 right hand side coefficient of momentum
c 35 em2(ngrid)        IO EM2 right hand side coefficient of Q-h relation
c  3 g                 P  -
c  9 h1                P  -
c  8 h2                P  -
c  1 i1                I  Index of first grid point in actual branch.
c  2 i2                I  Index of last grid point in actual branch.
c 24 lambda            P  -
c 18 lsalt             P  -
c 30 m2(ngrid)         IO M2 coefficient of Q-h relation
c 13 maxtab            I  Maximum number of defined tables.
c  7 ngrid             I  Number of grid points in network.
c 17 ngridm            I  Maximum number of gridpoints in a branch.
c 21 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c  4 nstru             I  Number of structures.
c 15 ntab              P  -
c 14 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 22 pmua(nnmu)        I  Uncertain energy loss parameters in case of
c 10 q2                P  -
c 19 rho               P  -
c 23 scimu(nstru)      I  Contains the number of the uncorrelated r.n.
c                         process for free gate flow in general structures
c                         (group nr.) of every structure, otherwise zero.
c 20 strclo            P  -
c  6 strpar            P  -
c  5 strtyp(10,nstru)  I  Structure definitions:
c                         (1,i) = Type of structure:
c                                 csweir (1) : Simple weir
c                                 caweir (2) : Advanced weir
c                                 csgate (3) : - not used, reserved for
c                                                simple gate -
c                                 cpump  (4) : Pump
c                                 cgenst (5) : General Structure
c                         (2,i) = Position of structure:
c                                 cstbra (1) : In branch
c                                 cstlat (2) : Lateral structure
c                         (3,i) = Left gridpoint.
c                         (4,i) = Right gridpoint.
c                         (5,i) = dummy
c                         (6,i) = dummy
c                         (7,i) = dummy
c                         (8,i) = dummy
c                         (9,i) = dummy
c                         (10,i)= dummy
c 16 table             P  -
c 12 wf2               P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c kaaw    KAlman Advanced Weir
c kags    KAlman General Structure
c kapp    KAlman structure PumP
c kasw    KAlman structure Simple Weir
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kastru.pf,v $
c Revision 1.7  1999/03/15  15:52:21  kuipe_j
c tabs removed
c
c Revision 1.6  1997/06/17  11:24:41  kuipe_j
c Initialize vars
c
c Revision 1.5  1997/05/05  14:31:28  kuipe_j
c Q and H coefficients are interchaged
c
c Revision 1.4  1996/12/05  10:00:07  kuipe_j
c Smoothing kgain,linearization,limit covariance,etc
c
c Revision 1.3  1996/09/03  14:54:29  kuipe_j
c frequency time hist,etc
c
c Revision 1.2  1996/04/12  13:05:29  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:25:03  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters:
c
      integer i1, i2, ngrid, ngridm, nstru, nnmu
      integer maxtab, ntabm, ntab(4,maxtab)
      integer strtyp(10,*), scimu(nstru)
      logical lsalt, strclo(*)
      real    lambda ,dhstru, g
      real    table(ntabm)
      real    strpar(dmstrpar,*), pmua(nnmu)
      real    af2(ngrid), wf2(ngrid)
      real    rho(ngrid)
c
      double precision h2(ngrid)  , h1(ngrid)  , q2(ngrid) 
      double precision a2(ngridm) , b2(ngridm) , c2(ngridm),
     +                 d2(ngridm) , m2(ngridm)
      double precision ea2(ngridm), eb2(ngridm), ec2(ngridm),
     +                 ed2(ngridm), em2(ngridm)
c
c     Declaration of local variables:
c
      integer il, ir, istru, k
      real    teken, hulp, pmu
      real    a2s, b2s, c2s, d2s, m2s
      real    ea2s, eb2s, ec2s, ed2s, em2s
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Do for each structure in branch
c
      do 100 istru = 1, nstru
c
c        - left /right gridpoint -
c
         il = strtyp(3,istru)
         ir = strtyp(4,istru)
c
         if ( il .ge. i1 .and. il .le. i2 ) then
c
c           - structure is in this branch -
c
            if ( strtyp(2,istru) .eq. cstbra ) then
c
c              Normal structure in branch
c
               if      ( strtyp(1,istru) .eq. csweir ) then
c
c                 - simple weir -
c
                  call KASW (g      ,il     ,ir     ,ngrid  ,istru  ,
     +                       strclo ,strpar ,h2     ,h1     ,q2     ,
     +                       af2    ,wf2    ,maxtab ,ntabm  ,ntab   ,
     +                       table  ,rho    ,a2s    ,b2s    ,c2s    ,
     +                       d2s    ,m2s    ,ea2s   ,eb2s   ,ec2s   ,
     +                       ed2s   ,em2s   ,teken  )
c
               else if ( strtyp(1,istru) .eq. caweir ) then
c
c                 - advanced weir -
c
                  call KAAW (g      ,il     ,ir     ,ngrid  ,istru  ,
     +                       strclo ,strpar ,h2     ,h1     ,q2     ,
     +                       af2    ,wf2    ,rho    ,a2s    ,b2s    ,
     +                       c2s    ,d2s    ,m2s    ,ea2s   ,eb2s   ,
     +                       ec2s   ,ed2s   ,em2s   ,teken  )
c
               else if ( strtyp(1,istru) .eq. cpump ) then
c
c                 - pump -
c
                  call KAPP (il     ,ir     ,ngrid  ,istru  ,strclo ,
     +                       strpar ,h2     ,maxtab ,ntabm  ,ntab   ,
     +                       table  ,a2s    ,b2s    ,c2s    ,d2s    ,
     +                       m2s    ,ea2s   ,eb2s   ,ec2s   ,ed2s   ,
     +                       em2s   ,teken  )
c
               else if ( strtyp(1,istru) .eq. cgenst ) then
c
c                 - general structure -
c
c                 - determine correction parameter pmu in (Q,h)
c                   relation for free gate flow
c
                  if ( scimu(istru) .eq. 0 ) then
                     pmu = 1.
                  else
                     pmu = pmua(scimu(istru))
                  endif
c
                  call KAGS (g      ,il     ,ir     ,ngrid  ,istru  ,
     +                       lambda ,dhstru ,strclo ,strpar ,h2     ,
     +                       h1     ,q2     ,af2    ,wf2    ,lsalt  ,
     +                       rho    ,pmu    ,a2s    ,b2s    ,c2s    ,
     +                       d2s    ,m2s    ,ea2s   ,eb2s   ,ec2s   ,
     +                       ed2s   ,em2s   ,teken  )
               endif
c
c              Interchange if teken < 0
c
               if ( strtyp(1,istru) .ne. cpump .and. teken .lt. 0.) then
                  hulp = a2s
                  a2s  = c2s
                  c2s  = hulp
                  hulp = b2s
                  b2s  = d2s
                  d2s  = hulp
               endif
c
c              Compute index for a2....e2
c
               k = il - i1 + 1
c
c              Add matrix coefficients
c
               a2(k) = a2(k) + dble(a2s)
               b2(k) = b2(k) + dble(b2s)
               c2(k) = c2(k) + dble(c2s)
               d2(k) = d2(k) + dble(d2s)
               m2(k) = m2(k) + dble(m2s)
c
               ea2(k) = ea2(k) + dble(ea2s)
               eb2(k) = eb2(k) + dble(eb2s)
               ec2(k) = ec2(k) + dble(ec2s)
               ed2(k) = ed2(k) + dble(ed2s)
               em2(k) = em2(k) + dble(em2s)
c
            endif
         endif
  100 continue
c
      end
