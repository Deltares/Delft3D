      subroutine sastru (ngrid  ,ngridm ,il     ,ir     ,j      ,istr  ,
     &                   q2     ,af     ,disgr  ,salstr ,strclo ,strhis,
     &                   aa     ,ba     ,da     ,ea     ,fd     ,gd    ,
     &                   md     ,nd     ,ra     ,rd     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SASTRU (SAlt a,b,d,e-etc. coef. cal. STRUctures)
c
c Module description: Calculate the A,B,D,E-etc coefficients for a
c                     structure cell between grid points Il and Ir
c                     (=Il+1).
c
c                     For a closed structure the coefficients are zero
c                     or one. For an open structure the coefficients
c                     GAMMA-s,l and GAMMA-s,r must be calculated first.
c                     [ Doc. S-DO-002.1SW / SASTRU ]
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 12 aa(ngridm)        O  a-coefficient [cs(i)] in diff.-advection equa-
c                         tion for every grid point in a branch.
c  8 af(ngrid)         I  Flow area at every grid point at time t(n+1)
c 13 ba(ngridm)        O  b-coefficient [c (i)] in diff.-advection equa-
c                         tion for every grid point in a branch.
c 14 da(ngridm)        O  d-coefficient [cs(i+1)] in diff.-advection
c                         equation for every grid point in a branch.
c  9 disgr(ngrid)      I  Dispersion coefficient in every grid point at
c                         time t(n+1).
c 15 ea(ngridm)        O  e-coefficient [c s(i+1)] in diff.-advection
c                         equation for every grid point in a branch.
c 16 fd(ngridm)        IO f-coefficient [cs(i)] in diffusion equation
c                         for every grid point in a branch.
c 17 gd(ngridm)        O  g-coefficient [c (i)] in diffusion equation
c                         for every grid point in a branch.
c  3 il                I  Grid point on left side of structure (lower
c                         index).
c  4 ir                I  Grid point on right side of structure (upper
c                         index).
c  6 istr              I  Number of structure.
c  5 j                 I  Index in arrays a, b etc. to store to be cal-
c                         culated coefficients.
c 18 md(ngridm)        O  m-coefficient [cs(i+1)] in diffusion equation
c                         for every grid point in a branch.
c 19 nd(ngridm)        O  n-coefficient [c s(i+1)] in diffusion equation
c                         for every grid point in a branch.
c  1 ngrid             I  Number of grid points in network.
c  2 ngridm            I  Maximum number of gridpoints in a branch.
c  7 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c 20 ra(ngridm)        O  Right-hand-side of advection-diffusion equa-
c                         tion for every grid point in a branch.
c 21 rd(ngridm)        O  Right-hand-side of diffusion equation for
c                         every grid point in a branch.
c 10 salstr(7,nstru)   I  Dispersion information for each structure:
c                         (1,i) = Option for dispersion:
c                                 cdsequ (1) : Equal salt concentrations
c                                              on both sides of strctrs.
c                                 cdsdf1 (2) : Different salt conctrns.
c                                 cdsdf2 (3) : Different salt conctrns,
c                                              with simplified input.
c                         For option 2, different salt concentrations,
c                         the following additional information will be
c                         stored:
c                         (2,i) = Effective structure length Ll.
c                         (3,i) = Effective structure length Lr.
c                         (4,i) = Coefficient GAMMA1,l for Q > 0.
c                         (5,i) = Coefficient GAMMA2,l for Q > 0.
c                         (6,i) = Coefficient GAMMA1,r for Q < 0.
c                         (7,i) = Coefficient GAMMA2,r for Q < 0.
c                         For option 3, simplified input, the following
c                         information will be stored:
c                         (2,i) = Coefficient GAMMA-s,l (dimension
c                                 length) for Q > 0.
c                         (3,i) = Coefficient GAMMA-s,r (dimension
c                                 length) for Q < 0.
c 11 strclo(nstru)     I  True if structure is closed.
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sastru.pf,v $
c Revision 1.7  1997/11/26  14:44:49  kuipe_j
c diffusion zero for free flow
c
c Revision 1.6  1997/02/17  10:27:49  kuipe_j
c Lateral  Q  in m3/s in cont equation
c
c Revision 1.5  1995/10/18  09:00:31  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.4  1995/08/23  14:29:45  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:56:16  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:06:17  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:58  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:07  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:15  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
      include '..\include\sobdim.i'
c
c     Declaration of parameters
c
      integer    ngrid  ,ngridm ,il    ,ir     ,j      ,istr
      real       disgr(ngrid)  ,af   (ngrid)   ,
     &           salstr(7,*)    ,strhis(dmstrh,*)
      double precision
     &           q2    (ngrid ) ,
     &           aa    (ngridm) ,ba   (ngridm) ,da    (ngridm) ,
     &           ea    (ngridm) ,fd   (ngridm) ,gd    (ngridm) ,
     &           md    (ngridm) ,nd   (ngridm) ,ra    (ngridm) ,
     &           rd    (ngridm)
      logical    strclo(*)
c
c     Declaration of local variables
c
      integer    phase
      real       ll     ,lr     ,gam1   ,gam2  ,gamma  ,ladl   ,ladr   ,
     &           alf
c
c     At a grid point a set of parallel structures can exist.
c     
c     Rules:
c     Drowned flow.
c         If one structure has open drowned flow the drowned flow
c     equation will be used. It does not matter of which structure
c     (any will do).
c     Closed.
c         If all structures are closed the close equation will be used.
c     Free flow.
c         Otherwise it is free flow (i.e. all open structures are free
c     flow), then the free flow equations are used.

c     Processing of a set continues until an open drowned structure
c     has been encountered (1 call per structure).

c     Coefficient fd will be used to determine if a previous structure
c     at this point was open or closed.
c     0 = closed or initialized.
c     1 = open.
c     Coefficient ba and ea  will be used to determine if a previous
c     structure at this point was drowned.        
c     drowned: ba=-1 ea=1 
c     free or closed or initialized: ba=0 or ea=0.
c
c     Phase to calculate cell
c     0 = all closed untill now
c     1 = all open structures free until now
c     2 = first drowned structure
c     3 = phase not altered, so skip
c
      if (ba(j)*ea(j) .gt. -.5d0) then
c        still not any drowned structure
         if (fd(j) .lt. .5d0) then
c           still all structures closed
            if (strclo(istr) .or. abs(q2(il)).lt.1.e-6) then
c              also current structure is closed
               phase = 0  
            else
c              first open current structure encountered
               phase = 2 - mod(int(strhis(8,istr)),2)
            endif
         else
c           still all structures free
            if (strclo(istr) .or. abs(q2(il)).lt.1.e-6 .or.
     +           mod(int(strhis(8,istr)),2) .eq. 1 ) then
c              is current structure also free or closed?
               phase = 3
            else
c              No drowned
               phase = 2
            endif
         endif
      else
c        there are more the one drowned structures
         phase = 3  
      endif
            
      if (phase .eq. 0) then
c
c        Until now all structures at this grid point are closed.
c        [ Doc. S-FO-001.5KV / Eq. 21-8 ]
c
         aa(j) = 0.d0
         ba(j) = 1.d0
         da(j) = 0.d0
         ea(j) = 0.d0
         fd(j) = 0.d0
         gd(j) = 0.d0
         md(j) = 0.d0
         nd(j) = 1.d0
         ra(j) = 0.d0
         rd(j) = 0.d0
      else if (phase .eq. 1) then
c
c        Until now all open structures at this grid point have free
c        flow
c
c        Should be zero: Sweep fails so set to small value
         aa(j) = 0.00001d0
         da(j) = 0.d0
         if (q2(il) .gt. 0.) then
c           Positive discharge through structure.
            ba(j) = 1.d0
            ea(j) = 0.d0
         else
            ba(j) = 0.d0
            ea(j) = 1.d0
         endif
         fd(j) = 1.d0
         gd(j) = 0.d0
         md(j) = -1.d0
c        Should be zero: Sweep fails so set to small value
         nd(j) = 0.00001d0
         ra(j) = 0.d0
         rd(j) = 0.d0
      else if (phase .eq. 2) then
c
c           At least one structure at this grid point is open 
c           drowned flow.
c           [ Doc. S-FO-001.5KV / Eq. 21-9 ]
c
            aa(j) = dble( q2(il) )
            ba(j) = -1.d0
            da(j) = dble(-q2(ir) )
            ea(j) = 1.d0
            ra(j) = 0.d0
            rd(j) = 0.d0
c
            if (int(salstr(1,istr)) .eq. cdsequ) then
c
c              Equal salt concentrations at both sides of structure.
c              [ Doc. S-FO-001.5KV / Eq. 21-10 ]
c
               gamma = 0.
            elseif (int(salstr(1,istr)) .eq. cdsdf1) then
c
c              Different salt concentrations at both sides of structure.
c              [ Doc. S-FO-001.5KV / Eq. 21-11 ]
c
               ll     = salstr(2,istr)
               lr     = salstr(3,istr)
               ladl   = ll / (af(il) * disgr(il))
               ladr   = lr / (af(ir) * disgr(ir))
c
               if (q2(il) .gt. 0.) then
c
c                 Left side (GAMMA s,l)
c
                  gam1   = salstr(4,istr)
                  gam2   = salstr(5,istr)
                  alf    = (gam1-1.) * ladl + (gam2-1.) * ladr
                  gamma  = - (1.-exp (alf*abs(q2(il)))) / q2(il)
               else
c
c                 Right side (GAMMA s,r)
c
                  gam1   = salstr(6,istr)
                  gam2   = salstr(7,istr)
                  alf    = (gam1-1.) * ladl + (gam2-1.) * ladr
                  gamma  = (1.-exp (alf*abs(q2(ir)))) / q2(ir)
               endif
            elseif (int(salstr(1,istr)) .eq. cdsdf2) then
c
c              Different salt concentrations at both sides of structure.
c              GAMMA left and right are specified by user ( > 0 )
c              [ Doc. S-FO-001.5KV / Eq. 21-5A ]
c
               if (q2(il) .gt. 0.) then
c
c                 Left side (GAMMA s,l)
c
                  gamma  = salstr(2,istr) / (af(il) * disgr(il))
               else
c
c                 Right side (GAMMA s,r)
c
                  gamma  = salstr(3,istr) / (af(ir) * disgr(ir))
               endif
            endif
c
            if (q2(il) .gt. 0.) then
c
c              Positive discharge through structure.
c

               fd(j) = 1.d0
               gd(j) = dble( gamma )
               md(j) = -1.d0
               nd(j) = 0.d0
            else
c
c              Negative discharge through structure.
c
               fd(j) = 1.d0
               gd(j) = 0.d0
               md(j) = -1.d0
               nd(j) = dble( gamma )
            endif
      endif
c
      end
