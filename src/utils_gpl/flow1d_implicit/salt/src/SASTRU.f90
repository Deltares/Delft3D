subroutine sastru (ngrid  ,ngridm ,il     ,ir     ,j      ,istr  ,&
&q2     ,af     ,disgr  ,salstr ,strclo ,strhis,&
&aa     ,ba     ,da     ,ea     ,fd     ,gd    ,&
&md     ,nd     ,ra     ,rd     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SASTRU (SAlt a,b,d,e-etc. coef. cal. STRUctures)
!
! Module description: Calculate the A,B,D,E-etc coefficients for a
!                     structure cell between grid points Il and Ir
!                     (=Il+1).
!
!                     For a closed structure the coefficients are zero
!                     or one. For an open structure the coefficients
!                     GAMMA-s,l and GAMMA-s,r must be calculated first.
!                     [ Doc. S-DO-002.1SW / SASTRU ]
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 12 aa(ngridm)        O  a-coefficient [cs(i)] in diff.-advection equa-
!                         tion for every grid point in a branch.
!  8 af(ngrid)         I  Flow area at every grid point at time t(n+1)
! 13 ba(ngridm)        O  b-coefficient [c (i)] in diff.-advection equa-
!                         tion for every grid point in a branch.
! 14 da(ngridm)        O  d-coefficient [cs(i+1)] in diff.-advection
!                         equation for every grid point in a branch.
!  9 disgr(ngrid)      I  Dispersion coefficient in every grid point at
!                         time t(n+1).
! 15 ea(ngridm)        O  e-coefficient [c s(i+1)] in diff.-advection
!                         equation for every grid point in a branch.
! 16 fd(ngridm)        IO f-coefficient [cs(i)] in diffusion equation
!                         for every grid point in a branch.
! 17 gd(ngridm)        O  g-coefficient [c (i)] in diffusion equation
!                         for every grid point in a branch.
!  3 il                I  Grid point on left side of structure (lower
!                         index).
!  4 ir                I  Grid point on right side of structure (upper
!                         index).
!  6 istr              I  Number of structure.
!  5 j                 I  Index in arrays a, b etc. to store to be cal-
!                         culated coefficients.
! 18 md(ngridm)        O  m-coefficient [cs(i+1)] in diffusion equation
!                         for every grid point in a branch.
! 19 nd(ngridm)        O  n-coefficient [c s(i+1)] in diffusion equation
!                         for every grid point in a branch.
!  1 ngrid             I  Number of grid points in network.
!  2 ngridm            I  Maximum number of gridpoints in a branch.
!  7 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
! 20 ra(ngridm)        O  Right-hand-side of advection-diffusion equa-
!                         tion for every grid point in a branch.
! 21 rd(ngridm)        O  Right-hand-side of diffusion equation for
!                         every grid point in a branch.
! 10 salstr(7,nstru)   I  Dispersion information for each structure:
!                         (1,i) = Option for dispersion:
!                                 cdsequ (1) : Equal salt concentrations
!                                              on both sides of strctrs.
!                                 cdsdf1 (2) : Different salt conctrns.
!                                 cdsdf2 (3) : Different salt conctrns,
!                                              with simplified input.
!                         For option 2, different salt concentrations,
!                         the following additional information will be
!                         stored:
!                         (2,i) = Effective structure length Ll.
!                         (3,i) = Effective structure length Lr.
!                         (4,i) = Coefficient GAMMA1,l for Q > 0.
!                         (5,i) = Coefficient GAMMA2,l for Q > 0.
!                         (6,i) = Coefficient GAMMA1,r for Q < 0.
!                         (7,i) = Coefficient GAMMA2,r for Q < 0.
!                         For option 3, simplified input, the following
!                         information will be stored:
!                         (2,i) = Coefficient GAMMA-s,l (dimension
!                                 length) for Q > 0.
!                         (3,i) = Coefficient GAMMA-s,r (dimension
!                                 length) for Q < 0.
! 11 strclo(nstru)     I  True if structure is closed.
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sastru.pf,v $
! Revision 1.7  1997/11/26  14:44:49  kuipe_j
! diffusion zero for free flow
!
! Revision 1.6  1997/02/17  10:27:49  kuipe_j
! Lateral  Q  in m3/s in cont equation
!
! Revision 1.5  1995/10/18  09:00:31  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.4  1995/08/23  14:29:45  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:56:16  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:06:17  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:58  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:07  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:15  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   integer    ngrid  ,ngridm ,il    ,ir     ,j      ,istr
   real       disgr(ngrid)  ,af   (ngrid)   ,&
   &salstr(7,*)    ,strhis(dmstrh,*)
   double precision&
   &q2    (ngrid ) ,&
   &aa    (ngridm) ,ba   (ngridm) ,da    (ngridm) ,&
   &ea    (ngridm) ,fd   (ngridm) ,gd    (ngridm) ,&
   &md    (ngridm) ,nd   (ngridm) ,ra    (ngridm) ,&
   &rd    (ngridm)
   logical    strclo(*)
!
!     Declaration of local variables
!
   integer    phase
   real       ll     ,lr     ,gam1   ,gam2  ,gamma  ,ladl   ,ladr   ,&
   &alf
!
!     At a grid point a set of parallel structures can exist.
!
!     Rules:
!     Drowned flow.
!         If one structure has open drowned flow the drowned flow
!     equation will be used. It does not matter of which structure
!     (any will do).
!     Closed.
!         If all structures are closed the close equation will be used.
!     Free flow.
!         Otherwise it is free flow (i.e. all open structures are free
!     flow), then the free flow equations are used.

!     Processing of a set continues until an open drowned structure
!     has been encountered (1 call per structure).

!     Coefficient fd will be used to determine if a previous structure
!     at this point was open or closed.
!     0 = closed or initialized.
!     1 = open.
!     Coefficient ba and ea  will be used to determine if a previous
!     structure at this point was drowned.
!     drowned: ba=-1 ea=1
!     free or closed or initialized: ba=0 or ea=0.
!
!     Phase to calculate cell
!     0 = all closed untill now
!     1 = all open structures free until now
!     2 = first drowned structure
!     3 = phase not altered, so skip
!
   if (ba(j)*ea(j) .gt. -.5d0) then
!        still not any drowned structure
      if (fd(j) .lt. .5d0) then
!           still all structures closed
         if (strclo(istr) .or. abs(q2(il)).lt.1.e-6) then
!              also current structure is closed
            phase = 0
         else
!              first open current structure encountered
            phase = 2 - mod(int(strhis(8,istr)),2)
         endif
      else
!           still all structures free
         if (strclo(istr) .or. abs(q2(il)).lt.1.e-6 .or.&
         &mod(int(strhis(8,istr)),2) .eq. 1 ) then
!              is current structure also free or closed?
            phase = 3
         else
!              No drowned
            phase = 2
         endif
      endif
   else
!        there are more the one drowned structures
      phase = 3
   endif

   if (phase .eq. 0) then
!
!        Until now all structures at this grid point are closed.
!        [ Doc. S-FO-001.5KV / Eq. 21-8 ]
!
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
!
!        Until now all open structures at this grid point have free
!        flow
!
!        Should be zero: Sweep fails so set to small value
      aa(j) = 0.00001d0
      da(j) = 0.d0
      if (q2(il) .gt. 0.) then
!           Positive discharge through structure.
         ba(j) = 1.d0
         ea(j) = 0.d0
      else
         ba(j) = 0.d0
         ea(j) = 1.d0
      endif
      fd(j) = 1.d0
      gd(j) = 0.d0
      md(j) = -1.d0
!        Should be zero: Sweep fails so set to small value
      nd(j) = 0.00001d0
      ra(j) = 0.d0
      rd(j) = 0.d0
   else if (phase .eq. 2) then
!
!           At least one structure at this grid point is open
!           drowned flow.
!           [ Doc. S-FO-001.5KV / Eq. 21-9 ]
!
      aa(j) = dble( q2(il) )
      ba(j) = -1.d0
      da(j) = dble(-q2(ir) )
      ea(j) = 1.d0
      ra(j) = 0.d0
      rd(j) = 0.d0
!
      if (int(salstr(1,istr)) .eq. cdsequ) then
!
!              Equal salt concentrations at both sides of structure.
!              [ Doc. S-FO-001.5KV / Eq. 21-10 ]
!
         gamma = 0.
      elseif (int(salstr(1,istr)) .eq. cdsdf1) then
!
!              Different salt concentrations at both sides of structure.
!              [ Doc. S-FO-001.5KV / Eq. 21-11 ]
!
         ll     = salstr(2,istr)
         lr     = salstr(3,istr)
         ladl   = ll / (af(il) * disgr(il))
         ladr   = lr / (af(ir) * disgr(ir))
!
         if (q2(il) .gt. 0.) then
!
!                 Left side (GAMMA s,l)
!
            gam1   = salstr(4,istr)
            gam2   = salstr(5,istr)
            alf    = (gam1-1.) * ladl + (gam2-1.) * ladr
            gamma  = - (1.-exp (alf*abs(q2(il)))) / q2(il)
         else
!
!                 Right side (GAMMA s,r)
!
            gam1   = salstr(6,istr)
            gam2   = salstr(7,istr)
            alf    = (gam1-1.) * ladl + (gam2-1.) * ladr
            gamma  = (1.-exp (alf*abs(q2(ir)))) / q2(ir)
         endif
      elseif (int(salstr(1,istr)) .eq. cdsdf2) then
!
!              Different salt concentrations at both sides of structure.
!              GAMMA left and right are specified by user ( > 0 )
!              [ Doc. S-FO-001.5KV / Eq. 21-5A ]
!
         if (q2(il) .gt. 0.) then
!
!                 Left side (GAMMA s,l)
!
            gamma  = salstr(2,istr) / (af(il) * disgr(il))
         else
!
!                 Right side (GAMMA s,r)
!
            gamma  = salstr(3,istr) / (af(ir) * disgr(ir))
         endif
      endif
!
      if (q2(il) .gt. 0.) then
!
!              Positive discharge through structure.
!

         fd(j) = 1.d0
         gd(j) = dble( gamma )
         md(j) = -1.d0
         nd(j) = 0.d0
      else
!
!              Negative discharge through structure.
!
         fd(j) = 1.d0
         gd(j) = 0.d0
         md(j) = -1.d0
         nd(j) = dble( gamma )
      endif
   endif
!
end
