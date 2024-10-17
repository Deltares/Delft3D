subroutine sadens (nbran ,ngrid ,juer  ,branch ,tw   ,csa2   ,&
&rho   ,ker   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SADENS (SAlt DENSity)
!
! Module description: Calculate water density, given the salt concentra-
!                     tion.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  6 csa2(ngrid)       I  Salt concentration in every grid point at time
!                         t(n+1).
!  3 juer              P  -
!  8 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  7 rho(ngrid)        O  Density of diluted water per grid point.
!  5 tw(nbran)         I  Temperature of water in branch.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sadens.pf,v $
! Revision 1.3  1995/05/30  09:56:05  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:05:56  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:37  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:33:27  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:12  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    nbran   ,ngrid ,juer  ,ker
   integer    branch(4,nbran)
   real       tw    (nbran)  ,csa2(ngrid) ,rho(ngrid)
!
!     Declaration of local parameters
!
   integer ibr ,igr ,i1  ,i2
   real    z1  ,z2  ,z3  ,t1  ,t2  ,t3  ,t4  ,tw2  ,dis
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
   do 20 ibr = 1,nbran
!
!        Calculation of quantities that are constant in a branch.
!        [ Doc. S-FO-001.5KV / Eq. 19-2 ]
!
      tw2 = tw(ibr)**2
      z1  = 5890.  + 38.   *tw(ibr) - .375  *tw2
      z2  = 1779.5 + 11.25 *tw(ibr) - .0745 *tw2
      z3  = 3.8    + .01   *tw(ibr)
!
!        [ Doc. S-FO-001.5KV / Eq. 19-5 ]
      t1  = 1000.*z1
      t2  = (z3-2.094) * 1000.
      t3  = (8376.*z1 + 12000.*z2) * 1000.
      t4  = 1. / (1.396*z1 + 2.*z2)
!
      i1 = branch(3,ibr)
      i2 = branch(4,ibr)
      do 10 igr = i1,i2
!
!           Calculate density in a grid point.
!           [ Doc. S-FO-001.5KV / Eq. 19-5 ]
!
         dis = (t2*csa2(igr)+t1)**2 + t3*csa2(igr)
         if (dis .lt. 0. ) then
            ker = fatal
            call error (juer,'SADENS',esaden,ker)
            goto 1000
         endif
         rho(igr) = (t1 +t2*csa2(igr) + sqrt(dis)) * t4
10    continue
20 continue
!
1000 continue
!
end
