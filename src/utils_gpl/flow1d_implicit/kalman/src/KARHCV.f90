subroutine KARHCV(ngrid  ,ea1    ,eb1    ,ec1    ,ed1    ,ea2    ,&
&eb2    ,ec2    ,ed2    ,ef2    ,em2    ,ew2    ,&
&nnf    ,sclfri ,scfric ,nnmu   ,sclmu  ,&
&scmu   ,nstru  ,strtyp ,nbran  ,branch ,wfrict ,&
&np     ,p      ,pcol   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KARHCV (KAlman make Right Hand side CoVariance matrix)
!
! Module description: Calculate right hande side matrix for covariances
!                     B(.)Pp. Matrix P(n|n) will be overwritten by
!                     B(.)Pp.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 22 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  2 ea1(ngrid)        I  EA1 right hand side coefficient of continuity
!  6 ea2(ngrid)        I  EA2 right hand side coefficient of momentum
!  3 eb1(ngrid)        I  EB1 right hand side coefficient of continuity
!  7 eb2(ngrid)        I  EB2 right hand side coefficient of momentum
!  4 ec1(ngrid)        I  EC1 right hand side coefficient of continuity
!  8 ec2(ngrid)        I  EC2 right hand side coefficient of momentum
!  5 ed1(ngrid)        I  ED1 right hand side coefficient of continuity
!  9 ed2(ngrid)        I  ED2 right hand side coefficient of momentum
! 10 ef2(ngrid)        I  EF2 right hand side coefficient of momentum
! 11 em2(ngrid)        I  EM2 right hand side coefficient of Q-h relation
! 12 ew2(ngrid)        I  EW2 right hand side coefficient of momentum
! 21 nbran             I  Number of branches.
!  1 ngrid             I  Number of grid points in network.
! 13 nnf               I  Number of uncertain bed friction parameters.
! 16 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
! 24 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
! 19 nstru             I  Number of structures.
! 25 p                 IO -
! 26 pcol              IO -
! 15 scfric(ngrid)     I  Grid cell numbers of all uncorrelated r.n. pro-
!                         cesses for bed friction. The grid cell numbers
!                         are grouped per process.
! 14 sclfri(nnf+1)     I  sclfri(i) points to begin of group i of grid
!                         cells (in array scfric) with correlated r.n.
!                         process for bed friction.
! 17 sclmu(nnmu+1)     I  sclmu(i) points to begin of group i of struc-
!                         tures (in array scmu) with correlated r.n. pro-
!                         cess for free gate flow in general structures.
! 18 scmu(nstru)       I  Structure numbers of all uncorrelated r.n. pro-
!                         cesses for free gate flow in general structures.
!                         The structure numbers are grouped per process.
! 20 strtyp(10,nstru)  I  Structure definitions:
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
! 23 wfrict(3,nbran)   I  Wind friction parameters in branch.
!                         (1,i) = Indicates wind defined for branch:
!                                 cnwndf (0) : No wind defined
!                                 cywndf (1) : Wind defined
!                         (2,i) = Table pointer for wind direction as a
!                                 function of time.
!                         (3,i) = Table pointer for wind velocity as a
!                                 function of time.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: karhcv.pf,v $
! Revision 1.3  1999/03/15  15:52:10  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:05:20  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:53  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer          ngrid, nnf, nnmu, nstru, nbran, np
   integer          sclfri(nnf+1), scfric(ngrid)
   integer          sclmu(nnmu+1), scmu(nstru)
   integer          strtyp(10,nstru)
   integer          branch(4,nbran), wfrict(3,nbran)
   real             p(np,np)       , pcol(np)
   double precision ea1(ngrid), eb1(ngrid), ec1(ngrid),&
   &ed1(ngrid)
   double precision ea2(ngrid), eb2(ngrid), ec2(ngrid),&
   &ed2(ngrid), ef2(ngrid), em2(ngrid),&
   &ew2(ngrid)
!
!     Declaration of local variables
!
   integer          i, j, m, ind, istr, ibr, i1, i2
   double precision e1i, e2i
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Function:   Calculate r.h.s = B(.)*P(n|n)
!
!     For each column j = 1,..,2*ngrid+Nnf+Nnmu+1 in B(.)*P{n|n} do
!
   do 80 j = 1, np

!
!        Specify a r.h.s. vector in terms of E1_i and E2_i that equals
!        the j-th column of B(.)*P{n|n}
!
!        For each grid cell i do
!
      do 10 i = 1, ngrid-1

         e1i = ea1(i) * p(i  ,j) + eb1(i) * p(ngrid+i  ,j) +&
         &ec1(i) * p(i+1,j) + ed1(i) * p(ngrid+i+1,j)
!
         e2i = ea2(i) * p(i  ,j) + eb2(i) * p(ngrid+i  ,j) +&
         &ec2(i) * p(i+1,j) + ed2(i) * p(ngrid+i+1,j)
!
         pcol(i+1    ) = sngl(e1i)
         pcol(ngrid+i) = sngl(e2i)

10    continue
      pcol(1      ) = 0.0
      pcol(2*ngrid) = 0.0
!
!        Bed friction
!        if i and j belong to group m :
!        E2_i =  E2_i + EF2_i*P{2*ngrid+m ,j}
!
      do 30 m = 1, nnf
         do 20 ind = sclfri(m), sclfri(m+1)-1
            i             = scfric(ind)
            pcol(ngrid+i) = pcol(ngrid+i) +&
            &sngl(ef2(i)) * p(2*ngrid+m ,j)

20       continue
30    continue
!
!        Free gate contractions
!        if i and j belong to group m :
!        E2_i =  E2_i + EM2_i*P{2*ngrid+Nnf+m  ,j}
!
      do 50 m = 1, nnmu
         do 40 ind = sclmu(m), sclmu(m+1)-1
            istr          = scmu(ind)
            i             = strtyp(3,istr)
            pcol(ngrid+i) = pcol(ngrid+i) + sngl(em2(i)) *&
            &p(2*ngrid+nnf+m ,j)

40       continue
50    continue
!
!        Wind
!        if i and j belong to group m :
!        E2_i = E2_i + EW2_i*P{NP,j}
!
      do 70 ibr = 1, nbran
         if (wfrict(1,ibr) .eq. cywndf) then
            i1 = branch (3,ibr)
            i2 = branch (4,ibr)
            do 60 i = i1, i2-1
               pcol(ngrid+i) = pcol(ngrid+i) + sngl(ew2(i))*p(np ,j)

60          continue
         endif
70    continue
!
      do 90 i = 1,2*ngrid
         p(i,j) = pcol(i)
90    continue
!
80 continue
!
end
