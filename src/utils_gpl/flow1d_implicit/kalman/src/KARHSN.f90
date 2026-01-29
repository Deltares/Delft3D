subroutine KARHSN(ngrid  ,nstru  ,nnc    ,nnm    ,nns    ,nnf    ,&
&nnmu   ,nbran  ,nosdim ,scceql ,scmeql ,scqhsl ,&
&snceq  ,snmeq  ,snqhs  ,snfric ,&
&snmu   ,snwind ,sclceq ,sclmeq ,sclqhs ,sclfri ,&
&sclmu  ,scceq  ,scmeq  ,scqhs  ,scfric ,scmu   ,&
&strtyp ,branch ,wfrict ,x      ,f2     ,m2     ,&
&w2     ,np     ,p      ,q      )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KARHSN (KAlman make Right Hand side for System Noise)
!
! Module description: Calculate right hande side matrix for system noise
!                     G Q G^t.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 28 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 31 f2(ngrid)         I  F2 coefficient of momentum equation
! 32 m2(ngrid)         I  M2 coefficient of Q-h relation
!  8 nbran             I  Number of branches.
!  1 ngrid             I  Number of grid points in network.
!  3 nnc               I  Number of uncorrelated random noise processes
!                         for the continity equation.
!  6 nnf               I  Number of uncertain bed friction parameters.
!  4 nnm               I  Number of uncorrelated random noise processes
!                         for the momentum equation.
!  7 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
!  5 nns               I  Number of uncorrelated random noise processes
!                         for the Q-H relations of structures.
! 34 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!  2 nstru             I  Number of structures.
! 35 p                 O  -
! 22 scceq(scceql)     I  Grid cell numbers of all uncorrelated r.n. pro-
!                         cesses for continuity equations. The grid cell
!                         numbers are grouped per process.
! 25 scfric(ngrid)     I  Grid cell numbers of all uncorrelated r.n. pro-
!                         cesses for bed friction. The grid cell numbers
!                         are grouped per process.
! 17 sclceq(nnc+1)     I  sclceq(i) points to begin of group i of grid
!                         cells (in array scceq) with correlated r.n.
!                         process for continuity equations.
! 20 sclfri(nnf+1)     I  sclfri(i) points to begin of group i of grid
!                         cells (in array scfric) with correlated r.n.
!                         process for bed friction.
! 18 sclmeq(nnm+1)     I  sclmeq(i) points to begin of group i of grid
!                         cells (in array scmeq) with correlated r.n.
!                         process for momentum equations.
! 21 sclmu(nnmu+1)     I  sclmu(i) points to begin of group i of struc-
!                         tures (in array scmu) with correlated r.n. pro-
!                         cess for free gate flow in general structures.
! 19 sclqhs(nns+1)     I  sclqhs(i) points to begin of group i of struc-
!                         tures (in array scqhs) with correlated r.n.
!                         process for Q-H relations of structures.
! 23 scmeq(scmeql)     I  Grid cell numbers of all uncorrelated r.n. pro-
!                         cesses for momentum equations. The grid cell
!                         numbers are grouped per process.
! 26 scmu(nstru)       I  Structure numbers of all uncorrelated r.n. pro-
!                         cesses for free gate flow in general structures.
!                         The structure numbers are grouped per process.
! 24 scqhs(scqhsl)     I  Structure numbers of all uncorrelated r.n. pro-
!                         cesses for Q-H relations of structures. The
!                         structure numbers are grouped per process.
! 11 snceq(nosdim,nnc) IO System noise in the hydrodynamic continuity
!                         equations for each group.
!                         (1,i) mean
!                         (2,i) deviation (input) or variance
!                         (3,i) indicator for absolute(0) or relative
!                               (1) noise variance
!                         (4,i) time correlation
!                         (5,i) current variance
! 14 snfric(2,nnf)     I  Mean and deviation (input) or variance of the
!                         noise for uncertain bed friction parameters for
!                         each group.
! 12 snmeq(nosdim,2) IO System noise in the hydrodynamic momentum equa-
!                         tions for each group.
!                         (1,i) mean
!                         (2,i) deviation (input) or variance
!                         (3,i) indicator for absolute(0)noise variance
!                         (4,i) time correlation
!                         (5,i) current variance
! 15 snmu(nnmu,2)      I  Mean and deviation (input) or variance of the
!                         noise for uncertain energy loss parameters for
!                         free gate flows for each group.
! 13 snqhs(nosdim,nns) IO Noise in the Q-h relations of structures for
!                         each group.
!                         (1,i) mean
!                         (2,i) deviation (input) or variance
!                         (3,i) indicator for absolute(0) or relative
!                               (1) noise variance
!                         (4,i) time correlation
!                         (5,i) current variance
! 16 snwind(2)         I  Mean and deviation (input) or variance of the
!                         noise for the uncertain wind stress parameter
! 27 strtyp(10,nstru)  I  Structure definitions:
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
! 33 w2(ngrid)         I  W2 coefficient of momentum equation
! 33 w2(ngrid)         I  Total width at (n+theta2) in every grid point.
! 29 wfrict(3,nbran)   I  Wind friction parameters in branch.
!                         (1,i) = Indicates wind defined for branch:
!                                 cnwndf (0) : No wind defined
!                                 cywndf (1) : Wind defined
!                         (2,i) = Table pointer for wind direction as a
!                                 function of time.
!                         (3,i) = Table pointer for wind velocity as a
!                                 function of time.
! 30 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: karhsn.pf,v $
! Revision 1.3  1999/03/15  15:52:12  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:05:22  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:55  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer          ngrid, nnc, nnm, nns, nnf, nnmu, nstru, nbran,&
   &np   , nosdim ,scceql ,scmeql ,scqhsl
   integer          sclceq(nnc+1), sclmeq(nnm+1), sclqhs(nns+1),&
   &sclfri(nnf+1), sclmu(nnmu+1)
   integer          scceq(scceql), scmeq(scmeql), scqhs(scqhsl),&
   &scfric(ngrid), scmu(nstru)
   integer          strtyp(10,nstru)
   integer          branch(4,nbran), wfrict(3,nbran)
!
   real             snceq(nosdim,nnc), snmeq(nosdim,nnm),&
   &snqhs(nosdim,nns),&
   &snfric(2,nnf), snmu(2,nnmu), snwind(2)
   real             p(np,np)
   real             x(ngrid)
!
   double precision f2(ngrid), m2(ngrid), w2(ngrid), q(ngrid)
!
!     Declaration of local variables
!
   integer          i, j, m, ind, ind1, istr, ibr, i1, i2
   real             var, scale
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Zeroes to matrix P.
!
   do 20 i = 1, np
      do 10 j = 1, np
         p(i,j) = 0.
10    continue
20 continue
!
!     Function:   Calculate r.h.s. = G * Q * G^t
!
!     Specify r.h.s. vectors in terms of E1_i and E2_i for each group
!     of GQG^T corresponding to H- or Q-covariances.
!
!     Continuity equation
!     if i and j belong to group m: E1_i = SNceq(2,m)
!
!     For each group m in GQG^T do
!
   do 40 m = 1,nnc
!        Add time correlation
      snceq(5,m) = snceq(4,m)**2 * snceq(5,m) + snceq(2,m)
!
      if (nosdim.le.2 .or. int(snceq(3,m)).eq.0) then
!           Absolute noise
         var = snceq(5,m)
      else
!           Relative noise
         scale = 0.
         do ind = sclceq(m), sclceq(m+1)-1
            i   = scceq(ind)
            scale = scale + q(i) - q(i+1)
         enddo
!           WRITE (*,*) 'Q-rest',m,scale
         var = scale**2 * snceq(5,m)
      endif
!
      do ind = sclceq(m), sclceq(m+1)-1
         j = scceq(ind)
         do ind1 = sclceq(m), sclceq(m+1)-1
            i       = scceq(ind1)
            p(i,j)  = p(i,j) +&
            &var*(x(i+1)-x(i))*(x(j+1)-x(j))
         enddo
      enddo
40 continue
!
!     Momentum equation
!     if i and j belong to group m: E2_i = SNmeq(2,m)
!
!     For each group m in GQG^T do
!
   do 60 m = 1,nnm
!        Add time correlation
      snmeq(5,m) = snmeq(4,m)**2 * snmeq(5,m) + snmeq(2,m)
!
      do ind = sclmeq(m), sclmeq(m+1)-1
         j = scmeq(ind)
         do ind1 = sclmeq(m), sclmeq(m+1)-1
            i                  = scmeq(ind1)
            p(ngrid+i,ngrid+j) = p(ngrid+i,ngrid+j) +&
            &snmeq(5,m)*(x(i+1)-x(i))*&
            &(x(j+1)-x(j))
         enddo
      enddo
60 continue
!
!     Q-H relation
!     if i and j belong to group m: E2_i = SNqhs(2,m)
!
!     For each group m in GQG^T do
!
   do 90 m = 1,nns
!        Add time correlation
      snqhs(5,m) = snqhs(4,m)**2 * snqhs(5,m) + snqhs(2,m)
!
      if (nosdim.le.2 .or. int(snqhs(3,m)).eq.0) then
!           Absolute noise
         var = snqhs(5,m)
      else
!           Relative noise
         scale = 0.
         do ind = sclqhs(m), sclqhs(m+1)-1
            istr = scqhs(ind)
            i    = strtyp(3,istr)
            scale = scale + q(i)**2
         enddo
         var = scale/(sclqhs(m+1)-sclqhs(m))**2 * snqhs(5,m)
!           WRITE (*,*) 'Q-stru',m,var
      endif
!
      do ind = sclqhs(m), sclqhs(m+1)-1
         istr = scqhs(ind)
         j    = strtyp(3,istr)
         do ind1= sclqhs(m), sclqhs(m+1)-1
            istr               = scqhs(ind1)
            i                  = strtyp(3,istr)
            p(ngrid+i,ngrid+j) = p(ngrid+i,ngrid+j) + var
         enddo
      enddo
90 continue
!
!     Specify r.h.s. vectors in terms of E1_i and E2_i for each column
!     of GQG^T corresponding to the covariances of parameters.
!
!     Bed friction
!     if i and j belong to group m: E2_i = -F2_i*SNfric(2,m)
!
   do 110 j = 2*ngrid + 1, 2*ngrid+nnf
      m = j-2*ngrid
      do 100 ind = sclfri(m), sclfri(m+1)-1
         i            = scfric(ind)
         p(ngrid+i,j) = -sngl(f2(i)) * snfric(2,m)
100   continue
110 continue
!
!     Free gate contraction
!     if i and j belong to group m: E2_i = -M2_i*SNmu(2,m)
!
   do 130 j = 2*ngrid+nnf+1, 2*ngrid+nnf+nnmu
      m = j-2*ngrid-nnf
      do 120 ind = sclmu(m), sclmu(m+1)-1
         istr         = scmu(ind)
         i            = strtyp(3,istr)
         p(ngrid+i,j) = -sngl(m2(i)) * snmu(2,m)
120   continue
130 continue
!
!     Wind
!     if i and j belong to group m: E2_i = -W2_i*SNwind(2)
!
   do 150 ibr = 1, nbran
      if (wfrict(1,ibr) .eq. cywndf) then
         i1 = branch (3,ibr)
         i2 = branch (4,ibr)
         do 140 i = i1, i2-1
            p(ngrid+i,np) = - sngl(w2(i)) * snwind(2)
140      continue
      endif
150 continue
!
!     Overwrite part of main diagonal with variances of correction
!     parameters.
!
   do 160 i = 1, nnf
      j = ngrid*2 + i
      p(j,j) = snfric(2,i)
160 continue
!
   do 170 i = 1, nnmu
      j = ngrid*2 + nnf + i
      p(j,j) = snmu(2,i)
170 continue
!
   p(np,np) = snwind(2)
!
end
