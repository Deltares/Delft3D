      subroutine KARHSN(ngrid  ,nstru  ,nnc    ,nnm    ,nns    ,nnf    ,
     +                  nnmu   ,nbran  ,nosdim ,scceql ,scmeql ,scqhsl ,
     +                  snceq  ,snmeq  ,snqhs  ,snfric ,
     +                  snmu   ,snwind ,sclceq ,sclmeq ,sclqhs ,sclfri ,
     +                  sclmu  ,scceq  ,scmeq  ,scqhs  ,scfric ,scmu   ,
     +                  strtyp ,branch ,wfrict ,x      ,f2     ,m2     ,
     +                  w2     ,np     ,p      ,q      )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KARHSN (KAlman make Right Hand side for System Noise)
c
c Module description: Calculate right hande side matrix for system noise
c                     G Q G^t.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 28 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 31 f2(ngrid)         I  F2 coefficient of momentum equation
c 32 m2(ngrid)         I  M2 coefficient of Q-h relation
c  8 nbran             I  Number of branches.
c  1 ngrid             I  Number of grid points in network.
c  3 nnc               I  Number of uncorrelated random noise processes
c                         for the continity equation.
c  6 nnf               I  Number of uncertain bed friction parameters.
c  4 nnm               I  Number of uncorrelated random noise processes
c                         for the momentum equation.
c  7 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c  5 nns               I  Number of uncorrelated random noise processes
c                         for the Q-H relations of structures.
c 34 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c  2 nstru             I  Number of structures.
c 35 p                 O  -
c 22 scceq(scceql)     I  Grid cell numbers of all uncorrelated r.n. pro-
c                         cesses for continuity equations. The grid cell
c                         numbers are grouped per process.
c 25 scfric(ngrid)     I  Grid cell numbers of all uncorrelated r.n. pro-
c                         cesses for bed friction. The grid cell numbers
c                         are grouped per process.
c 17 sclceq(nnc+1)     I  sclceq(i) points to begin of group i of grid
c                         cells (in array scceq) with correlated r.n.
c                         process for continuity equations.
c 20 sclfri(nnf+1)     I  sclfri(i) points to begin of group i of grid
c                         cells (in array scfric) with correlated r.n.
c                         process for bed friction.
c 18 sclmeq(nnm+1)     I  sclmeq(i) points to begin of group i of grid
c                         cells (in array scmeq) with correlated r.n.
c                         process for momentum equations.
c 21 sclmu(nnmu+1)     I  sclmu(i) points to begin of group i of struc-
c                         tures (in array scmu) with correlated r.n. pro-
c                         cess for free gate flow in general structures.
c 19 sclqhs(nns+1)     I  sclqhs(i) points to begin of group i of struc-
c                         tures (in array scqhs) with correlated r.n.
c                         process for Q-H relations of structures.
c 23 scmeq(scmeql)     I  Grid cell numbers of all uncorrelated r.n. pro-
c                         cesses for momentum equations. The grid cell
c                         numbers are grouped per process.
c 26 scmu(nstru)       I  Structure numbers of all uncorrelated r.n. pro-
c                         cesses for free gate flow in general structures.
c                         The structure numbers are grouped per process.
c 24 scqhs(scqhsl)     I  Structure numbers of all uncorrelated r.n. pro-
c                         cesses for Q-H relations of structures. The
c                         structure numbers are grouped per process.
c 11 snceq(nosdim,nnc) IO System noise in the hydrodynamic continuity
c                         equations for each group.
c                         (1,i) mean
c                         (2,i) deviation (input) or variance
c                         (3,i) indicator for absolute(0) or relative
c                               (1) noise variance
c                         (4,i) time correlation
c                         (5,i) current variance
c 14 snfric(2,nnf)     I  Mean and deviation (input) or variance of the
c                         noise for uncertain bed friction parameters for
c                         each group.
c 12 snmeq(nosdim,2) IO System noise in the hydrodynamic momentum equa-
c                         tions for each group.
c                         (1,i) mean
c                         (2,i) deviation (input) or variance
c                         (3,i) indicator for absolute(0)noise variance
c                         (4,i) time correlation
c                         (5,i) current variance
c 15 snmu(nnmu,2)      I  Mean and deviation (input) or variance of the
c                         noise for uncertain energy loss parameters for
c                         free gate flows for each group.
c 13 snqhs(nosdim,nns) IO Noise in the Q-h relations of structures for
c                         each group.
c                         (1,i) mean
c                         (2,i) deviation (input) or variance
c                         (3,i) indicator for absolute(0) or relative
c                               (1) noise variance
c                         (4,i) time correlation
c                         (5,i) current variance
c 16 snwind(2)         I  Mean and deviation (input) or variance of the
c                         noise for the uncertain wind stress parameter
c 27 strtyp(10,nstru)  I  Structure definitions:
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
c 33 w2(ngrid)         I  W2 coefficient of momentum equation
c 33 w2(ngrid)         I  Total width at (n+theta2) in every grid point.
c 29 wfrict(3,nbran)   I  Wind friction parameters in branch.
c                         (1,i) = Indicates wind defined for branch:
c                                 cnwndf (0) : No wind defined
c                                 cywndf (1) : Wind defined
c                         (2,i) = Table pointer for wind direction as a
c                                 function of time.
c                         (3,i) = Table pointer for wind velocity as a
c                                 function of time.
c 30 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: karhsn.pf,v $
c Revision 1.3  1999/03/15  15:52:12  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:05:22  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:55  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer          ngrid, nnc, nnm, nns, nnf, nnmu, nstru, nbran,
     +                 np   , nosdim ,scceql ,scmeql ,scqhsl
      integer          sclceq(nnc+1), sclmeq(nnm+1), sclqhs(nns+1),
     +                 sclfri(nnf+1), sclmu(nnmu+1)
      integer          scceq(scceql), scmeq(scmeql), scqhs(scqhsl),
     +                 scfric(ngrid), scmu(nstru)
      integer          strtyp(10,nstru)
      integer          branch(4,nbran), wfrict(3,nbran)
c
      real             snceq(nosdim,nnc), snmeq(nosdim,nnm),
     +                 snqhs(nosdim,nns),
     +                 snfric(2,nnf), snmu(2,nnmu), snwind(2)
      real             p(np,np)
      real             x(ngrid)    
c
      double precision f2(ngrid), m2(ngrid), w2(ngrid), q(ngrid)
c
c     Declaration of local variables
c
      integer          i, j, m, ind, ind1, istr, ibr, i1, i2
      real             var, scale
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Zeroes to matrix P.
c
      do 20 i = 1, np
         do 10 j = 1, np
            p(i,j) = 0.
   10    continue
   20 continue
c
c     Function:   Calculate r.h.s. = G * Q * G^t
c
c     Specify r.h.s. vectors in terms of E1_i and E2_i for each group
c     of GQG^T corresponding to H- or Q-covariances.
c
c     Continuity equation
c     if i and j belong to group m: E1_i = SNceq(2,m)
c
c     For each group m in GQG^T do
c
      do 40 m = 1,nnc
c        Add time correlation
         snceq(5,m) = snceq(4,m)**2 * snceq(5,m) + snceq(2,m)
c
         if (nosdim.le.2 .or. int(snceq(3,m)).eq.0) then
c           Absolute noise
            var = snceq(5,m)
         else
c           Relative noise
            scale = 0.
            do ind = sclceq(m), sclceq(m+1)-1
               i   = scceq(ind)
               scale = scale + q(i) - q(i+1)
            enddo
c           WRITE (*,*) 'Q-rest',m,scale
            var = scale**2 * snceq(5,m)
         endif
c
         do ind = sclceq(m), sclceq(m+1)-1
            j = scceq(ind)
            do ind1 = sclceq(m), sclceq(m+1)-1
               i       = scceq(ind1)
               p(i,j)  = p(i,j) +
     &                   var*(x(i+1)-x(i))*(x(j+1)-x(j))
            enddo
         enddo
   40 continue
c
c     Momentum equation
c     if i and j belong to group m: E2_i = SNmeq(2,m)
c
c     For each group m in GQG^T do
c
      do 60 m = 1,nnm
c        Add time correlation
         snmeq(5,m) = snmeq(4,m)**2 * snmeq(5,m) + snmeq(2,m)
c
         do ind = sclmeq(m), sclmeq(m+1)-1
            j = scmeq(ind)
            do ind1 = sclmeq(m), sclmeq(m+1)-1
               i                  = scmeq(ind1)
               p(ngrid+i,ngrid+j) = p(ngrid+i,ngrid+j) +
     &                              snmeq(5,m)*(x(i+1)-x(i))*
     &                                         (x(j+1)-x(j))
            enddo
         enddo
   60 continue
c
c     Q-H relation
c     if i and j belong to group m: E2_i = SNqhs(2,m)
c
c     For each group m in GQG^T do
c
      do 90 m = 1,nns
c        Add time correlation
         snqhs(5,m) = snqhs(4,m)**2 * snqhs(5,m) + snqhs(2,m)
c
         if (nosdim.le.2 .or. int(snqhs(3,m)).eq.0) then
c           Absolute noise
            var = snqhs(5,m)
         else
c           Relative noise
            scale = 0.
            do ind = sclqhs(m), sclqhs(m+1)-1
               istr = scqhs(ind)
               i    = strtyp(3,istr)
               scale = scale + q(i)**2
            enddo
            var = scale/(sclqhs(m+1)-sclqhs(m))**2 * snqhs(5,m)
c           WRITE (*,*) 'Q-stru',m,var
         endif
c
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
c
c     Specify r.h.s. vectors in terms of E1_i and E2_i for each column
c     of GQG^T corresponding to the covariances of parameters.
c
c     Bed friction
c     if i and j belong to group m: E2_i = -F2_i*SNfric(2,m)
c
      do 110 j = 2*ngrid + 1, 2*ngrid+nnf
         m = j-2*ngrid
         do 100 ind = sclfri(m), sclfri(m+1)-1
            i            = scfric(ind)
            p(ngrid+i,j) = -sngl(f2(i)) * snfric(2,m)
  100    continue
  110 continue
c
c     Free gate contraction
c     if i and j belong to group m: E2_i = -M2_i*SNmu(2,m)
c
      do 130 j = 2*ngrid+nnf+1, 2*ngrid+nnf+nnmu
         m = j-2*ngrid-nnf
         do 120 ind = sclmu(m), sclmu(m+1)-1
            istr         = scmu(ind)
            i            = strtyp(3,istr)
            p(ngrid+i,j) = -sngl(m2(i)) * snmu(2,m)
  120    continue
  130 continue
c
c     Wind
c     if i and j belong to group m: E2_i = -W2_i*SNwind(2)
c
      do 150 ibr = 1, nbran
         if (wfrict(1,ibr) .eq. cywndf) then
            i1 = branch (3,ibr)
            i2 = branch (4,ibr)
            do 140 i = i1, i2-1
               p(ngrid+i,np) = - sngl(w2(i)) * snwind(2)
  140       continue
         endif
  150 continue
c
c     Overwrite part of main diagonal with variances of correction
c     parameters.
c
      do 160 i = 1, nnf
         j = ngrid*2 + i
         p(j,j) = snfric(2,i)
  160 continue
c
      do 170 i = 1, nnmu
         j = ngrid*2 + nnf + i
         p(j,j) = snmu(2,i)
  170 continue
c
      p(np,np) = snwind(2)
c
      end
