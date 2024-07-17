      subroutine KARHCV(ngrid  ,ea1    ,eb1    ,ec1    ,ed1    ,ea2    ,
     +                  eb2    ,ec2    ,ed2    ,ef2    ,em2    ,ew2    ,
     +                  nnf    ,sclfri ,scfric ,nnmu   ,sclmu  ,
     +                  scmu   ,nstru  ,strtyp ,nbran  ,branch ,wfrict ,
     +                  np     ,p      ,pcol   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KARHCV (KAlman make Right Hand side CoVariance matrix)
c
c Module description: Calculate right hande side matrix for covariances
c                     B(.)Pp. Matrix P(n|n) will be overwritten by
c                     B(.)Pp.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 22 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  2 ea1(ngrid)        I  EA1 right hand side coefficient of continuity
c  6 ea2(ngrid)        I  EA2 right hand side coefficient of momentum
c  3 eb1(ngrid)        I  EB1 right hand side coefficient of continuity
c  7 eb2(ngrid)        I  EB2 right hand side coefficient of momentum
c  4 ec1(ngrid)        I  EC1 right hand side coefficient of continuity
c  8 ec2(ngrid)        I  EC2 right hand side coefficient of momentum
c  5 ed1(ngrid)        I  ED1 right hand side coefficient of continuity
c  9 ed2(ngrid)        I  ED2 right hand side coefficient of momentum
c 10 ef2(ngrid)        I  EF2 right hand side coefficient of momentum
c 11 em2(ngrid)        I  EM2 right hand side coefficient of Q-h relation
c 12 ew2(ngrid)        I  EW2 right hand side coefficient of momentum
c 21 nbran             I  Number of branches.
c  1 ngrid             I  Number of grid points in network.
c 13 nnf               I  Number of uncertain bed friction parameters.
c 16 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c 24 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c 19 nstru             I  Number of structures.
c 25 p                 IO -
c 26 pcol              IO -
c 15 scfric(ngrid)     I  Grid cell numbers of all uncorrelated r.n. pro-
c                         cesses for bed friction. The grid cell numbers
c                         are grouped per process.
c 14 sclfri(nnf+1)     I  sclfri(i) points to begin of group i of grid
c                         cells (in array scfric) with correlated r.n.
c                         process for bed friction.
c 17 sclmu(nnmu+1)     I  sclmu(i) points to begin of group i of struc-
c                         tures (in array scmu) with correlated r.n. pro-
c                         cess for free gate flow in general structures.
c 18 scmu(nstru)       I  Structure numbers of all uncorrelated r.n. pro-
c                         cesses for free gate flow in general structures.
c                         The structure numbers are grouped per process.
c 20 strtyp(10,nstru)  I  Structure definitions:
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
c 23 wfrict(3,nbran)   I  Wind friction parameters in branch.
c                         (1,i) = Indicates wind defined for branch:
c                                 cnwndf (0) : No wind defined
c                                 cywndf (1) : Wind defined
c                         (2,i) = Table pointer for wind direction as a
c                                 function of time.
c                         (3,i) = Table pointer for wind velocity as a
c                                 function of time.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: karhcv.pf,v $
c Revision 1.3  1999/03/15  15:52:10  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:05:20  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:53  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer          ngrid, nnf, nnmu, nstru, nbran, np
      integer          sclfri(nnf+1), scfric(ngrid)
      integer          sclmu(nnmu+1), scmu(nstru)
      integer          strtyp(10,nstru)
      integer          branch(4,nbran), wfrict(3,nbran)
      real             p(np,np)       , pcol(np)
      double precision ea1(ngrid), eb1(ngrid), ec1(ngrid),
     +                 ed1(ngrid)
      double precision ea2(ngrid), eb2(ngrid), ec2(ngrid),
     +                 ed2(ngrid), ef2(ngrid), em2(ngrid),
     +                 ew2(ngrid)
c
c     Declaration of local variables
c
      integer          i, j, m, ind, istr, ibr, i1, i2
      double precision e1i, e2i
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Function:   Calculate r.h.s = B(.)*P(n|n)
c
c     For each column j = 1,..,2*ngrid+Nnf+Nnmu+1 in B(.)*P{n|n} do
c
      do 80 j = 1, np

c
c        Specify a r.h.s. vector in terms of E1_i and E2_i that equals
c        the j-th column of B(.)*P{n|n}
c
c        For each grid cell i do
c
         do 10 i = 1, ngrid-1

            e1i = ea1(i) * p(i  ,j) + eb1(i) * p(ngrid+i  ,j) +
     +            ec1(i) * p(i+1,j) + ed1(i) * p(ngrid+i+1,j)
c
            e2i = ea2(i) * p(i  ,j) + eb2(i) * p(ngrid+i  ,j) +
     +            ec2(i) * p(i+1,j) + ed2(i) * p(ngrid+i+1,j)
c
            pcol(i+1    ) = sngl(e1i)
            pcol(ngrid+i) = sngl(e2i)

   10    continue
         pcol(1      ) = 0.0
         pcol(2*ngrid) = 0.0
c
c        Bed friction
c        if i and j belong to group m :
c        E2_i =  E2_i + EF2_i*P{2*ngrid+m ,j}
c
         do 30 m = 1, nnf
            do 20 ind = sclfri(m), sclfri(m+1)-1
               i             = scfric(ind)
               pcol(ngrid+i) = pcol(ngrid+i) +
     +                         sngl(ef2(i)) * p(2*ngrid+m ,j)

   20       continue
   30    continue
c
c        Free gate contractions
c        if i and j belong to group m :
c        E2_i =  E2_i + EM2_i*P{2*ngrid+Nnf+m  ,j}
c
         do 50 m = 1, nnmu
            do 40 ind = sclmu(m), sclmu(m+1)-1
               istr          = scmu(ind)
               i             = strtyp(3,istr)
               pcol(ngrid+i) = pcol(ngrid+i) + sngl(em2(i)) *
     +                           p(2*ngrid+nnf+m ,j)

   40       continue
   50    continue
c
c        Wind
c        if i and j belong to group m :
c        E2_i = E2_i + EW2_i*P{NP,j}
c
         do 70 ibr = 1, nbran
            if (wfrict(1,ibr) .eq. cywndf) then
               i1 = branch (3,ibr)
               i2 = branch (4,ibr)
               do 60 i = i1, i2-1
                  pcol(ngrid+i) = pcol(ngrid+i) + sngl(ew2(i))*p(np ,j)

   60          continue
            endif
   70    continue
c
         do 90 i = 1,2*ngrid
            p(i,j) = pcol(i)
   90    continue
c
   80 continue
c
      end
