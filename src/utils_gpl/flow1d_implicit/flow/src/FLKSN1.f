      subroutine FLKSN1(ngrid  ,nstru  ,nnc    ,nnm    ,nns    ,nnf    ,
     +                  nnmu   ,nbran  ,nosdim ,scceql ,scmeql ,scqhsl ,
     +                  sclceq ,sclmeq ,sclqhs ,scceq  ,scmeq  ,scqhs  ,
     +                  scifri ,scimu  ,snceq  ,snmeq  ,snqhs  ,snfric ,
     +                  snmu   ,snwind ,strtyp ,branch ,wfrict ,v1     ,
     +                  v2     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLKSN1 (FLow Kalman add System Noise 1)
c
c Module description: Add mean of system noise to right hand side vector
c                     v1 and v2.
c
c                     The mean of system noise of all processes, except
c                     noise in nodes, is added.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 21 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
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
c  2 nstru             I  Number of structures.
c 12 scifri(ngrid)     I  Contains the number of the uncorrelated r.n.
c                         process for bed friction (group nr. or correc-
c                         tion parameter nr.) of every normal grid cell,
c                         otherwise zero.
c 13 scimu(nstru)      I  Contains the number of the uncorrelated r.n.
c                         process for free gate flow in general structures
c                         (group nr.) of every structure, otherwise zero.
c 14 snceq(nosdim,nnc) I  Mean and deviation (input) or variance of the
c                         system noise in the hydrodynamic continuity
c                         equations for each group.
c 17 snfric(2,nnf)     I  Mean ,deviation (input) or variance, etc. of
c                         noise for uncertain bed friction parameters for
c                         each group.
c 15 snmeq(nosdim,nnn) I  Mean, deviation (input) or variance, etc of
c                         system noise in the hydrodynamic momentum equa-
c                         tions for each group.
c 18 snmu(2,nnmu)      I  Mean and deviation (input) or variance of the
c                         noise for uncertain energy loss parameters for
c                         free gate flows for each group.
c 16 snqhs(nosdim,nns) I  Mean, deviation (input) or variance, etc of
c                         noise in the Q-h relations of structures for
c                         each group.
c 19 snwind(2)         I  Mean and deviation (input) or variance of the
c                         noise for the uncertain wind stress parameter
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
c 23 v1(ngrid)         IO Right-hand-sides (2*i-1) of set of branch
c                         equations. One value per grid point.
c 24 v2(ngrid)         IO Right-hand-sides (2*i) of set of branch equa-
c                         tions. One value per grid point.
c 22 wfrict(3,nbran)   I  Wind friction parameters in branch.
c                         (1,i) = Indicates wind defined for branch:
c                                 cnwndf (0) : No wind defined
c                                 cywndf (1) : Wind defined
c                         (2,i) = Table pointer for wind direction as a
c                                 function of time.
c                         (3,i) = Table pointer for wind velocity as a
c                                 function of time.
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flksn1.pf,v $
c Revision 1.3  1999/03/15  15:50:20  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:04:05  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:23:38  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c
c     Declaration of parameters
c
      integer          ngrid, nnc, nnm, nns, nnf, nnmu, nstru, nbran,
     +                 nosdim, scceql, scmeql, scqhsl
      integer          sclceq(nnc+1), sclmeq(nnm+1), sclqhs(nns+1),
     +                 scifri(ngrid), scimu(nstru) , scceq (scceql),
     +                 scmeq(scmeql), scqhs(scqhsl)
      integer          strtyp(10,nstru)
      integer          branch(4,nbran), wfrict(3,nbran)
c
      real             snceq(nosdim,nnc), snmeq(nosdim,nnm),
     +                 snqhs(nosdim,nns),
     +                 snfric(2,nnf), snmu(2,nnmu), snwind(2)
      double precision v1(ngrid), v2(ngrid)
c
c     Declaration of local variables
c
      integer          i, m, istru, ibr, i1, i2,ind
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Calculate r.h.s. = G * MUw and add to array v1 and v2.
c
c     Add system noise for continuity equation.
c
      do 10 m = 1,nnc
         do ind = sclceq(m), sclceq(m+1)-1
            i     = scceq(ind)
            v1(i) = v1(i) + snceq(1,m)
         enddo
   10 continue
c
c     Add system noise for momentum equation.
c
      do 20 m = 1,nnm
         do ind = sclmeq(m), sclmeq(m+1)-1
            i     = scmeq(ind)
            v2(i) = v2(i) + snmeq(1,m)
         enddo
   20 continue
c
c     Add system noise for structures.
c
      do 30 m = 1,nns
         do ind = sclqhs(m), sclqhs(m+1)-1
            istru = scqhs(ind)
            i     = strtyp(3,istru)
            v2(i) = v2(i) + snqhs(1,m)
         enddo
   30 continue
c
c     Add system noise for contraction in general structures.
c
      do 40 istru = 1, nstru
         m = scimu(istru)
         if ( m .gt. 0 ) then
            i = strtyp(3,istru)
            v2(i) = v2(i) + snmu(1,m)
         endif
   40 continue
c
c     Add system noise for bottom friction.
c
      do 50 i = 1, ngrid
         m = scifri(i)
         if ( m .gt. 0 ) then
            v2(i) = v2(i) + snfric(1,m)
         endif
   50 continue
c
c     Add system noise for wind.
c
      do 70 ibr = 1, nbran
         if (wfrict(1,ibr) .eq. cywndf) then
            i1 = branch (3,ibr)
            i2 = branch (4,ibr)
            do 60 i = i1, i2-1
               v2(i) = v2(i) + snwind(1)
   60       continue
         endif
   70 continue
c
      end
