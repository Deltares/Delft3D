      subroutine KAINSN(ngrid  ,nstru  ,nnc    ,nnm    ,nns    ,nnn    ,
     +                  nnf    ,nnmu   ,nsamp  ,nosdim ,scifri ,scimu  ,
     +                  snceq  ,snmeq  ,snqhs  ,snnode ,snfric ,snmu   ,
     +                  snwind ,sclceq ,sclmeq ,sclqhs ,sclnod ,sclfri ,
     +                  sclmu  ,scfric ,scmu   ,smpns  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAINSN (KAlman INitialse System noise)
c
c Module description: Perform some initial work to ease the use of the
c                     sysem noise.
c
c                     At input the system and measurement noise are
c                     expressed in mean and deviation. The deviations
c                     are transformed to variances. Rearange the infor-
c                     mation with respect to the spatial correlation.
c                     -    Transform the arrays SCL.. from 'numbers' to
c                          'pointers'.
c                     -    Make the 'inverse' arrays SCi.. .
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 ngrid             I  Number of grid points in network.
c  3 nnc               I  Number of uncorrelated random noise processes
c                         for the continity equation.
c  7 nnf               I  Number of uncertain bed friction parameters.
c  4 nnm               I  Number of uncorrelated random noise processes
c                         for the momentum equation.
c  8 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c  6 nnn               I  Number of uncorrelated random noise processes
c                         for nodal (= boundary) equations.
c  5 nns               I  Number of uncorrelated random noise processes
c                         for the Q-H relations of structures.
c  9 nsamp             I  Number of hydrodynamic samples (measurements)
c  2 nstru             I  Number of structures.
c 28 scceq(ngrid)      I  Grid cell numbers of all uncorrelated r.n. pro-
c                         cesses for continuity equations. The grid cell
c                         numbers are grouped per process.
c 31 scfric(ngrid)     I  Grid cell numbers of all uncorrelated r.n. pro-
c                         cesses for bed friction. The grid cell numbers
c                         are grouped per process.
c 13 scifri(ngrid)     O  Contains the number of the uncorrelated r.n.
c                         process for bed friction (group nr. or correc-
c                         tion parameter nr.) of every normal grid cell,
c                         otherwise zero.
c 14 scimu(nstru)      O  Contains the number of the uncorrelated r.n.
c                         process for free gate flow in general structures
c                         (group nr.) of every structure, otherwise zero.
c 22 sclceq(nnc+1)     IO sclceq(i) points to begin of group i of grid
c                         cells (in array scceq) with correlated r.n.
c                         process for continuity equations.
c 26 sclfri(nnf+1)     IO sclfri(i) points to begin of group i of grid
c                         cells (in array scfric) with correlated r.n.
c                         process for bed friction.
c 23 sclmeq(nnm+1)     IO sclmeq(i) points to begin of group i of grid
c                         cells (in array scmeq) with correlated r.n.
c                         process for momentum equations.
c 27 sclmu(nnmu+1)     IO sclmu(i) points to begin of group i of struc-
c                         tures (in array scmu) with correlated r.n. pro-
c                         cess for free gate flow in general structures.
c 25 sclnod(nnn+1)     IO sclnod(i) points to begin of group i of nodes
c                         (in array scnode) with correlated r.n. process
c                         for node equations.
c 24 sclqhs(nns+1)     IO sclqhs(i) points to begin of group i of struc-
c                         tures (in array scqhs) with correlated r.n.
c                         process for Q-H relations of structures.
c 29 scmeq(ngrid)      I  Grid cell numbers of all uncorrelated r.n. pro-
c                         cesses for momentum equations. The grid cell
c                         numbers are grouped per process.
c 32 scmu(nstru)       I  Structure numbers of all uncorrelated r.n. pro-
c                         cesses for free gate flow in general structures.
c                         The structure numbers are grouped per process.
c 30 scqhs(nstru)      I  Structure numbers of all uncorrelated r.n. pro-
c                         cesses for Q-H relations of structures. The
c                         structure numbers are grouped per process.
c 33 smpns(nsamp,2)    IO Mean and deviation (input) or variance of the
c                         measurement noise
c 15 snceq(nnc,nosdim) IO Mean, deviation (input) or variance,etc of
c                         system noise in the hydrodynamic continuity
c                         equations for each group.
c 19 snfric(nnf,2)     IO Mean and deviation (input) or variance of the
c                         noise for uncertain bed friction parameters for
c                         each group.
c 16 snmeq(nnm,nosdim) IO Mean, deviation (input) or variance,etc of
c                         system noise in the hydrodynamic momentum equa-
c                         tions for each group.
c 20 snmu(nnmu,2)      IO Mean and deviation (input) or variance of the
c                         noise for uncertain energy loss parameters for
c                         free gate flows for each group.
c 18 snnode(nnn,nosdim)IO Mean, deviation (input) or variance,etc of
c                         boundary noise for each group.
c 17 snqhs(nns,nosdim) IO Mean, deviation (input) or variance,etc of
c                         noise in the Q-h relations of structures for
c                         each group.
c 21 snwind(2)         IO Mean and deviation (input) or variance of the
c                         noise for the uncertain wind stress parameter
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kainsn.pf,v $
c Revision 1.3  1999/03/15  15:51:58  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:05:08  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:42  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c
c     Declaration of parameters
c
      integer          ngrid, nstru, nnc, nnm, nns, nnn, nnf, nnmu
      integer          nsamp, nosdim
      integer          scifri(ngrid), scimu(nstru)
      integer          sclceq(nnc+1), sclmeq(nnm+1), sclqhs(nns+1),
     +                 sclnod(nnn+1), sclfri(nnf+1), sclmu(nnmu+1)
      integer          scfric(ngrid), scmu(nstru)
c
      real             snceq(nosdim,nnc), snmeq(nosdim,nnm) ,
     +                 snqhs(nosdim,nns), snnode(nosdim,nnn),
     +                 snfric(2,nnf)    , snmu(2,nnmu),
     +                 snwind(2)        , smpns(nosdim,nsamp)
c
c     Declaration of local variables
c
      integer          i, j, m, ind
c
c     Transfer deviations to variances.
c
      do 10 i = 1, nnc
         snceq(2,i) = snceq(2,i) * snceq(2,i)
   10 continue
c
      do 20 i = 1, nnm
         snmeq(2,i) = snmeq(2,i) * snmeq(2,i)
   20 continue
c
      do 30 i = 1, nns
         snqhs(2,i) = snqhs(2,i) * snqhs(2,i)
   30 continue
c
      do 40 i = 1, nnn
         snnode(2,i) = snnode(2,i) * snnode(2,i)
   40 continue
c
      do 50 i = 1, nnf
         snfric(2,i) = snfric(2,i) * snfric(2,i)
   50 continue
c
      do 60 i =1, nnmu
         snmu(2,i) = snmu(2,i) * snmu(2,i)
   60 continue
c
      snwind(2) = snwind(2) * snwind(2)
c
      do 65 i = 1, nsamp
         smpns(2,i) = smpns(2,i) * smpns(2,i)
   65 continue
c
c     Transform the arrays scl... from 'numbers' to 'pointers'.
c
      sclceq(nnc+1) = 1
      do 70 i = 1, nnc
         sclceq(nnc+1) = sclceq(nnc+1) + sclceq(i)
   70 continue
c
      do 80 i = nnc, 1, -1
         sclceq(i) = sclceq(i+1) - sclceq(i)
   80 continue
c
      sclmeq(nnm+1) = 1
      do 90 i = 1, nnm
         sclmeq(nnm+1) = sclmeq(nnm+1) + sclmeq(i)
   90 continue
c
      do 100 i = nnm, 1, -1
         sclmeq(i) = sclmeq(i+1) - sclmeq(i)
  100 continue
c
      sclqhs(nns+1) = 1
      do 110 i = 1, nns
         sclqhs(nns+1) = sclqhs(nns+1) + sclqhs(i)
  110 continue
c
      do 120 i = nns, 1, -1
         sclqhs(i) = sclqhs(i+1) - sclqhs(i)
  120 continue
c
      sclnod(nnn+1) = 1
      do 130 i = 1, nnn
         sclnod(nnn+1) = sclnod(nnn+1) + sclnod(i)
  130 continue
c
      do 140 i = nnn, 1, -1
         sclnod(i) = sclnod(i+1) - sclnod(i)
  140 continue
c
      sclfri(nnf+1) = 1
      do 150 i = 1, nnf
         sclfri(nnf+1) = sclfri(nnf+1) + sclfri(i)
  150 continue
c
      do 160 i = nnf, 1, -1
         sclfri(i) = sclfri(i+1) - sclfri(i)
  160 continue
c
      sclmu(nnmu+1) = 1
      do 170 i = 1, nnmu
         sclmu(nnmu+1) = sclmu(nnmu+1) + sclmu(i)
  170 continue
c
      do 180 i = nnmu, 1, -1
         sclmu(i) = sclmu(i+1) - sclmu(i)
  180 continue
c
c     Make 'inverse' arrays with spatial correlation
c
      do 300 i = 1, ngrid
         scifri(i) = 0
         do 290 m = 1, nnf
            do 280 ind = sclfri(m), sclfri(m+1)-1
               j = scfric(ind)
               if (i .eq. j) then
                  scifri(i) = m
                  goto 300
               endif
  280       continue
  290    continue
  300 continue
c
      do 330 i = 1, nstru
         scimu(i) = 0
         do 320 m = 1, nnmu
            do 310 ind = sclmu(m), sclmu(m+1)-1
               j = scmu(ind)
               if (i .eq. j) then
                  scimu(i) = m
                  goto 330
               endif
  310       continue
  320    continue
  330 continue
c
      end
