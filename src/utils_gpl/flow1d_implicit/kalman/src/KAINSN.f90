subroutine KAINSN(ngrid  ,nstru  ,nnc    ,nnm    ,nns    ,nnn    ,&
&nnf    ,nnmu   ,nsamp  ,nosdim ,scifri ,scimu  ,&
&snceq  ,snmeq  ,snqhs  ,snnode ,snfric ,snmu   ,&
&snwind ,sclceq ,sclmeq ,sclqhs ,sclnod ,sclfri ,&
&sclmu  ,scfric ,scmu   ,smpns  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAINSN (KAlman INitialse System noise)
!
! Module description: Perform some initial work to ease the use of the
!                     sysem noise.
!
!                     At input the system and measurement noise are
!                     expressed in mean and deviation. The deviations
!                     are transformed to variances. Rearange the infor-
!                     mation with respect to the spatial correlation.
!                     -    Transform the arrays SCL.. from 'numbers' to
!                          'pointers'.
!                     -    Make the 'inverse' arrays SCi.. .
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 ngrid             I  Number of grid points in network.
!  3 nnc               I  Number of uncorrelated random noise processes
!                         for the continity equation.
!  7 nnf               I  Number of uncertain bed friction parameters.
!  4 nnm               I  Number of uncorrelated random noise processes
!                         for the momentum equation.
!  8 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
!  6 nnn               I  Number of uncorrelated random noise processes
!                         for nodal (= boundary) equations.
!  5 nns               I  Number of uncorrelated random noise processes
!                         for the Q-H relations of structures.
!  9 nsamp             I  Number of hydrodynamic samples (measurements)
!  2 nstru             I  Number of structures.
! 28 scceq(ngrid)      I  Grid cell numbers of all uncorrelated r.n. pro-
!                         cesses for continuity equations. The grid cell
!                         numbers are grouped per process.
! 31 scfric(ngrid)     I  Grid cell numbers of all uncorrelated r.n. pro-
!                         cesses for bed friction. The grid cell numbers
!                         are grouped per process.
! 13 scifri(ngrid)     O  Contains the number of the uncorrelated r.n.
!                         process for bed friction (group nr. or correc-
!                         tion parameter nr.) of every normal grid cell,
!                         otherwise zero.
! 14 scimu(nstru)      O  Contains the number of the uncorrelated r.n.
!                         process for free gate flow in general structures
!                         (group nr.) of every structure, otherwise zero.
! 22 sclceq(nnc+1)     IO sclceq(i) points to begin of group i of grid
!                         cells (in array scceq) with correlated r.n.
!                         process for continuity equations.
! 26 sclfri(nnf+1)     IO sclfri(i) points to begin of group i of grid
!                         cells (in array scfric) with correlated r.n.
!                         process for bed friction.
! 23 sclmeq(nnm+1)     IO sclmeq(i) points to begin of group i of grid
!                         cells (in array scmeq) with correlated r.n.
!                         process for momentum equations.
! 27 sclmu(nnmu+1)     IO sclmu(i) points to begin of group i of struc-
!                         tures (in array scmu) with correlated r.n. pro-
!                         cess for free gate flow in general structures.
! 25 sclnod(nnn+1)     IO sclnod(i) points to begin of group i of nodes
!                         (in array scnode) with correlated r.n. process
!                         for node equations.
! 24 sclqhs(nns+1)     IO sclqhs(i) points to begin of group i of struc-
!                         tures (in array scqhs) with correlated r.n.
!                         process for Q-H relations of structures.
! 29 scmeq(ngrid)      I  Grid cell numbers of all uncorrelated r.n. pro-
!                         cesses for momentum equations. The grid cell
!                         numbers are grouped per process.
! 32 scmu(nstru)       I  Structure numbers of all uncorrelated r.n. pro-
!                         cesses for free gate flow in general structures.
!                         The structure numbers are grouped per process.
! 30 scqhs(nstru)      I  Structure numbers of all uncorrelated r.n. pro-
!                         cesses for Q-H relations of structures. The
!                         structure numbers are grouped per process.
! 33 smpns(nsamp,2)    IO Mean and deviation (input) or variance of the
!                         measurement noise
! 15 snceq(nnc,nosdim) IO Mean, deviation (input) or variance,etc of
!                         system noise in the hydrodynamic continuity
!                         equations for each group.
! 19 snfric(nnf,2)     IO Mean and deviation (input) or variance of the
!                         noise for uncertain bed friction parameters for
!                         each group.
! 16 snmeq(nnm,nosdim) IO Mean, deviation (input) or variance,etc of
!                         system noise in the hydrodynamic momentum equa-
!                         tions for each group.
! 20 snmu(nnmu,2)      IO Mean and deviation (input) or variance of the
!                         noise for uncertain energy loss parameters for
!                         free gate flows for each group.
! 18 snnode(nnn,nosdim)IO Mean, deviation (input) or variance,etc of
!                         boundary noise for each group.
! 17 snqhs(nns,nosdim) IO Mean, deviation (input) or variance,etc of
!                         noise in the Q-h relations of structures for
!                         each group.
! 21 snwind(2)         IO Mean and deviation (input) or variance of the
!                         noise for the uncertain wind stress parameter
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kainsn.pf,v $
! Revision 1.3  1999/03/15  15:51:58  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:05:08  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:42  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!
!     Declaration of parameters
!
   integer          ngrid, nstru, nnc, nnm, nns, nnn, nnf, nnmu
   integer          nsamp, nosdim
   integer          scifri(ngrid), scimu(nstru)
   integer          sclceq(nnc+1), sclmeq(nnm+1), sclqhs(nns+1),&
   &sclnod(nnn+1), sclfri(nnf+1), sclmu(nnmu+1)
   integer          scfric(ngrid), scmu(nstru)
!
   real             snceq(nosdim,nnc), snmeq(nosdim,nnm) ,&
   &snqhs(nosdim,nns), snnode(nosdim,nnn),&
   &snfric(2,nnf)    , snmu(2,nnmu),&
   &snwind(2)        , smpns(nosdim,nsamp)
!
!     Declaration of local variables
!
   integer          i, j, m, ind
!
!     Transfer deviations to variances.
!
   do 10 i = 1, nnc
      snceq(2,i) = snceq(2,i) * snceq(2,i)
10 continue
!
   do 20 i = 1, nnm
      snmeq(2,i) = snmeq(2,i) * snmeq(2,i)
20 continue
!
   do 30 i = 1, nns
      snqhs(2,i) = snqhs(2,i) * snqhs(2,i)
30 continue
!
   do 40 i = 1, nnn
      snnode(2,i) = snnode(2,i) * snnode(2,i)
40 continue
!
   do 50 i = 1, nnf
      snfric(2,i) = snfric(2,i) * snfric(2,i)
50 continue
!
   do 60 i =1, nnmu
      snmu(2,i) = snmu(2,i) * snmu(2,i)
60 continue
!
   snwind(2) = snwind(2) * snwind(2)
!
   do 65 i = 1, nsamp
      smpns(2,i) = smpns(2,i) * smpns(2,i)
65 continue
!
!     Transform the arrays scl... from 'numbers' to 'pointers'.
!
   sclceq(nnc+1) = 1
   do 70 i = 1, nnc
      sclceq(nnc+1) = sclceq(nnc+1) + sclceq(i)
70 continue
!
   do 80 i = nnc, 1, -1
      sclceq(i) = sclceq(i+1) - sclceq(i)
80 continue
!
   sclmeq(nnm+1) = 1
   do 90 i = 1, nnm
      sclmeq(nnm+1) = sclmeq(nnm+1) + sclmeq(i)
90 continue
!
   do 100 i = nnm, 1, -1
      sclmeq(i) = sclmeq(i+1) - sclmeq(i)
100 continue
!
   sclqhs(nns+1) = 1
   do 110 i = 1, nns
      sclqhs(nns+1) = sclqhs(nns+1) + sclqhs(i)
110 continue
!
   do 120 i = nns, 1, -1
      sclqhs(i) = sclqhs(i+1) - sclqhs(i)
120 continue
!
   sclnod(nnn+1) = 1
   do 130 i = 1, nnn
      sclnod(nnn+1) = sclnod(nnn+1) + sclnod(i)
130 continue
!
   do 140 i = nnn, 1, -1
      sclnod(i) = sclnod(i+1) - sclnod(i)
140 continue
!
   sclfri(nnf+1) = 1
   do 150 i = 1, nnf
      sclfri(nnf+1) = sclfri(nnf+1) + sclfri(i)
150 continue
!
   do 160 i = nnf, 1, -1
      sclfri(i) = sclfri(i+1) - sclfri(i)
160 continue
!
   sclmu(nnmu+1) = 1
   do 170 i = 1, nnmu
      sclmu(nnmu+1) = sclmu(nnmu+1) + sclmu(i)
170 continue
!
   do 180 i = nnmu, 1, -1
      sclmu(i) = sclmu(i+1) - sclmu(i)
180 continue
!
!     Make 'inverse' arrays with spatial correlation
!
   do 300 i = 1, ngrid
      scifri(i) = 0
      do 290 m = 1, nnf
         do 280 ind = sclfri(m), sclfri(m+1)-1
            j = scfric(ind)
            if (i .eq. j) then
               scifri(i) = m
               goto 300
            endif
280      continue
290   continue
300 continue
!
   do 330 i = 1, nstru
      scimu(i) = 0
      do 320 m = 1, nnmu
         do 310 ind = sclmu(m), sclmu(m+1)-1
            j = scmu(ind)
            if (i .eq. j) then
               scimu(i) = m
               goto 330
            endif
310      continue
320   continue
330 continue
!
end
