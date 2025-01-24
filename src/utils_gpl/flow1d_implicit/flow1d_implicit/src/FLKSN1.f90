subroutine FLKSN1(ngrid  ,nstru  ,nnc    ,nnm    ,nns    ,nnf    ,&
&nnmu   ,nbran  ,nosdim ,scceql ,scmeql ,scqhsl ,&
&sclceq ,sclmeq ,sclqhs ,scceq  ,scmeq  ,scqhs  ,&
&scifri ,scimu  ,snceq  ,snmeq  ,snqhs  ,snfric ,&
&snmu   ,snwind ,strtyp ,branch ,wfrict ,v1     ,&
&v2     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLKSN1 (FLow Kalman add System Noise 1)
!
! Module description: Add mean of system noise to right hand side vector
!                     v1 and v2.
!
!                     The mean of system noise of all processes, except
!                     noise in nodes, is added.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 21 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
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
!  2 nstru             I  Number of structures.
! 12 scifri(ngrid)     I  Contains the number of the uncorrelated r.n.
!                         process for bed friction (group nr. or correc-
!                         tion parameter nr.) of every normal grid cell,
!                         otherwise zero.
! 13 scimu(nstru)      I  Contains the number of the uncorrelated r.n.
!                         process for free gate flow in general structures
!                         (group nr.) of every structure, otherwise zero.
! 14 snceq(nosdim,nnc) I  Mean and deviation (input) or variance of the
!                         system noise in the hydrodynamic continuity
!                         equations for each group.
! 17 snfric(2,nnf)     I  Mean ,deviation (input) or variance, etc. of
!                         noise for uncertain bed friction parameters for
!                         each group.
! 15 snmeq(nosdim,nnn) I  Mean, deviation (input) or variance, etc of
!                         system noise in the hydrodynamic momentum equa-
!                         tions for each group.
! 18 snmu(2,nnmu)      I  Mean and deviation (input) or variance of the
!                         noise for uncertain energy loss parameters for
!                         free gate flows for each group.
! 16 snqhs(nosdim,nns) I  Mean, deviation (input) or variance, etc of
!                         noise in the Q-h relations of structures for
!                         each group.
! 19 snwind(2)         I  Mean and deviation (input) or variance of the
!                         noise for the uncertain wind stress parameter
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
! 23 v1(ngrid)         IO Right-hand-sides (2*i-1) of set of branch
!                         equations. One value per grid point.
! 24 v2(ngrid)         IO Right-hand-sides (2*i) of set of branch equa-
!                         tions. One value per grid point.
! 22 wfrict(3,nbran)   I  Wind friction parameters in branch.
!                         (1,i) = Indicates wind defined for branch:
!                                 cnwndf (0) : No wind defined
!                                 cywndf (1) : Wind defined
!                         (2,i) = Table pointer for wind direction as a
!                                 function of time.
!                         (3,i) = Table pointer for wind velocity as a
!                                 function of time.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flksn1.pf,v $
! Revision 1.3  1999/03/15  15:50:20  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:04:05  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:23:38  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!
!     Declaration of parameters
!
   integer          ngrid, nnc, nnm, nns, nnf, nnmu, nstru, nbran,&
   &nosdim, scceql, scmeql, scqhsl
   integer          sclceq(nnc+1), sclmeq(nnm+1), sclqhs(nns+1),&
   &scifri(ngrid), scimu(nstru) , scceq (scceql),&
   &scmeq(scmeql), scqhs(scqhsl)
   integer          strtyp(10,nstru)
   integer          branch(4,nbran), wfrict(3,nbran)
!
   real             snceq(nosdim,nnc), snmeq(nosdim,nnm),&
   &snqhs(nosdim,nns),&
   &snfric(2,nnf), snmu(2,nnmu), snwind(2)
   double precision v1(ngrid), v2(ngrid)
!
!     Declaration of local variables
!
   integer          i, m, istru, ibr, i1, i2,ind
!
!     Include sobek constants
!
   include '../include/sobcon.i'
!
!     Calculate r.h.s. = G * MUw and add to array v1 and v2.
!
!     Add system noise for continuity equation.
!
   do 10 m = 1,nnc
      do ind = sclceq(m), sclceq(m+1)-1
         i     = scceq(ind)
         v1(i) = v1(i) + snceq(1,m)
      enddo
10 continue
!
!     Add system noise for momentum equation.
!
   do 20 m = 1,nnm
      do ind = sclmeq(m), sclmeq(m+1)-1
         i     = scmeq(ind)
         v2(i) = v2(i) + snmeq(1,m)
      enddo
20 continue
!
!     Add system noise for structures.
!
   do 30 m = 1,nns
      do ind = sclqhs(m), sclqhs(m+1)-1
         istru = scqhs(ind)
         i     = strtyp(3,istru)
         v2(i) = v2(i) + snqhs(1,m)
      enddo
30 continue
!
!     Add system noise for contraction in general structures.
!
   do 40 istru = 1, nstru
      m = scimu(istru)
      if ( m .gt. 0 ) then
         i = strtyp(3,istru)
         v2(i) = v2(i) + snmu(1,m)
      endif
40 continue
!
!     Add system noise for bottom friction.
!
   do 50 i = 1, ngrid
      m = scifri(i)
      if ( m .gt. 0 ) then
         v2(i) = v2(i) + snfric(1,m)
      endif
50 continue
!
!     Add system noise for wind.
!
   do 70 ibr = 1, nbran
      if (wfrict(1,ibr) .eq. cywndf) then
         i1 = branch (3,ibr)
         i2 = branch (4,ibr)
         do 60 i = i1, i2-1
            v2(i) = v2(i) + snwind(1)
60       continue
      endif
70 continue
!
end
