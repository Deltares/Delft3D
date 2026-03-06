subroutine FLKAIN(pfa    ,pmua   ,pw     ,scceq  ,scmeq  ,scqhs ,&
&scifri ,scimu  ,sclceq ,sclmeq ,sclqhs ,sclnod,&
&scnode ,lfilt  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 17 lfilt             O  = True if a filter step must be performed.
!  7 pfa(nnf)          O  Uncertain bed friction parameters of all
!  8 pmua(nnmu)        O  Uncertain energy loss parameters in case of
!  9 pw                O  Uncertain wind stress parameter.
! 12 scifri(ngrid)     O  Contains the number of the uncorrelated r.n.
!                         process for bed friction (group nr. or correc-
!                         tion parameter nr.) of every normal grid cell,
!                         otherwise zero.
! 13 scimu(nstru)      O  Contains the number of the uncorrelated r.n.
!                         process for free gate flow in general structures
!                         (group nr.) of every structure, otherwise zero.
! 15 sclnod(nnn+1)     O  sclnod(i) points to begin of group i of nodes
!                         (in array scnode) with correlated r.n. process
!                         for node equations.
! 16 scnode(*)         O  Node numbers of all uncorrelated r.n.
!                         processes
!                         for node equations. The node numbers are grouped
!                         per process.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flkain.pf,v $
! Revision 1.4  1999/03/15  15:50:16  kuipe_j
! tabs removed
!
! Revision 1.3  1996/12/02  10:04:54  kuipe_j
! avoid negative pointers
!
! Revision 1.2  1996/04/12  13:03:59  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:23:29  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!
!     Declaration of parameters
!
   logical          lfilt
   integer          scceq(*), scmeq(*) ,scqhs(*)  ,scnode(*),&
   &scimu(*), scifri(*),&
   &sclnod(*),sclceq(*),sclmeq(*) ,sclqhs(*)
   real             pfa(*)   ,pmua(*)  ,pw(1)
!
   lfilt   = .false.
   pfa(1)  = 1.
   pmua(1) = 1.
   pw(1)   = 1.
!
   scceq(1) = 0
   scmeq(1) = 0
   scqhs(1) = 0
   scnode(1)= 0
!
   scifri(1) = 0
   scimu (1) = 0
!
   sclceq(1) = 1
   sclceq(2) = 1
   sclmeq(1) = 1
   sclmeq(2) = 1
   sclnod(1) = 1
   sclnod(2) = 1
   sclqhs(1) = 1
   sclqhs(2) = 1
!
end
