      subroutine FLKAIN(pfa    ,pmua   ,pw     ,scceq  ,scmeq  ,scqhs ,
     +                  scifri ,scimu  ,sclceq ,sclmeq ,sclqhs ,sclnod,
     +                  scnode ,lfilt  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 17 lfilt             O  = True if a filter step must be performed.
c  7 pfa(nnf)          O  Uncertain bed friction parameters of all
c  8 pmua(nnmu)        O  Uncertain energy loss parameters in case of
c  9 pw                O  Uncertain wind stress parameter.
c 12 scifri(ngrid)     O  Contains the number of the uncorrelated r.n.
c                         process for bed friction (group nr. or correc-
c                         tion parameter nr.) of every normal grid cell,
c                         otherwise zero.
c 13 scimu(nstru)      O  Contains the number of the uncorrelated r.n.
c                         process for free gate flow in general structures
c                         (group nr.) of every structure, otherwise zero.
c 15 sclnod(nnn+1)     O  sclnod(i) points to begin of group i of nodes
c                         (in array scnode) with correlated r.n. process
c                         for node equations.
c 16 scnode(*)         O  Node numbers of all uncorrelated r.n.
c                         processes
c                         for node equations. The node numbers are grouped
c                         per process.
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flkain.pf,v $
c Revision 1.4  1999/03/15  15:50:16  kuipe_j
c tabs removed
c
c Revision 1.3  1996/12/02  10:04:54  kuipe_j
c avoid negative pointers
c
c Revision 1.2  1996/04/12  13:03:59  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:23:29  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c
c     Declaration of parameters
c
      logical          lfilt
      integer          scceq(*), scmeq(*) ,scqhs(*)  ,scnode(*),
     +                 scimu(*), scifri(*),
     +                 sclnod(*),sclceq(*),sclmeq(*) ,sclqhs(*)
      real             pfa(*)   ,pmua(*)  ,pw(1)
c
      lfilt   = .false.
      pfa(1)  = 1.
      pmua(1) = 1.
      pw(1)   = 1.
c
      scceq(1) = 0
      scmeq(1) = 0
      scqhs(1) = 0
      scnode(1)= 0
c
      scifri(1) = 0
      scimu (1) = 0
c
      sclceq(1) = 1
      sclceq(2) = 1
      sclmeq(1) = 1
      sclmeq(2) = 1
      sclnod(1) = 1
      sclnod(2) = 1
      sclqhs(1) = 1
      sclqhs(2) = 1
c
      end
