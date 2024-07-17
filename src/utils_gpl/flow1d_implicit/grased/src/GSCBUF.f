      subroutine gscbuf ( nbran  ,ngrid  ,nfrac ,branch ,
     &                    sedtr  ,disgse ,morbuf )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             GSCBUF (Graded Sediment Copy transports to BUFfer)
c
c Module description: Fill array Morbuf with total transports that are
c                     redistributed at nodes and boundaries.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  6 dissed(4,nbran)   I  Redistributed sediment transport at begin and
c                         end of branches. At the outflow side of the
c                         branch the calculated transports are stored.
c                         At the inflow side the redistributed trans-
c                         ports are stored.
c                         (1,i)   Transport at section 1 (main or left
c                                 channel)  at begin of branch.
c                         (2,i)   Transport at section 2 (right channel)
c                                 at begin of branch.
c                         (3,i)   Transport at section 1 (main or left
c                                 channel)  at end of branch.
c                         (4,i)   Transport at section 2 (right channel)
c                                 at end of branch.
c  7 morbuf(ngrid,*)   O  Sediment transport results for each gridpoint.
c                1|2      As array Sedtr but with distributed transports
c                         at nodes and boundaries.
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  5 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
c               1|2       (At first transports per unit width, finaly
c                         total transports)
c                         - Normal branches:
c                         (i,1) = Sediment transport in gridpoint i of
c                                 main section.
c                         - Sedredge branches:
c                         (i,1) = Sediment transport in gridpoint i,
c                                 left channel.
c                         (i,2) = Sediment transport in gridpoint i,
c                                 right channel.
c                         (i,3) = Sediment exchange in gridpoint i.
c  4 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c=======================================================================
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gscbuf.F,v $
c Revision 1.2  1995/09/27  10:11:51  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c
c     Declaration of parameters
c
      integer  nbran , ngrid ,nfrac
      integer  branch(4,nbran)
      real     sedtr (ngrid), disgse(nfrac,2,nbran), morbuf(ngrid)
c
c     Declaration of local variables
c
      integer  ibr,  igp,  i1, i2 ,i
      real     esum, bsum
c
c     Copy sedtr and dissed buffer to morbuf
c
      do 30 ibr = 1, nbran
c
         i1 = branch(3,ibr)
         i2 = branch(4,ibr)
c
c        Normal branch
c
         do 10 igp = i1+1, i2-1
            morbuf(igp) = sedtr(igp)
 10      continue
c
c        Assign nodal values from array dissed
c
         bsum = 0.
         esum = 0.
         do 20 i=1,nfrac
            bsum = bsum + disgse(i,1,ibr)
            esum = esum + disgse(i,2,ibr)
 20      continue
         morbuf(i1) = bsum
         morbuf(i2) = esum
 30   continue
c
      end
 
