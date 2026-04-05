subroutine gscbuf ( nbran  ,ngrid  ,nfrac ,branch ,&
&sedtr  ,disgse ,morbuf )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             GSCBUF (Graded Sediment Copy transports to BUFfer)
!
! Module description: Fill array Morbuf with total transports that are
!                     redistributed at nodes and boundaries.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  6 dissed(4,nbran)   I  Redistributed sediment transport at begin and
!                         end of branches. At the outflow side of the
!                         branch the calculated transports are stored.
!                         At the inflow side the redistributed trans-
!                         ports are stored.
!                         (1,i)   Transport at section 1 (main or left
!                                 channel)  at begin of branch.
!                         (2,i)   Transport at section 2 (right channel)
!                                 at begin of branch.
!                         (3,i)   Transport at section 1 (main or left
!                                 channel)  at end of branch.
!                         (4,i)   Transport at section 2 (right channel)
!                                 at end of branch.
!  7 morbuf(ngrid,*)   O  Sediment transport results for each gridpoint.
!                1|2      As array Sedtr but with distributed transports
!                         at nodes and boundaries.
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  5 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
!               1|2       (At first transports per unit width, finaly
!                         total transports)
!                         - Normal branches:
!                         (i,1) = Sediment transport in gridpoint i of
!                                 main section.
!                         - Sedredge branches:
!                         (i,1) = Sediment transport in gridpoint i,
!                                 left channel.
!                         (i,2) = Sediment transport in gridpoint i,
!                                 right channel.
!                         (i,3) = Sediment exchange in gridpoint i.
!  4 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gscbuf.F,v $
! Revision 1.2  1995/09/27  10:11:51  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!
!     Declaration of parameters
!
   integer  nbran , ngrid ,nfrac
   integer  branch(4,nbran)
   real     sedtr (ngrid), disgse(nfrac,2,nbran), morbuf(ngrid)
!
!     Declaration of local variables
!
   integer  ibr,  igp,  i1, i2 ,i
   real     esum, bsum
!
!     Copy sedtr and dissed buffer to morbuf
!
   do 30 ibr = 1, nbran
!
      i1 = branch(3,ibr)
      i2 = branch(4,ibr)
!
!        Normal branch
!
      do 10 igp = i1+1, i2-1
         morbuf(igp) = sedtr(igp)
10    continue
!
!        Assign nodal values from array dissed
!
      bsum = 0.
      esum = 0.
      do 20 i=1,nfrac
         bsum = bsum + disgse(i,1,ibr)
         esum = esum + disgse(i,2,ibr)
20    continue
      morbuf(i1) = bsum
      morbuf(i2) = esum
30 continue
!
end

