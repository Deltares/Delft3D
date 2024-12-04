subroutine secbuf ( nbran  ,ngrid  ,branch ,typcr  ,sedtr  ,&
&dissed ,nsedrd ,morbuf )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SECBUF (SEdiment Copy transports to BUFfer)
!
! Module description: Fill array Morbuf with transports that are redis-
!                     tributed at nodes and boundaries.
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
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: secbuf.pf,v $
! Revision 1.4  1998/11/13  09:00:00  kuipe_j
! output error 2d morphology
!
! Revision 1.3  1995/05/30  09:56:25  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:07:12  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:15  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:35  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:19  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer  nbran, ngrid   , nsedrd
   integer  branch(4,nbran), typcr(nbran)
   real     sedtr (ngrid,*), dissed(4,nbran), morbuf(ngrid,*)
!
!     Declaration of local variables
!
   integer  ibr, igp, isec, i1, i2
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Copy sedtr and dissed buffer to morbuf
!
   do 40 ibr = 1, nbran
!
      i1 = branch(3,ibr)
      i2 = branch(4,ibr)
!
      if (typcr(ibr) .eq. ccrsed) then
!
!           Sedredge branch (including S exchange)
!
         do 20 igp = i1, i2
            do 10 isec = 1, 3
               morbuf(igp,isec) = sedtr(igp,isec)
10          continue
20       continue
!
!           Assign nodal values from array dissed
!
         morbuf(i1,1) = dissed(1,ibr)
         morbuf(i1,2) = dissed(2,ibr)
         morbuf(i2,1) = dissed(3,ibr)
         morbuf(i2,2) = dissed(4,ibr)
      else
!
!           Normal branch
!
         do 30 igp = i1+1, i2-1
            morbuf(igp,1) = sedtr(igp,1)
30       continue
         if (nsedrd .gt. 0) then
            do 35 igp = i1, i2
               morbuf(igp,2) = 0.
               morbuf(igp,3) = 0.
35          continue
         endif
!
!           Assign nodal values from array dissed
!
         morbuf(i1,1) = dissed(1,ibr)
         morbuf(i2,1) = dissed(3,ibr)
      endif
40 continue
!
end
