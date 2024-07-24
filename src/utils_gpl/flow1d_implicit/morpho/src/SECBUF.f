      subroutine secbuf ( nbran  ,ngrid  ,branch ,typcr  ,sedtr  ,
     &                    dissed ,nsedrd ,morbuf )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SECBUF (SEdiment Copy transports to BUFfer)
c
c Module description: Fill array Morbuf with transports that are redis-
c                     tributed at nodes and boundaries.
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
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: secbuf.pf,v $
c Revision 1.4  1998/11/13  09:00:00  kuipe_j
c output error 2d morphology
c
c Revision 1.3  1995/05/30  09:56:25  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:07:12  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:15  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:35  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:19  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer  nbran, ngrid   , nsedrd
      integer  branch(4,nbran), typcr(nbran)
      real     sedtr (ngrid,*), dissed(4,nbran), morbuf(ngrid,*)
c
c     Declaration of local variables
c
      integer  ibr, igp, isec, i1, i2
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Copy sedtr and dissed buffer to morbuf
c
      do 40 ibr = 1, nbran
c
         i1 = branch(3,ibr)
         i2 = branch(4,ibr)
c
         if (typcr(ibr) .eq. ccrsed) then
c
c           Sedredge branch (including S exchange)
c
            do 20 igp = i1, i2
               do 10 isec = 1, 3
                  morbuf(igp,isec) = sedtr(igp,isec)
 10            continue
 20         continue
c
c           Assign nodal values from array dissed
c
            morbuf(i1,1) = dissed(1,ibr)
            morbuf(i1,2) = dissed(2,ibr)
            morbuf(i2,1) = dissed(3,ibr)
            morbuf(i2,2) = dissed(4,ibr)
         else
c
c           Normal branch
c
            do 30 igp = i1+1, i2-1
               morbuf(igp,1) = sedtr(igp,1)
 30         continue
            if (nsedrd .gt. 0) then
               do 35 igp = i1, i2
                  morbuf(igp,2) = 0.
                  morbuf(igp,3) = 0.
 35            continue
            endif
c
c           Assign nodal values from array dissed
c
            morbuf(i1,1) = dissed(1,ibr)
            morbuf(i2,1) = dissed(3,ibr)
         endif
 40   continue
c
      end
