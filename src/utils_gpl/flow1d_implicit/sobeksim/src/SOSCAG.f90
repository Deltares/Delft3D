subroutine SOSCAG ( steps  ,&
&nbran  ,ngrid  ,&
&sedtr  ,dissed ,slat  ,&
&asedtr ,adissd ,aslat ,&
&ahp    ,hp     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOSCAG (SObek Sediment Celerity AGgregate)
!
! Module description: This routine aggregates the calculated sediment
!                     transports, water levels, lateral sediment and
!                     distributed sediment transport.
!
!                     Notice that sedredge branches are not allowed in
!                     an estuary morphology case !
!                     If the number of aggregation steps equals
!                     zero the arrays which contain averaged values are
!                     automatically initialised.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 10 adissd(4,nbran)   IO Avaraged distributed sediment transport in
!                         nodes and boundaries
!  8 asedtr(ngrid)     IO Avaraged sediment transport
! 11 aslat(ngrid)      IO Avaraged lateral sediment
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
!  2 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
!  4 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
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
!  7 slat(ngrid,*)     I  Actual lateral sediment transport in grid
!              1|2        point i+1/2 for:
!                         (i,1) = Main or Left channel.
!                         (i,2) = Right channel.
!  1 steps             IO Number of aggregation steps (estuary morpho-
!                         logy)
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: soscag.pf,v $
! Revision 1.5  1998/06/11  11:47:49  kuipe_j
! Estuary special integrated
!
! Revision 1.4  1995/09/22  10:04:31  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:57:05  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:59  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:25  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:10:06  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:45  kuipe_j
! Initial version
!
!
!***********************************************************************
!

!
!     Parameters
!
   integer  steps, nbran, ngrid
   real     sedtr  (ngrid,*),&
   &dissed (4,nbran),&
   &slat   (ngrid,*),&
   &asedtr (ngrid)  ,&
   &adissd (4,nbran),&
   &aslat  (ngrid)
   double precision hp(ngrid,*)
   real     ahp(ngrid,*)
!
!     Variables
!
   integer  igp, ibr, icnt ,it
!
   if (steps .eq. 0) then
!
!        Assign sediment transports, lateral sediment
!        and water levels.
!
      do 10 igp = 1, ngrid
         asedtr(igp) = sedtr(igp,1)
         aslat (igp) = slat (igp,1)
         do 5 it=1,3
            ahp(igp,it) = hp(igp,it)
5        continue
10    continue
!
      do 30 ibr = 1, nbran
         do 20 icnt = 1, 4
            adissd(icnt,ibr) = dissed(icnt,ibr)
20       continue
30    continue
!
   else
!
!        Add sediment transports, lateral sediment and
!        water levels.
!
      do 40 igp = 1, ngrid
         asedtr(igp) = asedtr(igp) + sedtr(igp,1)
         aslat (igp) = aslat (igp) + slat (igp,1)
         do 35 it=1,3
!                                            <H2>
            ahp(igp,it) = ahp(igp,it) + hp(igp,it)
35       continue
40    continue

      do 60 ibr = 1, nbran
         do 50 icnt = 1, 4
            adissd(icnt,ibr) = adissd(icnt,ibr) + dissed(icnt,ibr)
50       continue
60    continue
!
   endif
!
!     Increment number of avaraging steps
!
   steps = steps + 1
!
   return
end

