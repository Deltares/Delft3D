      subroutine SOSCAG ( steps  ,
     +                    nbran  ,ngrid  ,
     +                    sedtr  ,dissed ,slat  ,
     +                    asedtr ,adissd ,aslat , 
     +                    ahp    ,hp     )      

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOSCAG (SObek Sediment Celerity AGgregate)
c
c Module description: This routine aggregates the calculated sediment
c                     transports, water levels, lateral sediment and
c                     distributed sediment transport.  
c
c                     Notice that sedredge branches are not allowed in
c                     an estuary morphology case ! 
c                     If the number of aggregation steps equals
c                     zero the arrays which contain averaged values are
c                     automatically initialised.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 10 adissd(4,nbran)   IO Avaraged distributed sediment transport in
c                         nodes and boundaries
c  8 asedtr(ngrid)     IO Avaraged sediment transport
c 11 aslat(ngrid)      IO Avaraged lateral sediment
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
c  2 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c  4 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
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
c  7 slat(ngrid,*)     I  Actual lateral sediment transport in grid
c              1|2        point i+1/2 for:
c                         (i,1) = Main or Left channel.
c                         (i,2) = Right channel.
c  1 steps             IO Number of aggregation steps (estuary morpho-
c                         logy)
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: soscag.pf,v $
c Revision 1.5  1998/06/11  11:47:49  kuipe_j
c Estuary special integrated
c
c Revision 1.4  1995/09/22  10:04:31  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:57:05  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:59  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:25  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:10:06  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:45  kuipe_j
c Initial version
c
c
c***********************************************************************
c

c
c     Parameters
c
      integer  steps, nbran, ngrid
      real     sedtr  (ngrid,*),
     +         dissed (4,nbran),
     +         slat   (ngrid,*),
     +         asedtr (ngrid)  ,
     +         adissd (4,nbran),
     +         aslat  (ngrid)
      double precision hp(ngrid,*)  
      real     ahp(ngrid,*)
c
c     Variables
c
      integer  igp, ibr, icnt ,it
c
      if (steps .eq. 0) then
c
c        Assign sediment transports, lateral sediment
c        and water levels.
c
         do 10 igp = 1, ngrid
            asedtr(igp) = sedtr(igp,1)
            aslat (igp) = slat (igp,1)
            do 5 it=1,3
               ahp(igp,it) = hp(igp,it)
  5         continue
 10      continue
c
         do 30 ibr = 1, nbran
            do 20 icnt = 1, 4
               adissd(icnt,ibr) = dissed(icnt,ibr)
 20         continue
 30      continue
c
      else
c
c        Add sediment transports, lateral sediment and
c        water levels.                 
c
         do 40 igp = 1, ngrid
            asedtr(igp) = asedtr(igp) + sedtr(igp,1)
            aslat (igp) = aslat (igp) + slat (igp,1)
            do 35 it=1,3
c                                            <H2>
               ahp(igp,it) = ahp(igp,it) + hp(igp,it)
 35         continue
 40      continue
        
         do 60 ibr = 1, nbran
            do 50 icnt = 1, 4
               adissd(icnt,ibr) = adissd(icnt,ibr) + dissed(icnt,ibr)
 50         continue
 60      continue
c
      endif
c
c     Increment number of avaraging steps
c
      steps = steps + 1
c
      return
      end

