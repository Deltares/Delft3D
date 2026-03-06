subroutine senetw (nbran  ,nnode  ,ngrid  ,maxlev ,maxtab ,ntabm ,&
&time   ,g      ,pacfac ,relden ,kinvis ,juer  ,&
&branch ,node   ,sdrdbf ,sedinf ,ntab   ,table ,&
&e      ,bfrict ,engpar ,rc     ,nucoef ,uscoef,&
&trform ,prslot ,af     ,afs    ,wf     ,wfs   ,&
&nlev   ,hlev   ,wft    ,ws     ,secths ,asubsc,&
&alfab  ,rs     ,cs     ,x      ,q2     ,qs    ,&
&h2     ,grsize ,forcon ,sedtr  ,celer  ,lrivr ,&
&psltvr ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SENETW (SEdiment NETWork)
!
! Module description: Calculation of the sediment transport in the net-
!                     work.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 27 af                P  -
! 28 afs               P  -
! 37 alfab             P  -
! 36 asubsc            P  -
! 21 bfrict            P  -
! 14 branch            P  -
! 47 celer             P  -
! 39 cs                P  -
! 20 e                 P  -
! 22 engpar            P  -
! 45 forcon            P  -
!  8 g                 P  -
! 44 grsize            P  -
! 43 h2                P  -
! 32 hlev              P  -
! 13 juer              P  -
! 48 ker               P  -
! 12 kinvis            P  -
!  4 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  5 maxtab            I  Maximum number of defined tables.
!  1 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
! 31 nlev              P  -
!  2 nnode             I  Number of nodes.
! 15 node              P  -
! 18 ntab              P  -
!  6 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 10 pacfac            P  -
! 26 prslot            P  -
! 49 psltvr            P  -
! 41 q2                P  -
! 42 qs                P  -
! 23 rc                P  -
! 11 relden            P  -
! 38 rs                P  -
! 16 sdrdbf            P  -
! 35 secths            P  -
! 17 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
!                         sedredge branch or not.
!                         (1,j) = Sedredge branch number (1..nsedrd)
!                                 else 0.
!                         (2,j) = Starting index in Rc array (River bend
!                                 curvature).
! 46 sedtr             P  -
! 19 table             P  -
!  7 time              P  -
! 25 trform            P  -
! 24 uscoef            P  -
! 29 wf                P  -
! 30 wfs               P  -
! 33 wft               P  -
! 34 ws                P  -
! 40 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! senorb  SEdiment NORmal Branch
! sesedb  SEdiment SEDredge Branch
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: senetw.pf,v $
! Revision 1.5  1998/06/11  11:47:28  kuipe_j
! Estuary special integrated
!
! Revision 1.4  1997/01/23  08:29:58  kuipe_j
! Make flow module robust
!
! Revision 1.3  1995/10/18  09:00:41  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:07:26  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:26  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:47:38  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:34:57  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:22  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   integer    nbran  ,nnode ,ngrid ,maxlev  ,maxtab ,ntabm ,juer  ,&
   &ker    ,nucoef
   integer    branch(4,nbran)  ,node(4,nnode)    ,sdrdbf(2,*)      ,&
   &sedinf(2,nbran)  ,bfrict(3,nbran)  ,ntab  (4,maxtab) ,&
   &nlev  (ngrid,maxlev)
   real       g     ,pacfac    ,relden,kinvis
   real       table (ntabm)    ,rc    (*)        ,e     (7,*)      ,&
   &uscoef(nrcoefs,nucoef)             ,trform(3,nbran)  ,&
   &engpar(9)        ,prslot(3,nbran)  ,psltvr(7,ngrid)  ,&
   &af    (ngrid)    ,afs   (ngrid,2 ) ,secths(ngrid)    ,&
   &wf    (ngrid)    ,wfs   (ngrid,2)  ,ws    (ngrid)    ,&
   &wft   (ngrid,maxlev)               ,&
   &asubsc(ngrid)    ,alfab (ngrid)    ,&
   &rs    (ngrid,3)  ,x     (ngrid)    ,&
   &cs    (ngrid,3)  ,qs    (ngrid,2)  ,&
   &grsize(4,ngrid,*),forcon(4,ngrid,*),&
   &sedtr (ngrid,*)  ,celer (ngrid,*)
   logical    lrivr

   double     precision  time, hlev (ngrid,maxlev),&
   &h2(ngrid), q2(ngrid)
!
!     Declaration of local parameters
!
   integer    ibr

!
   do 10 ibr = 1, nbran
!
      if (sedinf(1,ibr) .gt. 0) then
!
!           Sedredge branch
!
         call sesedb(nbran  ,nnode  ,ngrid  ,maxlev ,maxtab ,ntabm ,&
         &time   ,g      ,pacfac ,relden ,kinvis ,ibr   ,&
         &juer   ,branch ,node   ,sdrdbf ,sedinf ,ntab  ,&
         &table  ,e      ,rc     ,nucoef ,uscoef ,trform,&
         &afs    ,wf     ,wfs    ,hlev   ,rs     ,cs    ,&
         &x      ,q2     ,qs     ,h2     ,grsize ,forcon,&
         &sedtr  ,celer  ,ker    )
      else
!
!           Normal branch
!
         call senorb (nbran  ,ngrid  ,maxlev ,g      ,pacfac ,relden,&
         &kinvis ,ibr    ,juer   ,branch ,bfrict ,engpar,&
         &nucoef ,uscoef ,trform ,prslot ,af     ,afs   ,&
         &wf     ,wfs    ,nlev   ,hlev   ,wft    ,ws    ,&
         &secths ,rs     ,cs     ,asubsc ,alfab  ,q2    ,&
         &qs     ,h2     ,grsize ,forcon ,sedtr  ,celer ,&
         &lrivr  ,psltvr ,ker    )
      endif
10 continue
!
end
