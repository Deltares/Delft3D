subroutine sedim (nbran  ,nnode  ,nbrnod ,nboun  ,&
&maxlev ,nsedrd ,nnelvl ,nqlat  ,nmlat  ,&
&ngrid  ,maxtab ,ntabm  ,juer   ,itim   ,&
&time   ,dt     ,g      ,lmorp  ,ntmpgr ,&
&tmpgr  ,branch ,sedinf ,bfrict ,nonall ,&
&node   ,seddb  ,brnode ,bgout  ,sdrdbf ,&
&mbdpar ,ntab   ,afwfqs ,waoft  ,&
&ws     ,hlev   ,nlev   ,wft    ,psltvr ,&
&sectv  ,alfab  ,rp     ,x      ,cp     ,&
&qp     ,hp     ,grsize ,forcon ,slat   ,&
&sedtr  ,celer  ,trform ,prslot ,dissed ,&
&nellvl ,rc     ,e      ,nucoef ,uscoef ,&
&engpar ,sedpar ,morcon ,mltpar ,qltpar ,&
&qlat   ,table  ,lrivr  ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SEDIM (SEDIMent Transport)
!
! Module description: Calculation of sediment transport.
!
!                     First the sediment transport in each gridpoint
!                     will be calculated using one of the transport
!                     formulas. Non-alluvial layers are processed by
!                     SEANAL. Sediment transport through structures is
!                     processed by SESTRU. If the morphology module has
!                     been activated also the folowing steps are carried
!                     out:
!                     1)  The sediment transports at the inflowing boun-
!                         dary stations are determined.
!                     2)  Adaption of the sediment transport in the
!                         nodes.
!                     3)  Calculation of the lateral sediment trans-
!                         ports.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 34 afwfqs            P  -
! 42 alfab             P  -
! 24 bfrict            P  -
! 29 bgout             P  -
! 22 branch            P  -
! 28 brnode            P  -
! 52 celer             P  -
! 45 cp                P  -
! 55 dissed            P  -
! 17 dt                P  -
! 58 e                 P  -
! 60 engpar            P  -
! 49 forcon            P  -
! 18 g                 P  -
! 48 grsize            P  -
! 37 hlev              P  -
! 47 hp                P  -
! 15 itim(2)           I  Actual time level tn+1 expressed in date and
!                         time. Format (integer):
!                         itim(1) = YYYYMMDD (year,month,day)
!                         itim(2) = HHMMSSHH (hour,minute,second,
!                                   hundredth of a second)
! 14 juer              P  -
! 67 ker               IO Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 19 lmorp             I  Logical indicator for morphology computation
!                         = .true.  : with morphology computation
!                         = .false. : without morphology computation
!  6 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 12 maxtab            I  Maximum number of defined tables.
! 32 mbdpar            P  -
! 63 mltpar            P  -
! 62 morcon            P  -
!  4 nboun             P  -
!  1 nbran             I  Number of branches.
!  3 nbrnod            I  Maximum number of connected branches to one
!                         node.
! 56 nellvl            P  -
! 11 ngrid             I  Number of grid points in network.
! 38 nlev              P  -
! 10 nmlat             P  -
!  8 nnelvl            I  Number of entries in nellvl.
!  2 nnode             I  Number of nodes.
! 26 node              P  -
! 25 nonall            P  -
!  9 nqlat             P  -
!  7 nsedrd            P  -
! 33 ntab              P  -
! 13 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 20 ntmpgr            I  Number of scratch arrays with length ngrid
!                         that are packed in tmpgr.
! 54 prslot            P  -
! 40 psltvr            P  -
! 65 qlat              P  -
! 64 qltpar            P  -
! 46 qp                P  -
! 57 rc                P  -
! 43 rp                P  -
! 30 sdrdbf            P  -
! 41 sectv             P  -
! 27 seddb             P  -
! 23 sedinf            P  -
! 61 sedpar(10)        I  (1) = kinematic viscosity
!                         (2) = packing factor (porosity)
!                         (3) = relative density
!                         (4) = bed form height ratio
!                         (5) = reduction factor for sed. transp. width
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
! 51 sedtr             P  -
! 50 slat              P  -
! 66 table             P  -
! 16 time              P  -
! 21 tmpgr             P  -
! 53 trform            P  -
! 59 uscoef            P  -
! 35 waoft             P  -
! 39 wft               P  -
! 36 ws                P  -
! 44 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! seanal  SEdiment Adapt Non-Alluvial Layers
! seboun  SEdiment BOUNdaries
! selsed  SEdiment Lateral SEDiment
! senetw  SEdiment NETWork
! senode  SEdiment transports in NODEs
! setost  SEdiment Total Sediment Transport
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sedim.pf,v $
! Revision 1.10  1999/03/15  15:53:34  kuipe_j
! tabs removed
!
! Revision 1.9  1998/06/11  11:47:25  kuipe_j
! Estuary special integrated
!
! Revision 1.8  1997/01/23  08:29:57  kuipe_j
! Make flow module robust
!
! Revision 1.7  1996/12/04  12:00:13  kuipe_j
! declarations / undefined vars
!
! Revision 1.6  1996/12/03  08:24:14  kuipe_j
! calibration factor added in power distribution
!
! Revision 1.5  1996/09/03  14:46:55  kuipe_j
! frequency time hist,Power distribution added
!
! Revision 1.4  1995/10/18  09:00:37  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/05/30  09:56:28  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:07:18  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:20  hoeks_a
! Initial check-in
!
! Revision 1.4  1994/12/02  12:52:37  kuipe_j
! Removal of Sestru
!
! Revision 1.3  1994/11/28  08:47:32  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:34:44  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:20  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   integer    nbran  ,nnode  ,nbrnod ,nboun ,maxlev ,&
   &nsedrd ,nnelvl ,ntmpgr ,nqlat ,nmlat  ,ngrid  ,&
   &maxtab ,ntabm  ,nucoef ,juer   ,ker
   integer    branch(4,nbran)  ,sedinf(2,nbran)  ,bfrict(3,nbran) ,&
   &nonall(3,nbran)  ,&
   &node  (4,nnode)  ,seddb(3,nnode)   ,&
   &brnode(nbrnod+1,nnode)             ,&
   &bgout (3,nbrnod) ,&
   &sdrdbf(2,*)      ,&
   &mbdpar(5,*)      ,&
   &itim  (2)        ,&
   &ntab  (4,maxtab) ,&
   &nlev  (ngrid)
!
   real       g
   real       afwfqs(ngrid,8 ) ,waoft (ngrid,6)  ,ws    (ngrid)   ,&
   &wft   (ngrid,maxlev)          ,sectv  (ngrid,dmsecv),&
   &rp    (ngrid,4)  ,x     (ngrid)    ,cp    (ngrid,4) ,&
   &alfab (ngrid)   ,&
   &grsize(4,ngrid,*),forcon(4,ngrid,*),slat(ngrid,*)   ,&
   &sedtr (ngrid,*)  ,celer (ngrid,*)  ,&
   &tmpgr (ngrid,ntmpgr),&
   &trform(3,nbran)  ,prslot(3,nbran)  ,dissed(4,nbran) ,&
   &nellvl(*)        ,&
   &rc    (*)        ,&
   &e     (7,*)      ,&
   &uscoef(nrcoefs,nucoef)             ,&
   &engpar(9)        ,sedpar(10)       ,&
   &morcon(4,*)      ,&
   &mltpar(9,*)      ,&
   &qltpar(9,*)      ,qlat(*)          ,&
   &table (ntabm)    ,psltvr(7,ngrid)
   double     precision   time ,dt, hlev  (ngrid,maxlev)
   double     precision   hp(ngrid,3), qp(ngrid,3)
!
   logical    lmorp  , lrivr
!
!     Declaration of local parameters
!
   real       pacfac ,relden ,kinvis ,bdhrat
   character  txt*18
!
   ker = ok
!
   kinvis = sedpar(1)
   relden = sedpar(2)
   pacfac = sedpar(3)
   bdhrat = sedpar(4)
!
   call senetw (nbran  ,nnode  ,ngrid  ,maxlev ,maxtab ,ntabm  ,&
   &time   ,g      ,pacfac ,relden ,kinvis ,juer   ,&
   &branch ,node   ,sdrdbf ,sedinf ,ntab   ,table  ,&
   &e      ,bfrict ,engpar ,rc     ,nucoef ,uscoef ,&
!                                    <Af>            <Afs>
   &trform ,prslot ,waoft(1,3),     afwfqs(1,1)    ,&
!                    <Wf>        <Wfs>
   &waoft(1,1) ,afwfqs(1,3),nlev   ,hlev   ,wft    ,&
!                         <secths>    <asubsc>            <Rs>
   &ws     ,sectv(1,8) ,sectv(1,1) ,alfab  ,rp(1,2),&
!                   <Cs>            <Q2>    <Qs>            <h2>
   &cp(1,2),x      ,qp(1,3),afwfqs(1,7)    ,hp(1,3),&
   &grsize ,forcon ,sedtr  ,celer  ,lrivr  ,psltvr ,&
   &ker    )
!
   if (ker .eq. fatal) goto 1000
!
!     Check for non-alluvial layers
!
   if (nnelvl .gt. 0) then
!
      call seanal(nbran  ,ngrid  ,maxlev ,bdhrat     ,g     ,&
!                                             <afs>       <wfs>
      &branch ,nonall ,sedinf ,afwfqs(1,1),afwfqs(1,3),&
!                                     <qs>
      &hlev   ,nellvl ,afwfqs(1,7)        ,&
      &sedtr  ,celer )
   endif
!
!                                                 <wfs>
   call setost (nbran  ,ngrid  ,branch ,sedinf ,afwfqs(1,3) ,ws     ,&
   &sedtr  )
!
   if (lmorp) then
!
!        Morphology module will be called, so process boundary cond.
!
      call seboun (nboun  ,nbran  ,ngrid  ,maxtab ,ntabm  ,time   ,&
!                    <q2>    <qs>
      &qp(1,3),afwfqs(1,7)    ,sedtr  ,mbdpar ,branch ,&
      &sedinf ,ntab   ,table  ,dissed )
!
      call senode (nnode  ,nbran  ,nbrnod ,ngrid  ,maxtab ,ntabm  ,&
      &branch ,brnode ,bgout  ,sedinf ,sdrdbf ,seddb  ,&
!                                       <q2>
      &ntab   ,morcon ,qp(1,3),ws     ,table  ,sedtr  ,&
      &dissed ,trform )
!
      call selsed (ngrid  ,nqlat  ,nmlat  ,nbran  ,nsedrd ,maxtab ,&
      &ntabm  ,time   ,dt     ,branch ,sedinf ,x      ,&
      &sedtr  ,qltpar ,mltpar ,qlat   ,ntab   ,table  ,&
!                     <slat'>
      &tmpgr  ,slat   )
!
   endif
!
1000 continue
!
   if (ker .ne. ok) then
      write (txt,'(2(1x,i8))') itim
      call error (juer,'SEDT timestep@'//txt//'@',esemes,info)
      if (ker .ne. fatal) ker = ok
   endif
!
end
