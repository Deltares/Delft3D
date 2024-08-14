      subroutine sedim (nbran  ,nnode  ,nbrnod ,nboun  ,
     &                  maxlev ,nsedrd ,nnelvl ,nqlat  ,nmlat  ,
     &                  ngrid  ,maxtab ,ntabm  ,juer   ,itim   ,
     &                  time   ,dt     ,g      ,lmorp  ,ntmpgr ,
     &                  tmpgr  ,branch ,sedinf ,bfrict ,nonall ,
     &                  node   ,seddb  ,brnode ,bgout  ,sdrdbf ,
     &                  mbdpar ,ntab   ,afwfqs ,waoft  ,
     &                  ws     ,hlev   ,nlev   ,wft    ,psltvr ,
     &                  sectv  ,alfab  ,rp     ,x      ,cp     ,
     &                  qp     ,hp     ,grsize ,forcon ,slat   ,
     &                  sedtr  ,celer  ,trform ,prslot ,dissed ,
     &                  nellvl ,rc     ,e      ,nucoef ,uscoef ,
     &                  engpar ,sedpar ,morcon ,mltpar ,qltpar ,
     &                  qlat   ,table  ,lrivr  ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SEDIM (SEDIMent Transport)
c
c Module description: Calculation of sediment transport.
c
c                     First the sediment transport in each gridpoint
c                     will be calculated using one of the transport
c                     formulas. Non-alluvial layers are processed by
c                     SEANAL. Sediment transport through structures is
c                     processed by SESTRU. If the morphology module has
c                     been activated also the folowing steps are carried
c                     out:
c                     1)  The sediment transports at the inflowing boun-
c                         dary stations are determined.
c                     2)  Adaption of the sediment transport in the
c                         nodes.
c                     3)  Calculation of the lateral sediment trans-
c                         ports.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 34 afwfqs            P  -
c 42 alfab             P  -
c 24 bfrict            P  -
c 29 bgout             P  -
c 22 branch            P  -
c 28 brnode            P  -
c 52 celer             P  -
c 45 cp                P  -
c 55 dissed            P  -
c 17 dt                P  -
c 58 e                 P  -
c 60 engpar            P  -
c 49 forcon            P  -
c 18 g                 P  -
c 48 grsize            P  -
c 37 hlev              P  -
c 47 hp                P  -
c 15 itim(2)           I  Actual time level tn+1 expressed in date and
c                         time. Format (integer):
c                         itim(1) = YYYYMMDD (year,month,day)
c                         itim(2) = HHMMSSHH (hour,minute,second,
c                                   hundredth of a second)
c 14 juer              P  -
c 67 ker               IO Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 19 lmorp             I  Logical indicator for morphology computation
c                         = .true.  : with morphology computation
c                         = .false. : without morphology computation
c  6 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 12 maxtab            I  Maximum number of defined tables.
c 32 mbdpar            P  -
c 63 mltpar            P  -
c 62 morcon            P  -
c  4 nboun             P  -
c  1 nbran             I  Number of branches.
c  3 nbrnod            I  Maximum number of connected branches to one
c                         node.
c 56 nellvl            P  -
c 11 ngrid             I  Number of grid points in network.
c 38 nlev              P  -
c 10 nmlat             P  -
c  8 nnelvl            I  Number of entries in nellvl.
c  2 nnode             I  Number of nodes.
c 26 node              P  -
c 25 nonall            P  -
c  9 nqlat             P  -
c  7 nsedrd            P  -
c 33 ntab              P  -
c 13 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 20 ntmpgr            I  Number of scratch arrays with length ngrid
c                         that are packed in tmpgr.
c 54 prslot            P  -
c 40 psltvr            P  -
c 65 qlat              P  -
c 64 qltpar            P  -
c 46 qp                P  -
c 57 rc                P  -
c 43 rp                P  -
c 30 sdrdbf            P  -
c 41 sectv             P  -
c 27 seddb             P  -
c 23 sedinf            P  -
c 61 sedpar(10)        I  (1) = kinematic viscosity
c                         (2) = packing factor (porosity)
c                         (3) = relative density
c                         (4) = bed form height ratio
c                         (5) = reduction factor for sed. transp. width
c                         (1,i) = Type of structure:
c                                 csweir (1) : Simple weir
c                                 caweir (2) : Advanced weir
c                                 csgate (3) : - not used, reserved for
c                                                simple gate -
c                                 cpump  (4) : Pump
c                                 cgenst (5) : General Structure
c                         (2,i) = Position of structure:
c                                 cstbra (1) : In branch
c                                 cstlat (2) : Lateral structure
c                         (3,i) = Left gridpoint.
c                         (4,i) = Right gridpoint.
c                         (5,i) = dummy
c                         (6,i) = dummy
c                         (7,i) = dummy
c                         (8,i) = dummy
c                         (9,i) = dummy
c                         (10,i)= dummy
c 51 sedtr             P  -
c 50 slat              P  -
c 66 table             P  -
c 16 time              P  -
c 21 tmpgr             P  -
c 53 trform            P  -
c 59 uscoef            P  -
c 35 waoft             P  -
c 39 wft               P  -
c 36 ws                P  -
c 44 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c seanal  SEdiment Adapt Non-Alluvial Layers
c seboun  SEdiment BOUNdaries
c selsed  SEdiment Lateral SEDiment
c senetw  SEdiment NETWork
c senode  SEdiment transports in NODEs
c setost  SEdiment Total Sediment Transport
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sedim.pf,v $
c Revision 1.10  1999/03/15  15:53:34  kuipe_j
c tabs removed
c
c Revision 1.9  1998/06/11  11:47:25  kuipe_j
c Estuary special integrated
c
c Revision 1.8  1997/01/23  08:29:57  kuipe_j
c Make flow module robust
c
c Revision 1.7  1996/12/04  12:00:13  kuipe_j
c declarations / undefined vars
c
c Revision 1.6  1996/12/03  08:24:14  kuipe_j
c calibration factor added in power distribution
c
c Revision 1.5  1996/09/03  14:46:55  kuipe_j
c frequency time hist,Power distribution added
c
c Revision 1.4  1995/10/18  09:00:37  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/05/30  09:56:28  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:07:18  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:20  hoeks_a
c Initial check-in
c
c Revision 1.4  1994/12/02  12:52:37  kuipe_j
c Removal of Sestru
c
c Revision 1.3  1994/11/28  08:47:32  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:34:44  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:20  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\sobdim.i'
c
c     Declaration of parameters
c
      integer    nbran  ,nnode  ,nbrnod ,nboun ,maxlev ,
     &           nsedrd ,nnelvl ,ntmpgr ,nqlat ,nmlat  ,ngrid  ,
     &           maxtab ,ntabm  ,nucoef ,juer   ,ker
      integer    branch(4,nbran)  ,sedinf(2,nbran)  ,bfrict(3,nbran) ,
     &           nonall(3,nbran)  ,
     &           node  (4,nnode)  ,seddb(3,nnode)   ,
     &           brnode(nbrnod+1,nnode)             ,
     &           bgout (3,nbrnod) ,
     &           sdrdbf(2,*)      ,
     &           mbdpar(5,*)      ,
     &           itim  (2)        ,
     &           ntab  (4,maxtab) ,
     &           nlev  (ngrid)
c
      real       g
      real       afwfqs(ngrid,8 ) ,waoft (ngrid,6)  ,ws    (ngrid)   ,
     &           wft   (ngrid,maxlev)          ,sectv  (ngrid,dmsecv),
     &           rp    (ngrid,4)  ,x     (ngrid)    ,cp    (ngrid,4) ,
     &           alfab (ngrid)   ,
     &           grsize(4,ngrid,*),forcon(4,ngrid,*),slat(ngrid,*)   ,
     &           sedtr (ngrid,*)  ,celer (ngrid,*)  ,
     &           tmpgr (ngrid,ntmpgr),
     &           trform(3,nbran)  ,prslot(3,nbran)  ,dissed(4,nbran) ,
     &           nellvl(*)        ,
     &           rc    (*)        ,
     &           e     (7,*)      ,
     &           uscoef(nrcoefs,nucoef)             ,
     &           engpar(9)        ,sedpar(10)       ,
     &           morcon(4,*)      ,
     &           mltpar(9,*)      ,
     &           qltpar(9,*)      ,qlat(*)          ,
     &           table (ntabm)    ,psltvr(7,ngrid)
      double     precision   time ,dt, hlev  (ngrid,maxlev)
      double     precision   hp(ngrid,3), qp(ngrid,3)
c
      logical    lmorp  , lrivr
c
c     Declaration of local parameters
c
      real       pacfac ,relden ,kinvis ,bdhrat 
      character  txt*18
c
      ker = ok
c
      kinvis = sedpar(1)
      relden = sedpar(2)
      pacfac = sedpar(3)
      bdhrat = sedpar(4)
c
      call senetw (nbran  ,nnode  ,ngrid  ,maxlev ,maxtab ,ntabm  ,
     &             time   ,g      ,pacfac ,relden ,kinvis ,juer   ,
     &             branch ,node   ,sdrdbf ,sedinf ,ntab   ,table  ,
     &             e      ,bfrict ,engpar ,rc     ,nucoef ,uscoef ,
c                                    <Af>            <Afs>
     &             trform ,prslot ,waoft(1,3),     afwfqs(1,1)    ,
c                    <Wf>        <Wfs>
     &             waoft(1,1) ,afwfqs(1,3),nlev   ,hlev   ,wft    ,
c                         <secths>    <asubsc>            <Rs>
     &             ws     ,sectv(1,8) ,sectv(1,1) ,alfab  ,rp(1,2),
c                   <Cs>            <Q2>    <Qs>            <h2>
     &             cp(1,2),x      ,qp(1,3),afwfqs(1,7)    ,hp(1,3),
     &             grsize ,forcon ,sedtr  ,celer  ,lrivr  ,psltvr ,
     &             ker    )
c
      if (ker .eq. fatal) goto 1000
c
c     Check for non-alluvial layers
c
      if (nnelvl .gt. 0) then
c
         call seanal(nbran  ,ngrid  ,maxlev ,bdhrat     ,g     ,
c                                             <afs>       <wfs>
     &               branch ,nonall ,sedinf ,afwfqs(1,1),afwfqs(1,3),
c                                     <qs>
     &               hlev   ,nellvl ,afwfqs(1,7)        ,
     &               sedtr  ,celer )
      endif
c
c                                                 <wfs>
      call setost (nbran  ,ngrid  ,branch ,sedinf ,afwfqs(1,3) ,ws     ,
     &             sedtr  )
c
      if (lmorp) then
c
c        Morphology module will be called, so process boundary cond.
c
         call seboun (nboun  ,nbran  ,ngrid  ,maxtab ,ntabm  ,time   ,
c                    <q2>    <qs>
     &                qp(1,3),afwfqs(1,7)    ,sedtr  ,mbdpar ,branch ,
     &                sedinf ,ntab   ,table  ,dissed )
c
         call senode (nnode  ,nbran  ,nbrnod ,ngrid  ,maxtab ,ntabm  ,
     &                branch ,brnode ,bgout  ,sedinf ,sdrdbf ,seddb  ,
c                                       <q2>
     &                ntab   ,morcon ,qp(1,3),ws     ,table  ,sedtr  ,
     &                dissed ,trform )
c
         call selsed (ngrid  ,nqlat  ,nmlat  ,nbran  ,nsedrd ,maxtab ,
     &                ntabm  ,time   ,dt     ,branch ,sedinf ,x      ,
     &                sedtr  ,qltpar ,mltpar ,qlat   ,ntab   ,table  ,
c                     <slat'>
     &                tmpgr  ,slat   )
c
      endif
c
 1000 continue
c
      if (ker .ne. ok) then
         write (txt,'(2(1x,i8))') itim
         call error (juer,'SEDT timestep@'//txt//'@',esemes,info)
         if (ker .ne. fatal) ker = ok
      endif
c
      end
