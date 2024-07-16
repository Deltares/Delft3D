      subroutine sewres (nseman ,nsemap ,nsetim ,ngrid  ,nbran  ,nsedrd,
     &                   ntmpgr ,itim   ,istep  ,nstep  ,first  ,writim,
     &                   juer   ,branch ,typcr  ,lmorp  ,fd_nefis_res,
     &                   sedmap ,sedtim ,sedtr  ,dissed ,afwfqs ,cp    ,
     &                   trform ,grsize ,ncelse ,secpre ,buf    ,dt    ,
     &                   delta  ,g      ,sedini ,buffer ,gridnm ,nefhis)
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SEWRES (SEdiment Write sediment RESults)
c
c Module description: Writing of user defined sediment results to the
c                     result file.
c
c                     The user selected sediment results at user sup-
c                     plied locations and time levels will be stored on
c                     the result file. The stored data can be processed
c                     further by the User Interface.
c
c                     The user can select functions of place and of
c                     time. Writing can start on a new file or in case
c                     the data model is unchanged an existing file can
c                     be extended. In case the morphology model is
c                     active, distributed transport is copied to array
c                     SEDTR.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 14 branch            P  -
c 25 buf               P  -
c 18 dafdrs            P  -
c 17 defdrs            P  -
c 22 dissed            P  -
c 11 first             O  True in case of first call.
c  9 istep             P  -
c  8 itim              P  -
c 13 juer              P  -
c 26 ker               P  -
c 16 lmorp             I  Logical indicator for morphology computation
c                         = .true.  : with morphology computation
c                         = .false. : without morphology computation
c  5 nbran             I  Number of branches.
c 23 ncelse            P  -
c  4 ngrid             I  Number of grid points in network.
c  6 nsedrd            P  -
c  1 nseman            I  Number of main codes of sediment results.
c  2 nsemap            I  Number of entries in sedmap.
c  3 nsetim            I  Number of entries in sedtim.
c 10 nstep             P  -
c  7 ntmpgr            I  Number of scratch arrays with length ngrid
c                         that are packed in tmpgr.
c 24 secpre            P  -
c 19 sedmap            P  -
c 20 sedtim            P  -
c 21 sedtr             P  -
c 15 typcr             P  -
c 12 writim            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c secbuf  SEdiment Copy transports to BUFfer
c seser   SEdiment write SEdiment Results
c sewrhi  SEdiment WRite HIs files
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sewres.pf,v $
c Revision 1.5  1998/11/13  09:00:02  kuipe_j
c output error 2d morphology
c
c Revision 1.4  1997/06/17  11:27:17  kuipe_j
c output in history format
c
c Revision 1.3  1995/10/18  09:00:46  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:07:41  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:37  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:24  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer       nentri
      parameter    (nentri=6)
      integer      nseman  ,nsemap,nsetim ,ngrid ,nbran  ,ntmpgr,
     &             juer    ,istep ,nstep  ,nsedrd,ker    ,nefhis
      integer      fd_nefis_res, sedmap(nsemap),
     &             sedtim(nsetim) ,secpre(nseman),branch(4,nbran),
     &             itim  (2)      ,ncelse(2)     ,typcr (nbran)  ,
     &             sedini(*)
      real         delta ,g
      real         sedtr (ngrid,*),buf(ngrid,ntmpgr),dissed(4,nbran)  ,
     &             afwfqs(ngrid,8),cp (ngrid,4)     ,grsize(4,ngrid,*),
     &             trform(3,nbran), 
     &             buffer(nentri,ngrid)
      double precision    dt
      character*40 gridnm(*)
      logical      first   ,writim,lmorp
c
c     Declaration of local variables
c
      integer i
      integer code(nentri)
      data    (code(i),i=1,nentri) / 1, 2, 2, 2 ,3 ,3/
c
      if (first) call resadm(nentri, code, secpre)
c
      if (lmorp) then
c
c        Create a new sediment transport buffer including the distributed
c        sediment transports at nodes and boundaries
c
         call secbuf (nbran   ,ngrid  ,branch ,typcr   ,sedtr  ,dissed,
c                               <sedtr>
     &                nsedrd  ,buf(1,1))
c
c        Call output routine with new sediment transport buffer
c
         if     (nefhis.ge.1) then
c        NEFIS output
         call seser  (nseman  ,nsemap ,nsetim ,ngrid   ,nsedrd ,itim  ,
     &                istep, nstep, first, writim, juer, fd_nefis_res,
     &                sedmap ,sedtim ,buf(1,1),ncelse ,secpre,
     &                buf(1,4),ker    )
c
         endif
c
         if (nefhis.ne.1.and.dt.gt.0.0D0) then
c        HIS output
c        dt = negative for estuary morfology step, so no output
c
         call sewrhi(nseman  ,nsemap ,nsetim ,ngrid  ,nbran  ,nsedrd ,
     &               itim    ,istep  ,nstep  ,sedmap ,sedtim ,branch ,
c                    <sedtr>  <q2s>          <afs>            <cs> 
     &               buf(1,1),afwfqs(1,7)    ,afwfqs(1,1)    ,cp(1,2),
     &               trform  ,grsize ,secpre ,dt     ,delta  ,g      ,
     &               sedini  ,buffer ,gridnm )
         endif
      else
c
c        Use original sediment transport buffer
c
         if     (nefhis.ne.1) then
c        HIS output
         call sewrhi(nseman ,nsemap ,nsetim ,ngrid  ,nbran  ,nsedrd  ,
     &               itim   ,istep  ,nstep  ,sedmap ,sedtim ,branch  ,
c                            <q2s>           <afs>           <cs> 
     &               sedtr  ,afwfqs(1,7)    ,afwfqs(1,1)    ,cp(1,2) ,
     &               trform ,grsize ,secpre ,dt     ,delta  ,g       ,
     &               sedini ,buffer ,gridnm )
c
         endif
c
         if (nefhis.ge.1) then
c        NEFIS output
         call seser  (nseman  ,nsemap ,nsetim ,ngrid   ,nsedrd ,itim  ,
     &                istep, nstep, first, writim, juer, fd_nefis_res,
     &                sedmap ,sedtim ,sedtr   ,ncelse ,secpre,
     &                buf(1,4),ker    )
         endif
      endif
c
c     Set first to false
c
      first = .false.
c
      end
