subroutine sewres (nseman ,nsemap ,nsetim ,ngrid  ,nbran  ,nsedrd,&
&ntmpgr ,itim   ,istep  ,nstep  ,first  ,writim,&
&juer   ,branch ,typcr  ,lmorp  ,fd_nefis_res,&
&sedmap ,sedtim ,sedtr  ,dissed ,afwfqs ,cp    ,&
&trform ,grsize ,ncelse ,secpre ,buf    ,dt    ,&
&delta  ,g      ,sedini ,buffer ,gridnm ,nefhis)
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SEWRES (SEdiment Write sediment RESults)
!
! Module description: Writing of user defined sediment results to the
!                     result file.
!
!                     The user selected sediment results at user sup-
!                     plied locations and time levels will be stored on
!                     the result file. The stored data can be processed
!                     further by the User Interface.
!
!                     The user can select functions of place and of
!                     time. Writing can start on a new file or in case
!                     the data model is unchanged an existing file can
!                     be extended. In case the morphology model is
!                     active, distributed transport is copied to array
!                     SEDTR.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 14 branch            P  -
! 25 buf               P  -
! 18 dafdrs            P  -
! 17 defdrs            P  -
! 22 dissed            P  -
! 11 first             O  True in case of first call.
!  9 istep             P  -
!  8 itim              P  -
! 13 juer              P  -
! 26 ker               P  -
! 16 lmorp             I  Logical indicator for morphology computation
!                         = .true.  : with morphology computation
!                         = .false. : without morphology computation
!  5 nbran             I  Number of branches.
! 23 ncelse            P  -
!  4 ngrid             I  Number of grid points in network.
!  6 nsedrd            P  -
!  1 nseman            I  Number of main codes of sediment results.
!  2 nsemap            I  Number of entries in sedmap.
!  3 nsetim            I  Number of entries in sedtim.
! 10 nstep             P  -
!  7 ntmpgr            I  Number of scratch arrays with length ngrid
!                         that are packed in tmpgr.
! 24 secpre            P  -
! 19 sedmap            P  -
! 20 sedtim            P  -
! 21 sedtr             P  -
! 15 typcr             P  -
! 12 writim            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! secbuf  SEdiment Copy transports to BUFfer
! seser   SEdiment write SEdiment Results
! sewrhi  SEdiment WRite HIs files
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sewres.pf,v $
! Revision 1.5  1998/11/13  09:00:02  kuipe_j
! output error 2d morphology
!
! Revision 1.4  1997/06/17  11:27:17  kuipe_j
! output in history format
!
! Revision 1.3  1995/10/18  09:00:46  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:07:41  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:37  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:24  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer       nentri
   parameter    (nentri=6)
   integer      nseman  ,nsemap,nsetim ,ngrid ,nbran  ,ntmpgr,&
   &juer    ,istep ,nstep  ,nsedrd,ker    ,nefhis
   integer      fd_nefis_res, sedmap(nsemap),&
   &sedtim(nsetim) ,secpre(nseman),branch(4,nbran),&
   &itim  (2)      ,ncelse(2)     ,typcr (nbran)  ,&
   &sedini(*)
   real         delta ,g
   real         sedtr (ngrid,*),buf(ngrid,ntmpgr),dissed(4,nbran)  ,&
   &afwfqs(ngrid,8),cp (ngrid,4)     ,grsize(4,ngrid,*),&
   &trform(3,nbran),&
   &buffer(nentri,ngrid)
   double precision    dt
   character*40 gridnm(*)
   logical      first   ,writim,lmorp
!
!     Declaration of local variables
!
   integer i
   integer code(nentri)
   data    (code(i),i=1,nentri) / 1, 2, 2, 2 ,3 ,3/
!
   if (first) call resadm(nentri, code, secpre)
!
   if (lmorp) then
!
!        Create a new sediment transport buffer including the distributed
!        sediment transports at nodes and boundaries
!
      call secbuf (nbran   ,ngrid  ,branch ,typcr   ,sedtr  ,dissed,&
!                               <sedtr>
      &nsedrd  ,buf(1,1))
!
!        Call output routine with new sediment transport buffer
!
      if     (nefhis.ge.1) then
!        NEFIS output
         call seser  (nseman  ,nsemap ,nsetim ,ngrid   ,nsedrd ,itim  ,&
         &istep, nstep, first, writim, juer, fd_nefis_res,&
         &sedmap ,sedtim ,buf(1,1),ncelse ,secpre,&
         &buf(1,4),ker    )
!
      endif
!
      if (nefhis.ne.1.and.dt.gt.0.0D0) then
!        HIS output
!        dt = negative for estuary morfology step, so no output
!
         call sewrhi(nseman  ,nsemap ,nsetim ,ngrid  ,nbran  ,nsedrd ,&
         &itim    ,istep  ,nstep  ,sedmap ,sedtim ,branch ,&
!                    <sedtr>  <q2s>          <afs>            <cs>
         &buf(1,1),afwfqs(1,7)    ,afwfqs(1,1)    ,cp(1,2),&
         &trform  ,grsize ,secpre ,dt     ,delta  ,g      ,&
         &sedini  ,buffer ,gridnm )
      endif
   else
!
!        Use original sediment transport buffer
!
      if     (nefhis.ne.1) then
!        HIS output
         call sewrhi(nseman ,nsemap ,nsetim ,ngrid  ,nbran  ,nsedrd  ,&
         &itim   ,istep  ,nstep  ,sedmap ,sedtim ,branch  ,&
!                            <q2s>           <afs>           <cs>
         &sedtr  ,afwfqs(1,7)    ,afwfqs(1,1)    ,cp(1,2) ,&
         &trform ,grsize ,secpre ,dt     ,delta  ,g       ,&
         &sedini ,buffer ,gridnm )
!
      endif
!
      if (nefhis.ge.1) then
!        NEFIS output
         call seser  (nseman  ,nsemap ,nsetim ,ngrid   ,nsedrd ,itim  ,&
         &istep, nstep, first, writim, juer, fd_nefis_res,&
         &sedmap ,sedtim ,sedtr   ,ncelse ,secpre,&
         &buf(1,4),ker    )
      endif
   endif
!
!     Set first to false
!
   first = .false.
!
end
