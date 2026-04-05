subroutine gswres(nseman ,nsemap ,nsetim ,ngrid  ,nlayer ,nunlay ,&
&nbran ,ntmpgr ,nfrac ,ngrain ,submin ,subplus,&
&itim   ,istep  ,nstep  ,first  ,writim ,wrirst ,&
&dt     ,branch ,fd_nefis_res, fd_nefis_rst,&
&sedmap ,sedtim ,sedtr  ,disgse ,grsize ,grain  ,&
&grsizmun       ,dmed0  ,deff   ,depos  ,ncelse ,&
&secpre ,buf    ,sedini ,buffer ,gridnm ,nefhis ,&
&nfrcmap,nfrctim,frcmap ,frctim ,lanrinbt       ,&
&nrdzdl ,zbave  ,zbfl   ,dzunla ,dzr    ,levunl ,&
&p0la   ,ptrla  ,pexla  ,juer   ,ker    )
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment Module
!
! Programmer:         J.Kuipers
!
! Module:             GSWRES (Graded SEdiment Write sediment RESults)
!
! Module description: Writing of user defined graded sediment results
!                     to the result file.
!
!                     The user selected graded sediment results at user
!                     supplied locations and time levels will be stored
!                     on the result file. The stored data can be
!                     processed further by the User Interface.
!
!                     The user can select functions of place and of
!                     time. Writing can start on a new file or in case
!                     the data model is unchanged an existing file can
!                     be extended.
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
! NAME     DESCRIPTION
! gscbuf   Graded Sediment Copy transports to BUFfer
! gsexsedt Graded Sediment EXchange SEDiment Transports
! gsfrrhi  Graded Sediment FRaction Results HIs files
! gsrsta   Graded Sediment read or write ReSTArt information
! gsser    Graded Sediment SEdiment Results
! gswrhi   Graded Sediment WRite sediment HIS files
!=======================================================================
!
!     Declaration of parameters
!
   integer      nentri
   parameter   (nentri=16)
   integer      nseman  ,nsemap ,nsetim ,ngrid ,nlayer ,nunlay ,&
   &nbran   ,ntmpgr ,nfrac  ,juer  ,istep  ,nstep  ,&
   &ker     ,nefhis ,nfrcmap,nfrctim       ,ngrain ,&
   &submin  ,subplus
   integer      fd_nefis_res, fd_nefis_rst,&
   &sedmap(nsemap) ,sedtim(nsetim),secpre(nseman)  ,&
   &branch(4,nbran),&
   &itim  (2)      ,ncelse(3)     ,sedini(*)       ,&
   &grain (4)      ,&
   &frcmap(nfrcmap),frctim(nfrctim),&
   &lanrinbt(ngrid),nrdzdl (ngrid)
   real         dzunla
   real         sedtr (ngrid,nfrac+2)         ,disgse(nfrac,2,nbran),&
   &buf   (ngrid,ntmpgr)          ,buffer(nentri,ngrid) ,&
   &grsize(4,ngrid,*)             ,dmed0 (ngrid)        ,&
   &grsizmun       (ngrid,ngrain,submin:subplus)        ,&
   &dzr   (ngrid)                 ,levunl(ngrid)        ,&
   &zbave  (ngrid)                ,zbfl   (ngrid)       ,&
   &deff(ngrid,2)                 ,&
   &ptrla (ngrid,nfrac,2)         ,pexla (ngrid,nfrac,2),&
   &p0la  (ngrid,nfrac,nunlay)
   double precision      dt
   logical      first ,writim, wrirst
   character*40 gridnm(*)
   logical      depos  (ngrid)
!
!     Declaration of local variables
!
   integer      i

   integer               :: fd_nefis_dum = -1
   integer      code(nentri)
   logical      inires

   data         (code(i),i=1,nentri)&
   &/1,2,2,2,2,2,2,2,2,2,2,2,2,3,3,4/

   if (first) then
      do i=1,nseman
         secpre(i) = 100
      enddo
      call resadm (nentri ,code   ,secpre )
   endif

!     Create a new sediment transport buffer including the distributed
!     sediment transports at nodes and boundaries
!
!                                                 <sedtr-total>
   call gscbuf (nbran   ,ngrid  ,nfrac ,branch ,sedtr(1,nfrac+1),&
   &disgse  ,buf(1,1))
!
!     Call output routine with new sediment transport buffer
!
   if     (nefhis.ge.1) then
!        HIS output
      call gsser  (nseman  ,nsemap ,nsetim  ,ngrid   ,nlayer ,itim  ,&
      &istep, nstep, first, writim, juer, fd_nefis_res,&
!                                           <sedtr-total>
      &sedmap ,sedtim  ,buf(1,1),grsize ,deff  ,&
      &ncelse  ,secpre ,buf(1,2),ker     )
   endif
!
   if (nefhis.ne.1.and.dt.gt.0.0D0) then
!        HIS output
!        dt = negative for estuary morfology step, so no output
      call gswrhi (nseman ,nsemap  ,nsetim ,ngrid ,nlayer ,ngrain ,&
      &submin ,subplus ,itim   ,istep ,nstep  ,sedmap ,&
!                            <sedtr-total>
      &sedtim ,buf(1,1),grsize ,grain ,grsizmun       ,&
      &deff   ,secpre  ,dt     ,dzunla,dzr    ,levunl ,&
      &nrdzdl ,sedini ,buffer  ,gridnm,juer   ,ker    )
!
!        Set distrubuted transports in SEDTR
!
      call gsexsedt (nbran   ,ngrid   ,nfrac  ,branch ,sedtr   ,&
      &disgse  )
!
      call gsfrrhi  (nfrcmap ,nfrctim ,ngrid  ,nunlay ,nfrac   ,&
      &itim    ,istep   ,nstep  ,frcmap ,frctim  ,&
      &sedtr   ,dt      ,sedini ,buffer ,gridnm  ,&
      &lanrinbt,nrdzdl  ,p0la   ,ptrla  ,pexla   ,&
      &juer    ,ker     )
!
!        Set original transports back in SEDTR
!
      call gsexsedt (nbran   ,ngrid   ,nfrac  ,branch ,sedtr   ,&
      &disgse  )
!
   endif
!
!     Set first to false
!
   first = .false.
!
   if (wrirst) then

      call gsrsta (ngrid   ,nfrac  ,nunlay ,nlayer ,itim   ,&
      &ptrla(1,1,2)    ,pexla(1,1,2)   ,p0la   ,nrdzdl ,&
      &lanrinbt        ,zbave  ,zbfl   ,levunl ,dzr    ,&
      &deff(1,2)       ,dmed0  ,depos  ,&
      &.false. , fd_nefis_rst, fd_nefis_dum,juer   ,&
!                              <ncelst>
      &first   ,ncelse(3)      ,inires ,ker    )
!
   endif
!
end
