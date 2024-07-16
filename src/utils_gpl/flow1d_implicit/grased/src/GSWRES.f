      subroutine gswres(nseman ,nsemap ,nsetim ,ngrid  ,nlayer ,nunlay ,
     &                  nbran ,ntmpgr ,nfrac ,ngrain ,submin ,subplus,  
     &                  itim   ,istep  ,nstep  ,first  ,writim ,wrirst ,
     &                  dt     ,branch ,fd_nefis_res, fd_nefis_rst,
     &                  sedmap ,sedtim ,sedtr  ,disgse ,grsize ,grain  ,
     &                  grsizmun       ,dmed0  ,deff   ,depos  ,ncelse ,
     &                  secpre ,buf    ,sedini ,buffer ,gridnm ,nefhis ,
     &                  nfrcmap,nfrctim,frcmap ,frctim ,lanrinbt       ,
     &                  nrdzdl ,zbave  ,zbfl   ,dzunla ,dzr    ,levunl ,
     &                  p0la   ,ptrla  ,pexla  ,juer   ,ker    )
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Graded Sediment Module
c
c Programmer:         J.Kuipers
c
c Module:             GSWRES (Graded SEdiment Write sediment RESults)
c
c Module description: Writing of user defined graded sediment results
c                     to the result file.
c
c                     The user selected graded sediment results at user  
c                     supplied locations and time levels will be stored
c                     on the result file. The stored data can be
c                     processed further by the User Interface.
c
c                     The user can select functions of place and of
c                     time. Writing can start on a new file or in case
c                     the data model is unchanged an existing file can
c                     be extended.
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
c NAME     DESCRIPTION
c gscbuf   Graded Sediment Copy transports to BUFfer
c gsexsedt Graded Sediment EXchange SEDiment Transports
c gsfrrhi  Graded Sediment FRaction Results HIs files
c gsrsta   Graded Sediment read or write ReSTArt information
c gsser    Graded Sediment SEdiment Results
c gswrhi   Graded Sediment WRite sediment HIS files
c=======================================================================
c
c     Declaration of parameters
c
      integer      nentri
      parameter   (nentri=16)
      integer      nseman  ,nsemap ,nsetim ,ngrid ,nlayer ,nunlay ,
     &             nbran   ,ntmpgr ,nfrac  ,juer  ,istep  ,nstep  ,
     &             ker     ,nefhis ,nfrcmap,nfrctim       ,ngrain ,
     &             submin  ,subplus  
      integer      fd_nefis_res, fd_nefis_rst,
     &             sedmap(nsemap) ,sedtim(nsetim),secpre(nseman)  ,
     &             branch(4,nbran),
     &             itim  (2)      ,ncelse(3)     ,sedini(*)       ,
     &             grain (4)      ,
     &             frcmap(nfrcmap),frctim(nfrctim),
     &             lanrinbt(ngrid),nrdzdl (ngrid) 
      real         dzunla
      real         sedtr (ngrid,nfrac+2)         ,disgse(nfrac,2,nbran),   
     &             buf   (ngrid,ntmpgr)          ,buffer(nentri,ngrid) ,
     &             grsize(4,ngrid,*)             ,dmed0 (ngrid)        ,
     &             grsizmun       (ngrid,ngrain,submin:subplus)        , 
     &             dzr   (ngrid)                 ,levunl(ngrid)        ,
     &             zbave  (ngrid)                ,zbfl   (ngrid)       ,
     &             deff(ngrid,2)                 ,
     &             ptrla (ngrid,nfrac,2)         ,pexla (ngrid,nfrac,2),   
     &             p0la  (ngrid,nfrac,nunlay)    
      double precision      dt
      logical      first ,writim, wrirst 
      character*40 gridnm(*)
      logical      depos  (ngrid)   
c
c     Declaration of local variables
c
      integer      i
 
      integer               :: fd_nefis_dum = -1
      integer      code(nentri)
      logical      inires

      data         (code(i),i=1,nentri) 
     &              /1,2,2,2,2,2,2,2,2,2,2,2,2,3,3,4/  

      if (first) then  
         do i=1,nseman
             secpre(i) = 100
         enddo   
         call resadm (nentri ,code   ,secpre )
      endif   
      
c     Create a new sediment transport buffer including the distributed
c     sediment transports at nodes and boundaries
c
c                                                 <sedtr-total>
      call gscbuf (nbran   ,ngrid  ,nfrac ,branch ,sedtr(1,nfrac+1),
     &             disgse  ,buf(1,1))
c
c     Call output routine with new sediment transport buffer
c
      if     (nefhis.ge.1) then
c        HIS output
         call gsser  (nseman  ,nsemap ,nsetim  ,ngrid   ,nlayer ,itim  ,
     &                istep, nstep, first, writim, juer, fd_nefis_res,
c                                           <sedtr-total>
     &                sedmap ,sedtim  ,buf(1,1),grsize ,deff  ,
     &                ncelse  ,secpre ,buf(1,2),ker     )
      endif
c
      if (nefhis.ne.1.and.dt.gt.0.0D0) then
c        HIS output
c        dt = negative for estuary morfology step, so no output
         call gswrhi (nseman ,nsemap  ,nsetim ,ngrid ,nlayer ,ngrain ,
     &                submin ,subplus ,itim   ,istep ,nstep  ,sedmap ,
c                            <sedtr-total>
     &                sedtim ,buf(1,1),grsize ,grain ,grsizmun       ,
     &                deff   ,secpre  ,dt     ,dzunla,dzr    ,levunl ,
     &                nrdzdl ,sedini ,buffer  ,gridnm,juer   ,ker    )
c
c        Set distrubuted transports in SEDTR
c
         call gsexsedt (nbran   ,ngrid   ,nfrac  ,branch ,sedtr   ,
     &                  disgse  )
c
         call gsfrrhi  (nfrcmap ,nfrctim ,ngrid  ,nunlay ,nfrac   ,
     &                  itim    ,istep   ,nstep  ,frcmap ,frctim  ,
     &                  sedtr   ,dt      ,sedini ,buffer ,gridnm  ,
     &                  lanrinbt,nrdzdl  ,p0la   ,ptrla  ,pexla   ,
     &                  juer    ,ker     )
c
c        Set original transports back in SEDTR
c
         call gsexsedt (nbran   ,ngrid   ,nfrac  ,branch ,sedtr   ,
     &                  disgse  )
c     
      endif            
c
c     Set first to false
c
      first = .false.
c
      if (wrirst) then

         call gsrsta (ngrid   ,nfrac  ,nunlay ,nlayer ,itim   ,
     &                ptrla(1,1,2)    ,pexla(1,1,2)   ,p0la   ,nrdzdl ,
     &                lanrinbt        ,zbave  ,zbfl   ,levunl ,dzr    ,
     &                deff(1,2)       ,dmed0  ,depos  ,
     &                .false. , fd_nefis_rst, fd_nefis_dum,juer   ,
c                              <ncelst> 
     &                first   ,ncelse(3)      ,inires ,ker    )
c
      endif
c
      end
