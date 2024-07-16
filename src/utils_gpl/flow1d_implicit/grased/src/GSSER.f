      subroutine gsser  (nseman ,nsemap ,nsetim ,ngrid  ,nlayer , 
     &                   itim   ,istep  ,nstep  ,first  ,writim ,juer  ,
     &                   fd_nefis_res, sedmap ,sedtim ,sedtr  ,grsize,
     &                   deff   ,ncelse ,secpre ,buf    ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Graded Sediment Module
c
c Programmer:         J.Kuipers
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 19 buf(ngrid)        O  Buffer for results to be written to NEFIS.
c 13 dafdrs            P  -
c 12 defdrs            P  -
c  9 first             I  True in case of first call.
c  7 istep             I  Current time step number (t(n+1)).
c  6 itim              P  -
c 11 juer              P  -
c 20 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 17 ncelse(2)         IO Contains actual cell numbers of sediment modu-
c                         le:
c                         (1) = ncelm (Map)
c                         (2) = ncelh (History)
c  4 ngrid             I  Number of grid points in network.
c  1 nseman            I  Number of main codes of sediment results.
c  2 nsemap            I  Number of entries in sedmap.
c  3 nsetim            I  Number of entries in sedtim.
c  8 nstep             I  Last time step number in simulation.
c 18 secpre(nseman)    I  secpre(i) = index in block tabel (1..nentri)
c                         for main code i of sediment results.
c 14 sedmap(nsemap)    I  Parameter list for MAP block with sediment
c                         results:
c                         (1)      = Report begin time
c                         (2)      = Report end time
c                         (3)      = Report time step
c                         (4)      = Report parameter 1 main code
c                         (5)      = Report parameter 1 sub code
c                         (i)      = Report parameter 1 main code
c                         (i+1)    = Report parameter 1 sub code
c                         (nsemap) = Report parameter n sub code
c 15 sedtim(nsetim)    I  Parameter list for HIST block with sediment
c                         results:
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                         (i+1)    = Report parameter code 1
c                         (i+2)    = Report parameter sub code 1
c                         (nsetim) = Report parameter n sub code
c 16 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
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
c 10 writim            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c flsdat  FLuSh buffers of DATa file
c putrel  PUT Real ELement to a nefis file
c resadm  RESults; make ADMinistration for element selection
c resdes  RESults; DEScription group is defined
c resini  RESults; writing is INItialized
c restim  RESults; writing of current TIMe
c yesmap  YES MAP results are written
c=======================================================================
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsser.F,v $
c Revision 1.2  1995/09/27  10:12:52  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c
c     Declaration of parameters
c
      integer      nseman  ,nsemap,nsetim ,ngrid ,nlayer ,juer  ,
     &             istep   ,nstep ,ker
      integer      fd_nefis_res, sedmap(nsemap),
     &             sedtim(nsetim) ,secpre(nseman),
     &             itim  (2)      ,ncelse(2)
      real         sedtr (ngrid)  ,buf(ngrid)    ,deff(ngrid,2) ,
     &             grsize(4,ngrid,*)
      logical      first   ,writim
c
c     Declaration of local variables
c
      integer      nentri
      parameter   (nentri=14)
      integer      neferr  ,i      ,j     ,k        ,ie       ,ie1  ,
     &             nrerr   ,ind    ,ncelm   ,ncelh  ,nsk      ,nlc  ,
     &             lastcod
      integer      usrord(1)       ,uindex(3)        
      character*16 grnamm          ,grnamh          ,grnamd         ,
     &             name
      character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,
     &             nameac(nentri)
      character*64 descel(nentri)
      character*8  txt
      logical      llog  ,new      ,newuit
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     Declaration of external functions
c
      integer      putrel ,flsdat
      logical      yesmap
      external     putrel ,flsdat ,yesmap
c
      data  usrord /1/
c
c     Definition of elements.
c
      data (descel(i) ,nameel(i) ,quanel(i) ,
     &      unitel(i),i=1,nentri) /
c      1
     &    'Sediment transport'       ,'HIS_Sg'     ,'Sg'     ,'m3/s' ,
c      2      
     &    'Grain size D10'           ,'HIS_D10'    ,'D10'    ,'m'    ,
c      2
     &    'Grain size D50'           ,'HIS_D50'    ,'D50'    ,'m'    ,
c      2
     &    'Grain size D90'           ,'HIS_D90'    ,'D90'    ,'m'    ,
c      2
     &    'Grain size Dmed'          ,'HIS_Dmed'   ,'Dmed'   ,'m'    ,
c      2  
     &    'Grain size D10 ex. layer' ,'HIS_D10(e)' ,'D10(e)' ,'m'    ,
c      2
     &    'Grain size D50 ex. layer' ,'HIS_D50(e)' ,'D50(e)' ,'m'    ,
c      2
     &    'Grain size D90 ex. layer' ,'HIS_D90(e)' ,'D90(e)' ,'m'    ,
c      2
     &    'Grain size Dmed ex. layer','HIS_Dmed(e)','Dmed(e)','m'    ,
c      2
     &    'Grain size D10 un. layer' ,'HIS_D10(u)' ,'D10(e)' ,'m'    ,
c      2
     &    'Grain size D50 un. layer' ,'HIS_D50(u)' ,'D50(e)' ,'m'    ,
c      2
     &    'Grain size D90 un. layer' ,'HIS_D90(u)' ,'D90(e)' ,'m'    ,
c      2 
     &    'Grain size Dmed un. layer','HIS_Dmed(u)','Dmed(e)','m'    ,
c      3
     &    'Effective layer thickness','HIS_Elth'   ,'Elth'   ,'m'    /
c
      data  grnamm   ,grnamh ,grnamd/
     &      'GSEDT-MAP-GROUP' ,
     &      'GSEDT-HIS-GROUP' ,
     &      'GSEDT-DES-GROUP' /
c
      ncelm   = ncelse(1)
      ncelh   = ncelse(2)
      lastcod = nentri
c
c     Initialize.
c
      if (first) then
         nrerr = eseboo
c
         call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'GSEDT' ,
     &                nentri ,nsemap ,nsetim ,ngrid  ,itim   ,nameel ,
     &                quanel ,unitel ,descel ,secpre ,sedmap ,sedtim ,
     &                ncelm  ,ncelh  ,nameac ,lastcod,neferr )
c
         if (neferr.ne.0) goto 1000
      endif
c
c     Write Map results
c
      if (nsemap .gt. 3 .and. ncelm .ge. 0) then
         if (yesmap(sedmap(1),sedmap(2),sedmap(3),istep)) then
            nrerr = esemap
c
            call restim (fd_nefis_res ,grnamm ,1  ,itim  ,ncelm  ,
     &                   nameac ,neferr )
            if (neferr.ne.0) goto 1000
c
            call resdes ('GSEDT', fd_nefis_res, grnamd ,1 ,writim ,
     &                   sedtim ,ncelm  ,ncelh  ,neferr )
            if (neferr.ne.0) goto 1000
c
            uindex(1) = ncelm
            uindex(2) = ncelm
            uindex(3) = 1
c
            do 20 i = 4,nsemap,2
               ie = secpre(sedmap(i))+sedmap(i+1)
               if (ie.le.lastcod) then
c             
c                Codes > lastcod can only be used in his format             
c
c
c                The element names of the Map block start with
c                MAP instead of HIS.
c 
                 name  = 'MAP'//nameel(ie)(4:)
               
                 if (ie.eq.1) then
c
                    neferr = putrel (fd_nefis_res, grnamm ,name   ,
     &                               uindex  ,usrord ,sedtr  )
                    if (neferr.ne.0) goto 1000
                 else if (ie.eq.14) then
c
                    neferr = putrel (fd_nefis_res, grnamm ,name   ,
     &                               uindex  ,usrord ,deff(1,2))
                    if (neferr.ne.0) goto 1000
                 else
                    if (ie .gt. 9) then
                       ie1 = ie - 9
                       k   = nlayer+1                                   
                    else if (ie .gt. 5) then
                       ie1 = ie - 5
                       k   = 2
                    else
                       ie1 = ie - 1
                       k   = 1
                    endif
c
                    do 10 j =1,ngrid
                       buf(j) = grsize(ie1,j,k)
   10               continue
                    neferr = putrel (fd_nefis_res, grnamm ,name   ,
     &                               uindex  ,usrord ,buf    )
                    if (neferr.ne.0) goto 1000
                  endif
                endif  
   20       continue
         endif
      endif
c
c     Write History results.
c
      nlc = sedtim(1)
      new = mod(nsetim-nlc,2) .eq. 0
      if (new) then 
         nsk = 3
      else
         nsk = 0
      endif

      if (nsetim .gt. 1+nsk .and. ncelh .ge. 0) then
         if (new) then
            newuit = yesmap(sedtim(nlc+2),sedtim(nlc+3),sedtim(nlc+4),
     &                      istep)
         else
            newuit = .false.
         endif
         if ( (sedtim(1).gt.0 .and. .not. new) .or. newuit ) then
            nrerr = esehis
c
            call restim (fd_nefis_res, grnamh ,1  ,itim  ,ncelh  ,
     &                   nameac ,neferr )
            if (neferr.ne.0) goto 1000
c
            call resdes ('GSEDT', fd_nefis_res, grnamd ,2 ,writim ,
     &                   sedtim ,ncelm  ,ncelh  ,neferr )
            if (neferr.ne.0) goto 1000
c
            uindex(1) = ncelh
            uindex(2) = ncelh
            uindex(3) = 1
c
            do 60 i = sedtim(1)+2+nsk,nsetim,2
               ie = secpre(sedtim(i))+sedtim(i+1)
               if (ie.le.lastcod) then
c             
c              Codes > lastcod can only be used in his format             
c
                 if (ie.eq.1) then
                     do 30 j=1,sedtim(1)
                        buf(j) = sedtr(sedtim(j+1))
   30               continue
                 else if (ie.eq.14) then
                    do 35 j=1,sedtim(1)
                        buf(j) = deff(sedtim(j+1),2)
   35               continue
                 else
                    if (ie .gt. 9) then
                       ie1 = ie - 9
                       k   = nlayer+1 
                    else if (ie .gt. 5) then
                       ie1 = ie - 5
                       k   = 2
                    else
                       ie1 = ie - 1
                       k   = 1
                    endif
c
                    do 40 j=1,sedtim(1)
                        ind    = sedtim(j+1)
                        buf(j) = grsize(ie1,ind,k)
   40               continue
                 endif
c
                 neferr = putrel (fd_nefis_res, grnamh ,nameel(ie) ,
     &                           uindex  ,usrord ,buf    )
                 if (neferr.ne.0) goto 1000
               endif
   60       continue
         endif
      endif
c
c     Be sure that at the last step the number of cells has been
c     written correctly.
c
      if (istep.ge.nstep) then
         nrerr = eseeoo
         llog  = .true.
c
         call resdes ('GSEDT', fd_nefis_res, grnamd ,3 ,llog  ,
     &                 sedtim ,ncelm  ,ncelh  ,neferr )
         if (neferr.ne.0) goto 1000
c
         neferr = flsdat(fd_nefis_res)
         if (neferr.ne.0) goto 1000
      endif
c
      ncelse(1) = ncelm
      ncelse(2) = ncelh
c
      return
c
 1000 continue
c
      ker = fatal
      write (txt,'(i8)') neferr
      call error (juer ,'GSSER @'//txt//'@' ,nrerr ,ker)
c
      end
