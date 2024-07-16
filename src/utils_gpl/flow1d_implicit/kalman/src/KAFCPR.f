      subroutine KAFCPR(fd_nefis_res, nfcpmn ,nkfmmp ,np     ,nkapar ,
     &                  itim   ,istep  ,nstep  ,first  ,writim ,lfilt  ,
     &                  juer   ,kfmmap ,p1     ,buf    ,ncelm  ,fcpcpr ,
     &                  ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAFCPR (KAlman Filter Covariance Parameter Results)
c
c Module description: Write the covariance matrix of parameters to the
c                     result file.
c
c                     The stored data can be processed further by the
c                     User Interface. It is in map format but as it is
c                     a 2D-array, it cannot be used in the same way as
c                     other map functions. See [S-DD-002.1MR] for a
c                     specification of the nefis names.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 16 buf(ngrid)        O  Buffer for results to be written to NEFIS.
c  2 dafdrs            P  -
c  1 defdrs            P  -
c 18 fcpcpr(nfcpmn)    I  fcpcpr(i) = index in block table (1...nentri)
c                         for main code i of the parameter covariance
c 10 first             I  True in case of first call.
c  8 istep             I  Current time step number (t(n+1)).
c  7 itim              P  -
c 13 juer              P  -
c 19 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 14 kfmmap(*)         I  Parameter list for parameter covariance matrix
c                         (MAP block)
c                         (1/2/3) Report step of begin, end, resp.
c                         increment.
c                         (4) Main code parameter 1.
c                         (5) Sub code parameter 1.
c 12 lfilt             I  = True if a filter step must be performed.
c 17 ncelm             I  Actual cell number of a map block of result
c                         file.
c  3 nfcpmn            I  Number of main codes of parameter covariance
c  6 nkapar            I  = nnf + nnmu + 1
c  4 nkfmmp            I  Number of entries in kfmmap.
c  5 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c  9 nstep             I  Last time step number in simulation.
c 15 p1(np,np)         I  Matrix with covariances of waterlevels,
c                         discharges and uncertain correction parameters
c                         (bed friction, contraction in case of free gate
c                         flow and wind) on time level n+1|n+1 (filtered
c                         values) or n|n (previous time step).
c 11 writim            P  -
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
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kafcpr.pf,v $
c Revision 1.4  1999/03/15  15:51:43  kuipe_j
c tabs removed
c
c Revision 1.3  1996/12/03  07:59:02  kuipe_j
c dimension of element names improved
c
c Revision 1.2  1996/04/12  13:04:49  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:26  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer      nfcpmn ,nkfmmp ,np   ,nkapar   ,juer  ,istep  ,
     &             nstep  ,ncelm  ,ker
      integer      fd_nefis_res, kfmmap(nkfmmp),
     &             fcpcpr(nfcpmn),itim  (2)
      real         p1    (np,np) ,buf   (np*np)
      logical      first   ,writim,lfilt
c
c     Declaration of local variables
c
      integer      nentri, nkfmtm
      parameter   (nentri=1 ,nkfmtm=1)
      integer      errr  ,i ,ie ,nrerr, nceld  ,ii  ,jj    ,k  ,np1 ,
     &             lastcod 
      integer      kfmtim(nkfmtm)  ,code  (nentri)  ,usrord(1)      ,
     &             uindex(3)
      character*16 grnamm          ,grnamh          ,grnamd         ,
     &             name
      character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,
     &             nameac(nentri+1)
      character*64 descel(nentri)
      character*8  txt
      logical      llog
c
c     Declaration of external functions
c
      integer      putrel ,flsdat
      logical      yesmap
      external     putrel ,flsdat ,yesmap
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
      data  usrord /1/
c
c     Definition of elements.
c
      data (code(i) ,descel(i) ,nameel(i) ,quanel(i) ,
     &      unitel(i),i=1,nentri) /
c
     & 1 ,'Covariance parameter matrix' ,'HIS_cpm' ,'cpm', '-' /
c
      data  grnamm   ,grnamh ,grnamd/
     &      'KFCPM-MAP-GROUP' ,
     &      'UNUSED'          ,
     &      'KFCPM-DES-GROUP' /
c
      kfmtim(1) = 0
      lastcod   = nentri
c
c     Initialize.
c
      if (first) then
         nrerr = ekaboo
c
         call resadm (nentri ,code   ,fcpcpr )
c
         call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'KFCPM',
     &                nentri ,nkfmmp ,nkfmtm ,nkapar*nkapar  ,itim   ,
     &                nameel ,quanel ,unitel ,descel ,fcpcpr ,kfmmap ,
     &                kfmtim ,ncelm  ,nceld  ,nameac ,lastcod,errr   )
c
         if (errr.ne.0) goto 1000
      endif
c
      if (lfilt) then
c
c       Write Map results
c
        if (nkfmmp .gt. 3) then
          if (yesmap(kfmmap(1),kfmmap(2),kfmmap(3),istep)) then
            nrerr = ekamap
c
            call restim (fd_nefis_res, grnamm ,1  ,itim  ,ncelm  ,
     &                   nameac ,errr   )
            if (errr.ne.0) goto 1000
c
            call resdes ('KFCPM',fd_nefis_res, grnamd ,1 ,writim ,
     &                   kfmtim ,ncelm  ,nceld  ,errr   )
            if (errr.ne.0) goto 1000
c
            uindex(1) = ncelm
            uindex(2) = ncelm
            uindex(3) = 1
c
            do 30 i = 4,nkfmmp,2
               ie  = fcpcpr(kfmmap(i))+kfmmap(i+1)
c
c              Copy covariances of parameters to buffer
c
               k   = 0
               np1 = np - nkapar
               do 20 ii=1,nkapar
                  do 10 jj=1,nkapar
                     k = k+1
                     buf(k) = p1(ii+np1,jj+np1)
   10             continue
   20          continue
c
c              The element names of the Map block start with
c              MAP instead of HIS.
c
               name  = 'MAP'//nameel(ie)(4:)
               errr = putrel (fd_nefis_res, grnamm ,name   ,
     &                        uindex  ,usrord ,buf    )
               if (errr.ne.0) goto 1000
   30       continue
          endif
        endif
      endif
c
c     Be sure that at the last step the number of cells has been
c     written correctly.
c
      if (istep.ge.nstep) then
         nrerr = ekaeoo
         llog  = .true.
c
         call resdes ('KFCPM' ,fd_nefis_res, grnamd ,3 ,llog ,
     &                 kfmtim ,ncelm  ,nceld  ,errr   )
         if (errr.ne.0) goto 1000
c
         errr = flsdat (fd_nefis_res)
         if (errr.ne.0) goto 1000
      endif
c
      return
c
 1000 continue
c
      ker = fatal
      write (txt,'(i8)') errr
      call error (juer ,'KAFCPR @'//txt//'@' ,nrerr ,ker)
c
      end
