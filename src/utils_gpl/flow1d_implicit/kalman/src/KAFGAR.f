      subroutine KAFGAR(fd_nefis_res, nfgamn ,nkfgmp ,itim   ,
     &                  istep  ,nstep  ,first  ,writim ,lfilt  ,juer   ,
     &                  kfgmap ,ncelm  ,fgacpr ,np     ,nsamp  ,
     &                  kgain  ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAFGAR (KAlman Filter kalman GAin Results)
c
c Module description: Write the Kalman gain matrix to the result file.
c
c                     The stored data can be processed further by the
c                     User Interface. It is in map format but as it is a
c                     2D-array, it cannot be used in the same way as
c                     other map functions. See [S-DD-002.1MR] for a
c                     specification of the nefis names.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 dafdrs            P  -
c  1 defdrs            P  -
c 14 fgacpr(nfgamn)    I  fgacpr(i) = index in block table (1...nentri)
c                         for main code i of the Kalman gain.
c  8 first             I  True in case of first call.
c  6 istep             I  Current time step number (t(n+1)).
c  5 itim              P  -
c 11 juer              P  -
c 18 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 12 kfgmap(*)         I  Parameter list for Kalman gain (MAP block).
c                         (1/2/3) Report step of begin, end, resp.
c                         increment.
c                         (4) Main code parameter 1.
c                         (5) Sub code parameter 1.
c 17 kgain             P  -
c 10 lfilt             I  = True if a filter step must be performed.
c 13 ncelm             I  Actual cell number of a map block of result
c                         file.
c  3 nfgamn            I  Number of main codes of the Kalman gain.
c  4 nkfgmp            I  Number of entries in kfgmap.
c 15 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c 16 nsamp             I  Number of hydrodynamic samples (measurements)
c  7 nstep             I  Last time step number in simulation.
c  9 writim            P  -
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
c $Log: kafgar.pf,v $
c Revision 1.3  1996/12/03  07:59:03  kuipe_j
c dimension of element names improved
c
c Revision 1.2  1996/04/12  13:04:50  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:27  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer      nfgamn ,nkfgmp ,juer   ,istep  ,
     &             nstep  ,ncelm  ,np     ,nsamp  ,ker
      integer      fd_nefis_res, kfgmap(nkfgmp),
     &             fgacpr(nfgamn) ,itim  (2)
      real         kgain(np*nsamp)
      logical      first   ,writim,lfilt
c
c     Declaration of local variables
c
      integer      nentri, nkfgtm
      parameter   (nentri=1 ,nkfgtm=1)
      integer      errr  ,i ,ie ,nrerr, nceld, lastcod
      integer      kfgtim(nkfgtm)  ,code  (nentri)  ,usrord(1)      ,
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
     & 1 ,'Kalman gain' ,'HIS_Kg' ,'Kg', '-' /
c
      data  grnamm   ,grnamh ,grnamd/
     &      'KFGAN-MAP-GROUP' ,
     &      'UNUSED'          ,
     &      'KFGAN-DES-GROUP' /
c
      kfgtim(1) = 0
      lastcod   = nentri
c
c     Initialize.
c
      if (first) then
         nrerr = ekaboo
c
         call resadm (nentri ,code   ,fgacpr )
c
         call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'KFGAN',
     &                nentri ,nkfgmp ,nkfgtm ,np*nsamp       ,itim   ,
     &                nameel ,quanel ,unitel ,descel ,fgacpr ,kfgmap ,
     &                kfgtim ,ncelm  ,nceld  ,nameac ,lastcod,errr   )
c
         if (errr.ne.0) goto 1000
      endif
c
      if (lfilt) then
c
c       Write Map results
c
        if (nkfgmp .gt. 3) then
          if (yesmap(kfgmap(1),kfgmap(2),kfgmap(3),istep)) then
            nrerr = ekamap
c
            call restim (fd_nefis_res, grnamm ,1  ,itim  ,ncelm  ,
     &                   nameac ,errr   )
            if (errr.ne.0) goto 1000
c
            call resdes ('KFGAN',fd_nefis_res, grnamd ,1 ,writim ,
     &                   kfgtim ,ncelm  ,nceld  ,errr   )
            if (errr.ne.0) goto 1000
c
            uindex(1) = ncelm
            uindex(2) = ncelm
            uindex(3) = 1
c
            do 20 i = 4,nkfgmp,2
               ie = fgacpr(kfgmap(i))+kfgmap(i+1)
c
c              The element names of the Map block start with
c              MAP instead of HIS.
c
               name  = 'MAP'//nameel(ie)(4:)
               errr = putrel (fd_nefis_res, grnamm ,name   ,
     &                        uindex  ,usrord ,kgain  )
               if (errr.ne.0) goto 1000
   20       continue
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
         call resdes ('KFGAN' ,fd_nefis_res, grnamd ,3 ,llog ,
     &                 kfgtim ,ncelm  ,nceld  ,errr   )
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
      call error (juer ,'KAFGAR @'//txt//'@' ,nrerr ,ker)
c
      end
