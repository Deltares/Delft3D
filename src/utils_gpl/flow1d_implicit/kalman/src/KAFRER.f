      subroutine KAFRER(fd_nefis_res, nfremn ,nkfrmp ,nkfrtm ,nsamp  ,
     &                  itim   ,istep  ,nstep  ,first  ,writim ,lfilt  ,
     &                  juer   ,kfrmap ,kfrtim ,res    ,scares ,rescov ,
     &                  ncelm  ,ncelh  ,frecpr ,buf    ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAFRER (KAlman Filter REsidual Results)
c
c Module description: Write the user selected Kalman module results to
c                     the result file. The results processed in this
c                     module are scaled residuals and covariances of
c                     residuals.
c
c                     The stored data can be processed further by the
c                     User Interface. The user can select functions of
c                     place or functions of time. See [S-DD-002.1MR] for
c                     a specification of the nefis names.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 22 buf(ngrid)        O  Buffer for results to be written to NEFIS.
c  2 dafdrs            P  -
c  1 defdrs            P  -
c 10 first             I  True in case of first call.
c 21 frecpr(nfremn)    I  frecpr(i) = index in block table (1...nentri)
c                         for main code i of the residuals.
c  8 istep             I  Current time step number (t(n+1)).
c  7 itim              P  -
c 13 juer              P  -
c 23 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 14 kfrmap(*)         I  Parameter list for residuals MAP block
c                         (1/2/3) Report step of begin, end, resp.
c                         increment.
c                         (4) Main code parameter 1.
c                         (5) Sub code parameter 1.
c                         (.) Etc.
c 15 kfrtim(*)         I  Parameter list for  residuals HIST
c                         block
c                         (1) Number of places.
c                         (2) Location 1.
c                         (i) Location n.
c                         (i+1) Main code parameter 1.
c                         (i+2) Sub code parameter 1.
c                         (.) Etc.
c 12 lfilt             I  = True if a filter step must be performed.
c 20 ncelh             I  Actual cell number of a history block of re-
c                         sult file.
c 19 ncelm             I  Actual cell number of a map block of result
c                         file.
c  3 nfremn            I  Number of main codes of residuals.
c  4 nkfrmp            I  Number of entries in kfrmap.
c  5 nkfrtm            I  Number of entries in kfrtim.
c  6 nsamp             I  Number of hydrodynamic samples (measurements)
c  9 nstep             I  Last time step number in simulation.
c 16 res(nsamp)        I  Residual vector
c 18 rescov(nsamp,     I  Matrix with covariances of residuals.
c      ,nsamp)
c 17 scares(nsamp)     I  Scaled residual vector
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
c $Log: kafrer.pf,v $
c Revision 1.5  1999/03/15  15:51:50  kuipe_j
c tabs removed
c
c Revision 1.4  1996/12/03  07:59:06  kuipe_j
c dimension of element names improved
c
c Revision 1.3  1996/09/03  14:54:24  kuipe_j
c frequency time hist,etc
c
c Revision 1.2  1996/04/12  13:04:56  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:34  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer      nfremn ,nkfrmp ,nkfrtm ,nsamp  ,juer   ,istep  ,
     &             nstep  ,ncelm  ,ncelh  ,ker
      integer      fd_nefis_res, kfrmap(nkfrmp)  ,
     &             kfrtim(nkfrtm) ,frecpr(nfremn),itim  (2)
      real         buf(nsamp)     ,res(nsamp)    ,scares(nsamp)   ,
     &             rescov(nsamp,nsamp)
      logical      first  ,writim ,lfilt
c
c     Declaration of local variables
c
      integer      nentri
      parameter   (nentri=3)
      integer      errr  ,i ,j ,ie ,nrerr, ismp, nsk, nlc, lastcod
      integer      code  (nentri)   ,usrord(1),
     &             uindex(3)
      character*16 grnamm          ,grnamh          ,grnamd         ,
     &             name
      character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,
     &             nameac(nentri+1)
      character*64 descel(nentri)
      character*8  txt
      logical      llog, new    ,newuit
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
     & 1 ,'residual'            ,'HIS_rs' ,'rs', '-' ,
     & 1 ,'Scaled residual'     ,'HIS_sr' ,'sr', '-' ,
     & 1 ,'Residual covariance' ,'HIS_rc' ,'rc', '-' /
c
      data  grnamm   ,grnamh ,grnamd/
     &      'KFRES-MAP-GROUP' ,
     &      'KFRES-HIS-GROUP' ,
     &      'KFRES-DES-GROUP' /
c
      lastcod = nentri 
c
c     Initialize.
c
      if (first) then
         nrerr = ekaboo
c
         call resadm (nentri ,code   ,frecpr )
c
         call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'KFRES',
     &                nentri ,nkfrmp ,nkfrtm ,nsamp  ,itim   ,nameel ,
     &                quanel ,unitel ,descel ,frecpr ,kfrmap ,kfrtim ,
     &                ncelm  ,ncelh  ,nameac ,lastcod,errr   )
c
         if (errr.ne.0) goto 1000
      endif
c
      if (lfilt) then
c
c       Write Map results
c
        if (nkfrmp .gt. 3) then
          if (yesmap(kfrmap(1),kfrmap(2),kfrmap(3),istep)) then
            nrerr = ekamap
c
            call restim (fd_nefis_res, grnamm ,1  ,itim  ,ncelm  ,
     &                   nameac ,errr   )
            if (errr.ne.0) goto 1000
c
            call resdes ('KFRES',fd_nefis_res, grnamd ,1 ,writim ,
     &                   kfrtim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
c
            uindex(1) = ncelm
            uindex(2) = ncelm
            uindex(3) = 1
c
            do 50 i = 4,nkfrmp,2
               ie = frecpr(kfrmap(i))+kfrmap(i+1)
c
c              The element names of the Map block start with
c              MAP instead of HIS.
c
               name  = 'MAP'//nameel(ie)(4:)
               goto (10,20,30) , ie
   10          continue
                  do 15 j = 1, nsamp
                     buf(j) = res(j)
   15             continue
                  goto 40
   20          continue
                  do 25 j = 1, nsamp
                     buf(j) = scares(j)
   25             continue
                  goto 40
   30          continue
                  do 35 j = 1, nsamp
                     buf(j) = rescov(j,j)
   35             continue
                  goto 40
   40          continue
                  errr = putrel (fd_nefis_res, grnamm ,name   ,
     &                           uindex  ,usrord ,buf    )
                  if (errr.ne.0) goto 1000
   50       continue
          endif
        endif
c
c       Write History results.
c
        nlc = kfrtim(1)
        new = mod(nkfrtm-nlc,2) .eq. 0

        if ( new ) then
           nsk = 3
        else
           nsk = 0
        endif
c
        if (nkfrtm .gt. 1+nsk) then
          if (new) then
            newuit = yesmap(kfrtim(nlc+2),kfrtim(nlc+3),kfrtim(nlc+4),
     &                      istep)
          else
            newuit = .false.
          endif
          if ( (kfrtim(1).gt.0 .and. .not. new) .or. newuit ) then
            nrerr = ekahis
c
            call restim (fd_nefis_res, grnamh ,1  ,itim  ,ncelh  ,
     &                   nameac ,errr   )
            if (errr.ne.0) goto 1000
c
            call resdes ('KFRES',fd_nefis_res, grnamd ,2 ,writim ,
     &                   kfrtim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
c
            uindex(1) = ncelh
            uindex(2) = ncelh
            uindex(3) = 1
c
            do 100 i = kfrtim(1)+2+nsk,nkfrtm,2
               ie = frecpr(kfrtim(i))+kfrtim(i+1)
               goto (60,70,80) ,ie
   60          continue
                  do 65 j=1,kfrtim(1)
                     ismp = kfrtim(j+1)
                     buf(j) = res(ismp)
   65             continue
                  goto 90
   70          continue
                  do 75 j=1,kfrtim(1)
                     ismp = kfrtim(j+1)
                     buf(j) = scares(ismp)
   75             continue
                  goto 90
   80          continue
                  do 85 j=1,kfrtim(1)
                     ismp = kfrtim(j+1)
                     buf(j) = rescov(ismp,ismp)
   85             continue
                  goto 90
   90          continue
                  errr = putrel (fd_nefis_res, grnamh ,nameel(ie),
     &                           uindex  ,usrord ,buf    )
                  if (errr.ne.0) goto 1000
  100       continue
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
         call resdes ('KFRES' ,fd_nefis_res, grnamd ,3 ,llog ,
     &                 kfrtim ,ncelm  ,ncelh  ,errr   )
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
      call error (juer ,'KAFRER @'//txt//'@' ,nrerr ,ker)
c
      end
