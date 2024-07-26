      subroutine sasar (fd_nefis_res, nsaman ,nsamap ,nsatim ,ngrid  ,
     &                  itim   ,istep  ,nstep  ,first  ,writim ,juer   ,
     &                  salmap ,saltim ,csa2   ,disgr  ,ncelm  ,ncelh  ,
     &                  sacpre ,rho    ,buf    ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SASAR (SAlt SAlt Results)
c
c Module description: Writing of user selected salt results to the re-
c                     sult file.
c
c                     The user selected salt results at user supplied
c                     locations and time levels will be stored on the
c                     result file. The stored data can be processed
c                     further by the User Interface.
c
c                     The user can select functions of place and of
c                     time.
c
c                     Writing can start on a new file or in case the
c                     data model is unchanged an existing file can be
c                     extended.
c
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 21 buf(ngrid)        O  Buffer for results to be written to NEFIS.
c 15 csa2(ngrid)       I  Salt concentration in every grid point at time
c                         t(n+1).
c  2 dafdrs            P  -
c  1 defdrs            P  -
c 16 disgr(ngrid)      I  Dispersion coefficient in every grid point at
c                         time t(n+1).
c 10 first             I  True in case of first call.
c  8 istep             I  Current time step number (t(n+1)).
c  7 itim              P  -
c 12 juer              P  -
c 22 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 18 ncelh             I  Actual cell number of a history block of re-
c                         sult file.
c 17 ncelm             I  Actual cell number of a map block of result
c                         file.
c  6 ngrid             I  Number of grid points in network.
c  3 nsaman            I  Number of main codes of salt results.
c  4 nsamap            I  Number of entries in salmap.
c  5 nsatim            I  Number of entries in saltim.
c  9 nstep             I  Last time step number in simulation.
c 20 rho(ngrid)        I  Density of diluted water per grid point.
c 19 sacpre(nsaman)    I  sacpre(i) = index in block tabel (1..nentri)
c                         for main code i of salt results.
c 13 salmap(nsamap)    I  Parameter list for MAP block with salt
c                         results:
c                         (1)      = Report begin time
c                         (2)      = Report end time
c                         (3)      = Report time step
c                         (4)      = Report parameter 1 main code
c                         (5)      = Report parameter 1 sub code
c                         (i)      = Report parameter 1 main code
c                         (i+1)    = Report parameter 1 sub code
c                         (nsamap) = Report parameter n sub code
c 14 saltim(nsatim)    I  Parameter list for HIST block with salt re-
c                         sults:
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                         (i+1)    = Report parameter code 1
c                         (i+2)    = Report parameter sub code 1
c                         (nsatim) = Report parameter n sub code
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
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sasar.pf,v $
c Revision 1.7  1997/06/18  08:21:26  kuipe_j
c remove unreferences
c
c Revision 1.6  1997/06/17  11:27:09  kuipe_j
c output in history format
c
c Revision 1.5  1996/12/02  15:31:43  kuipe_j
c Salt restart improved
c
c Revision 1.4  1996/09/03  14:54:35  kuipe_j
c frequency time hist,etc
c
c Revision 1.3  1995/05/30  09:56:15  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:06:14  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:56  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:01  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:15  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer      nsaman  ,nsamap,nsatim ,ngrid ,juer  ,istep  ,nstep ,
     &             ncelm   ,ncelh ,ker
      integer      fd_nefis_res, salmap(nsamap),
     &             saltim(nsatim) ,sacpre(nsaman),itim  (2)
      real         csa2(ngrid)    ,disgr(ngrid)  ,rho(ngrid),
     &             buf(ngrid)
      logical      first   ,writim
c
c     Declaration of local variables
c
      integer      nentri
      parameter   (nentri=3)
      integer      neferr  ,i ,j ,ie  ,nrerr  ,nlc  ,nsk   ,lastcod
      integer      usrord(1),
     &             uindex(3)
      character*16 grnamm          ,grnamh          ,grnamd         ,
     &             name
      character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,
     &             nameac(nentri+1)
      character*64 descel(nentri)
      character*8  txt
      logical      llog ,new  ,newuit   
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
c
c      1
     &    'Salt concentration'       ,'HIS_Cs'  ,'Cs' ,'kg/m3' ,
c      2 
     &    'Dispersion coefficient'   ,'HIS_Ds'  ,'Ds' ,'m2/s'  ,
c      3
     &    'Density'                  ,'HIS_Rho' ,'Rho','kg/m3' /
c
      data  grnamm   ,grnamh ,grnamd/
     &      'SALT-MAP-GROUP' ,
     &      'SALT-HIS-GROUP' ,
     &      'SALT-DES-GROUP' /
c
      lastcod = nentri
c
c     Initialize.
c
      if (first) then
c 
         nrerr = esaboo
c
c        call resadm (nentri ,code   ,sacpre )
c
         call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'SALT' ,
     &                nentri ,nsamap ,nsatim ,ngrid  ,itim   ,nameel ,
     &                quanel ,unitel ,descel ,sacpre ,salmap ,saltim ,
     &                ncelm  ,ncelh  ,nameac ,lastcod,neferr )
c
         if (neferr.ne.0) goto 1000
      endif
c
c     Write Map results
c
      if (nsamap .gt. 3 .and. ncelm .ge. 0) then
         if (yesmap(salmap(1),salmap(2),salmap(3),istep)) then
            nrerr = esamap
c
            call restim (fd_nefis_res, grnamm ,1  ,itim  ,ncelm  ,
     &                   nameac ,neferr )
            if (neferr.ne.0) goto 1000
c
            call resdes ('SALT' ,fd_nefis_res, grnamd ,1 ,writim ,
     &                   saltim ,ncelm  ,ncelh  ,neferr )
            if (neferr.ne.0) goto 1000
c
            uindex(1) = ncelm
            uindex(2) = ncelm
            uindex(3) = 1
c
            do 10 i = 4,nsamap,2
               ie = sacpre(salmap(i))+salmap(i+1)
               if (ie.le.lastcod) then
c             
c              Codes > lastcod can only be used in his format             
c
c              The element names of the Map block start with
c              MAP instead of HIS.
c
               name  = 'MAP'//nameel(ie)(4:)
               if (ie.eq.1) then
                  neferr = putrel (fd_nefis_res, grnamm ,name   ,
     &                             uindex  ,usrord ,csa2   )
                  if (neferr.ne.0) goto 1000
               else if (ie.eq.2) then
                  neferr = putrel (fd_nefis_res, grnamm ,name   ,
     &                             uindex  ,usrord ,disgr  )
                  if (neferr.ne.0) goto 1000
               else if (ie.eq.3) then
                  neferr = putrel (fd_nefis_res, grnamm ,name   ,
     &                             uindex  ,usrord ,rho    )
                  if (neferr.ne.0) goto 1000
               endif
               endif
   10       continue
         endif
      endif
c
c     Write History results.
c
      nlc = saltim(1)
      new = mod(nsatim-nlc,2) .eq. 0

      if ( new ) then
         nsk = 3
      else
         nsk = 0
      endif
c
      if (nsatim .gt. 1+nsk .and. ncelh .ge. 0) then
         if (new) then
            newuit = yesmap(saltim(nlc+2),saltim(nlc+3),saltim(nlc+4),
     &                      istep)
         else
            newuit = .false.
         endif
         if ( (saltim(1).gt.0 .and. .not. new) .or. newuit ) then
            nrerr = esahis
c
            call restim (fd_nefis_res, grnamh ,1  ,itim  ,ncelh  ,
     &                   nameac ,neferr )
            if (neferr.ne.0) goto 1000
c
            call resdes ('SALT' ,fd_nefis_res, grnamd ,2 ,writim ,
     &                   saltim ,ncelm  ,ncelh  ,neferr )
            if (neferr.ne.0) goto 1000
c
            uindex(1) = ncelh
            uindex(2) = ncelh
            uindex(3) = 1
c
            do 50 i = saltim(1)+2+nsk,nsatim,2
               ie = sacpre(saltim(i))+saltim(i+1)
               if (ie.le.lastcod) then
c             
c              Codes > lastcod can only be used in his format             
c                             
               if (ie.eq.1) then
                  do 20 j=1,saltim(1)
                     buf(j) = csa2(saltim(j+1))
   20             continue
                  neferr = putrel (fd_nefis_res, grnamh ,nameel(ie) ,
     &                             uindex  ,usrord ,buf    )
                  if (neferr.ne.0) goto 1000
               else if (ie.eq.2) then
                  do 30 j=1,saltim(1)
                     buf(j) = disgr(saltim(j+1))
   30             continue
                  neferr = putrel (fd_nefis_res, grnamh ,nameel(ie) ,
     &                             uindex  ,usrord ,buf    )
                  if (neferr.ne.0) goto 1000
               else if (ie.eq.3) then
                  do 40 j=1,saltim(1)
                     buf(j) = rho(saltim(j+1))
   40             continue
                  neferr = putrel (fd_nefis_res, grnamh ,nameel(ie) ,
     &                             uindex  ,usrord ,buf    )
                  if (neferr.ne.0) goto 1000
               endif
               endif
   50       continue
         endif
      endif
c
c     Be sure that at the last step the number of cells has been
c     written correctly.
c
      if (istep.ge.nstep) then
         nrerr = esaeoo
c
         llog = .true.
         call resdes ('SALT'  ,fd_nefis_res, grnamd ,3 ,llog ,
     &                 saltim ,ncelm  ,ncelh  ,neferr )
         if (neferr.ne.0) goto 1000
c
         neferr = flsdat (fd_nefis_res)
         if (neferr.ne.0) goto 1000
      endif
c
      return
c
 1000 continue
c
      ker = fatal
      write (txt,'(i8)') neferr
      call error (juer ,'SASAR @'//txt//'@' ,nrerr ,ker)
c
      end
