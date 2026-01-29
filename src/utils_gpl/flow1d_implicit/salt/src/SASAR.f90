subroutine sasar (fd_nefis_res, nsaman ,nsamap ,nsatim ,ngrid  ,&
&itim   ,istep  ,nstep  ,first  ,writim ,juer   ,&
&salmap ,saltim ,csa2   ,disgr  ,ncelm  ,ncelh  ,&
&sacpre ,rho    ,buf    ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SASAR (SAlt SAlt Results)
!
! Module description: Writing of user selected salt results to the re-
!                     sult file.
!
!                     The user selected salt results at user supplied
!                     locations and time levels will be stored on the
!                     result file. The stored data can be processed
!                     further by the User Interface.
!
!                     The user can select functions of place and of
!                     time.
!
!                     Writing can start on a new file or in case the
!                     data model is unchanged an existing file can be
!                     extended.
!
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 21 buf(ngrid)        O  Buffer for results to be written to NEFIS.
! 15 csa2(ngrid)       I  Salt concentration in every grid point at time
!                         t(n+1).
!  2 dafdrs            P  -
!  1 defdrs            P  -
! 16 disgr(ngrid)      I  Dispersion coefficient in every grid point at
!                         time t(n+1).
! 10 first             I  True in case of first call.
!  8 istep             I  Current time step number (t(n+1)).
!  7 itim              P  -
! 12 juer              P  -
! 22 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 18 ncelh             I  Actual cell number of a history block of re-
!                         sult file.
! 17 ncelm             I  Actual cell number of a map block of result
!                         file.
!  6 ngrid             I  Number of grid points in network.
!  3 nsaman            I  Number of main codes of salt results.
!  4 nsamap            I  Number of entries in salmap.
!  5 nsatim            I  Number of entries in saltim.
!  9 nstep             I  Last time step number in simulation.
! 20 rho(ngrid)        I  Density of diluted water per grid point.
! 19 sacpre(nsaman)    I  sacpre(i) = index in block tabel (1..nentri)
!                         for main code i of salt results.
! 13 salmap(nsamap)    I  Parameter list for MAP block with salt
!                         results:
!                         (1)      = Report begin time
!                         (2)      = Report end time
!                         (3)      = Report time step
!                         (4)      = Report parameter 1 main code
!                         (5)      = Report parameter 1 sub code
!                         (i)      = Report parameter 1 main code
!                         (i+1)    = Report parameter 1 sub code
!                         (nsamap) = Report parameter n sub code
! 14 saltim(nsatim)    I  Parameter list for HIST block with salt re-
!                         sults:
!                         (1)      = Number of places
!                         (2)      = Report grid point 1
!                         (3)      = Report grid point 2
!                         (i)      = Report grid point n
!                         (i+1)    = Report parameter code 1
!                         (i+2)    = Report parameter sub code 1
!                         (nsatim) = Report parameter n sub code
! 11 writim            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! flsdat  FLuSh buffers of DATa file
! putrel  PUT Real ELement to a nefis file
! resadm  RESults; make ADMinistration for element selection
! resdes  RESults; DEScription group is defined
! resini  RESults; writing is INItialized
! restim  RESults; writing of current TIMe
! yesmap  YES MAP results are written
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sasar.pf,v $
! Revision 1.7  1997/06/18  08:21:26  kuipe_j
! remove unreferences
!
! Revision 1.6  1997/06/17  11:27:09  kuipe_j
! output in history format
!
! Revision 1.5  1996/12/02  15:31:43  kuipe_j
! Salt restart improved
!
! Revision 1.4  1996/09/03  14:54:35  kuipe_j
! frequency time hist,etc
!
! Revision 1.3  1995/05/30  09:56:15  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:06:14  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:56  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:01  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:15  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer      nsaman  ,nsamap,nsatim ,ngrid ,juer  ,istep  ,nstep ,&
   &ncelm   ,ncelh ,ker
   integer      fd_nefis_res, salmap(nsamap),&
   &saltim(nsatim) ,sacpre(nsaman),itim  (2)
   real         csa2(ngrid)    ,disgr(ngrid)  ,rho(ngrid),&
   &buf(ngrid)
   logical      first   ,writim
!
!     Declaration of local variables
!
   integer      nentri
   parameter   (nentri=3)
   integer      neferr  ,i ,j ,ie  ,nrerr  ,nlc  ,nsk   ,lastcod
   integer      usrord(1),&
   &uindex(3)
   character*16 grnamm          ,grnamh          ,grnamd         ,&
   &name
   character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,&
   &nameac(nentri+1)
   character*64 descel(nentri)
   character*8  txt
   logical      llog ,new  ,newuit
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     Declaration of external functions
!
   integer      putrel ,flsdat
   logical      yesmap
   external     putrel ,flsdat ,yesmap
!
   data  usrord /1/
!
!     Definition of elements.
!
   data (descel(i) ,nameel(i) ,quanel(i) ,&
   &unitel(i),i=1,nentri) /&
!
!      1
   &'Salt concentration'       ,'HIS_Cs'  ,'Cs' ,'kg/m3' ,&
!      2
   &'Dispersion coefficient'   ,'HIS_Ds'  ,'Ds' ,'m2/s'  ,&
!      3
   &'Density'                  ,'HIS_Rho' ,'Rho','kg/m3' /
!
   data  grnamm   ,grnamh ,grnamd/&
   &'SALT-MAP-GROUP' ,&
   &'SALT-HIS-GROUP' ,&
   &'SALT-DES-GROUP' /
!
   lastcod = nentri
!
!     Initialize.
!
   if (first) then
!
      nrerr = esaboo
!
!        call resadm (nentri ,code   ,sacpre )
!
      call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'SALT' ,&
      &nentri ,nsamap ,nsatim ,ngrid  ,itim   ,nameel ,&
      &quanel ,unitel ,descel ,sacpre ,salmap ,saltim ,&
      &ncelm  ,ncelh  ,nameac ,lastcod,neferr )
!
      if (neferr.ne.0) goto 1000
   endif
!
!     Write Map results
!
   if (nsamap .gt. 3 .and. ncelm .ge. 0) then
      if (yesmap(salmap(1),salmap(2),salmap(3),istep)) then
         nrerr = esamap
!
         call restim (fd_nefis_res, grnamm ,1  ,itim  ,ncelm  ,&
         &nameac ,neferr )
         if (neferr.ne.0) goto 1000
!
         call resdes ('SALT' ,fd_nefis_res, grnamd ,1 ,writim ,&
         &saltim ,ncelm  ,ncelh  ,neferr )
         if (neferr.ne.0) goto 1000
!
         uindex(1) = ncelm
         uindex(2) = ncelm
         uindex(3) = 1
!
         do 10 i = 4,nsamap,2
            ie = sacpre(salmap(i))+salmap(i+1)
            if (ie.le.lastcod) then
!
!              Codes > lastcod can only be used in his format
!
!              The element names of the Map block start with
!              MAP instead of HIS.
!
               name  = 'MAP'//nameel(ie)(4:)
               if (ie.eq.1) then
                  neferr = putrel (fd_nefis_res, grnamm ,name   ,&
                  &uindex  ,usrord ,csa2   )
                  if (neferr.ne.0) goto 1000
               else if (ie.eq.2) then
                  neferr = putrel (fd_nefis_res, grnamm ,name   ,&
                  &uindex  ,usrord ,disgr  )
                  if (neferr.ne.0) goto 1000
               else if (ie.eq.3) then
                  neferr = putrel (fd_nefis_res, grnamm ,name   ,&
                  &uindex  ,usrord ,rho    )
                  if (neferr.ne.0) goto 1000
               endif
            endif
10       continue
      endif
   endif
!
!     Write History results.
!
   nlc = saltim(1)
   new = mod(nsatim-nlc,2) .eq. 0

   if ( new ) then
      nsk = 3
   else
      nsk = 0
   endif
!
   if (nsatim .gt. 1+nsk .and. ncelh .ge. 0) then
      if (new) then
         newuit = yesmap(saltim(nlc+2),saltim(nlc+3),saltim(nlc+4),&
         &istep)
      else
         newuit = .false.
      endif
      if ( (saltim(1).gt.0 .and. .not. new) .or. newuit ) then
         nrerr = esahis
!
         call restim (fd_nefis_res, grnamh ,1  ,itim  ,ncelh  ,&
         &nameac ,neferr )
         if (neferr.ne.0) goto 1000
!
         call resdes ('SALT' ,fd_nefis_res, grnamd ,2 ,writim ,&
         &saltim ,ncelm  ,ncelh  ,neferr )
         if (neferr.ne.0) goto 1000
!
         uindex(1) = ncelh
         uindex(2) = ncelh
         uindex(3) = 1
!
         do 50 i = saltim(1)+2+nsk,nsatim,2
            ie = sacpre(saltim(i))+saltim(i+1)
            if (ie.le.lastcod) then
!
!              Codes > lastcod can only be used in his format
!
               if (ie.eq.1) then
                  do 20 j=1,saltim(1)
                     buf(j) = csa2(saltim(j+1))
20                continue
                  neferr = putrel (fd_nefis_res, grnamh ,nameel(ie) ,&
                  &uindex  ,usrord ,buf    )
                  if (neferr.ne.0) goto 1000
               else if (ie.eq.2) then
                  do 30 j=1,saltim(1)
                     buf(j) = disgr(saltim(j+1))
30                continue
                  neferr = putrel (fd_nefis_res, grnamh ,nameel(ie) ,&
                  &uindex  ,usrord ,buf    )
                  if (neferr.ne.0) goto 1000
               else if (ie.eq.3) then
                  do 40 j=1,saltim(1)
                     buf(j) = rho(saltim(j+1))
40                continue
                  neferr = putrel (fd_nefis_res, grnamh ,nameel(ie) ,&
                  &uindex  ,usrord ,buf    )
                  if (neferr.ne.0) goto 1000
               endif
            endif
50       continue
      endif
   endif
!
!     Be sure that at the last step the number of cells has been
!     written correctly.
!
   if (istep.ge.nstep) then
      nrerr = esaeoo
!
      llog = .true.
      call resdes ('SALT'  ,fd_nefis_res, grnamd ,3 ,llog ,&
      &saltim ,ncelm  ,ncelh  ,neferr )
      if (neferr.ne.0) goto 1000
!
      neferr = flsdat (fd_nefis_res)
      if (neferr.ne.0) goto 1000
   endif
!
   return
!
1000 continue
!
   ker = fatal
   write (txt,'(i8)') neferr
   call error (juer ,'SASAR @'//txt//'@' ,nrerr ,ker)
!
end
