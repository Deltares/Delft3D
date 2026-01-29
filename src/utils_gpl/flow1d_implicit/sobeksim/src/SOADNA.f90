subroutine soadna ( strtim, nstru, strunm, qlatnm, qltpar,&
&strhis )

!=======================================================================
!            Rijkswaterstaat/RIZA
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sobeksim
!
! Programmer:         Niek Praagman (SEPRA BV)
!
! First version:      13-01-2004
!
! Module:             SOADNA (SOBEK)
!
! Module description: SOADNA is a helpmodule to determine the
!                     extra names in the case that structures are coupled
!                     to lateral discharges. If a lateral discharge is
!                     related to two structures the names get prefixes 1_
!                     and 2_ in combination with the lateral discharge
!                     name.
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!
! nstru                I  total number of structures in use
! strtim               I  array for histories of structure results
! nstru                I  total number of structures in use
! strunm              I,O array with names of structures
! qlatnm               I  array with names of lateral structures
! qltpar               I  array with lateral discharge parameters
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!
! None
!
!-----------------------------------------------------------------------
!
! Method:
!
! Determine whether there are structures without name.
! In that case check which lateral name is related and place this name
! in the appropriate position.
! Finally: If two structures (inflow and outflow) are used for one
! lateral discharge (retention) then give the names prefixes 1_ and 2_.
!
!=======================================================================
!
   integer nstru, strtim(*)

   real qltpar(9,*), strhis(13,*)

   character*40 strunm(*), qlatnm(*)

!
!    LOCAL PARAMETERS

   integer i, j, istmax, istru, iqlat

   character*40 name

!
!    Description of local parameters
!
!    i         loop variable
!    iqlat     loopvariable for lateral number
!    istmax    first number of nameless structure
!    istru     helpvariable for structure number
!    j         loop variable
!    name      helpvariable to store name of structure
!
! -----------------------------------------------------------------------

!     initialize istmax

   istmax = 0

!     determine whether there are nameless structures:

   do i = 1, nstru

      if ( strunm(i)(1:5) == '     ' .and. istmax == 0 ) then

         istmax = i

      endif

   enddo

!     determine lateral names if there are nameless structures:

   if ( strtim(1) .gt. 1 .and. istmax .gt. 0 ) then

      do i=2,strtim(1) + 1

         istru = i
         iqlat = strtim(i)

         if ( iqlat .gt. istmax .and.&
         &INT(qltpar(2,iqlat)).eq. 5 ) then

!                Adjust position of (lateral) structure:

            istru = MOD(INT(qltpar(9,iqlat)),1000)

            strtim(i)     = istru
            strunm(istru) = qlatnm(iqlat)

!                Check for two stations:

            if ( strtim(i-1) == istru ) then

!                   Two structures with the same name:

               istru = istru + 1

               strtim(i) = istru

               name(1:2)  = "1_"
               name(3:40) = strunm(istru-1)(1:38)
               strunm(istru-1)(1:40) = name(1:40)
               name(1:1)  = "2"
               strunm(istru  )(1:40) = name(1:40)

            endif

         endif

      enddo

   endif

!     --- Run through all structure names and consider retention
!         combinations
!         Not yet used (ARS11484): maybe later

   if ( 0 > 1 ) then

!     --- Skip this part

      do i = 1, nstru

         strhis(12,i) = -100E0

         write(*,*) 'Naam',i,' is ',strunm(i)(1:40)

         if ( strunm(i)(1:4) == 'Ret-' ) then

!           --- Retention Combination found

            strhis(12,i) = 0

         endif

      enddo

!     --- Check

      do i = 1, nstru

         if ( abs(strhis(12,i)) < 0.1 ) then

!           --- Retention combination:

            do j = i+1, nstru

               if ( abs(strhis(12,j)) < 0.1 ) then

!                 --- Check combination

                  if ( strunm(i)(5:7) == strunm(j)(5:7) ) then

!                    --- Check

                     strhis(12,i) = j
                     strhis(12,j) = i

                  endif

               endif

            enddo

         endif

      enddo

!     --- End part to be skipped

   endif

end

