      subroutine soadna ( strtim, nstru, strunm, qlatnm, qltpar, 
     +                    strhis )

c=======================================================================
c            Rijkswaterstaat/RIZA 
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sobeksim
c
c Programmer:         Niek Praagman (SEPRA BV)
c
c First version:      13-01-2004
c
c Module:             SOADNA (SOBEK)
c
c Module description: SOADNA is a helpmodule to determine the
c                     extra names in the case that structures are coupled 
c                     to lateral discharges. If a lateral discharge is 
c                     related to two structures the names get prefixes 1_ 
c                     and 2_ in combination with the lateral discharge
c                     name.
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION 
c 
c nstru                I  total number of structures in use
c strtim               I  array for histories of structure results 
c nstru                I  total number of structures in use
c strunm              I,O array with names of structures
c qlatnm               I  array with names of lateral structures
c qltpar               I  array with lateral discharge parameters
c 
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c
c None
c
c-----------------------------------------------------------------------
c
c Method:
c
c Determine whether there are structures without name.
c In that case check which lateral name is related and place this name
c in the appropriate position.
c Finally: If two structures (inflow and outflow) are used for one 
c lateral discharge (retention) then give the names prefixes 1_ and 2_.
c 
c=======================================================================
c
      integer nstru, strtim(*)

      real qltpar(9,*), strhis(13,*)

      character*40 strunm(*), qlatnm(*)

c
c    LOCAL PARAMETERS

      integer i, j, istmax, istru, iqlat

      character*40 name

c    
c    Description of local parameters
c
c    i         loop variable
c    iqlat     loopvariable for lateral number
c    istmax    first number of nameless structure
c    istru     helpvariable for structure number
c    j         loop variable
c    name      helpvariable to store name of structure
c
c -----------------------------------------------------------------------

c     initialize istmax

      istmax = 0

c     determine whether there are nameless structures:

      do i = 1, nstru

            if ( strunm(i)(1:5) == '     ' .and. istmax == 0 ) then

               istmax = i

            endif

      enddo

c     determine lateral names if there are nameless structures:

      if ( strtim(1) .gt. 1 .and. istmax .gt. 0 ) then

         do i=2,strtim(1) + 1

            istru = i 
            iqlat = strtim(i)

            if ( iqlat .gt. istmax .and.
     +           INT(qltpar(2,iqlat)).eq. 5 ) then

c                Adjust position of (lateral) structure:

                 istru = MOD(INT(qltpar(9,iqlat)),1000)

                 strtim(i)     = istru
                 strunm(istru) = qlatnm(iqlat)

c                Check for two stations:

                 if ( strtim(i-1) == istru ) then

c                   Two structures with the same name:

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
     
