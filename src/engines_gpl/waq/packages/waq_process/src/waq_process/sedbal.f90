module m_sedbal

implicit none

contains

     subroutine SEDBAL     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!XXX DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'SEDBAL' :: SEDBAL
!
!*******************************************************************************
!
     use m_evaluate_waq_attribute
      use layered_sediment

      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint(*)  ! I  Array of pointers in pmsa to get and store the data
      integer increm(*)  ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
!
!*******************************************************************************

      integer,parameter :: ip_balout = 1
      integer,parameter :: ip_itime = 2
      integer,parameter :: ip_delt = 3
      integer,parameter :: last = 3
      integer balout
      integer itime
      real    delt

      ! INPUT: first the above parameters
      ! next the masses of all subs
      ! next the water transport fluxes of all subs
      ! next the sediment biochemistry fluxes

      ! sediment substances definition - module layered_sediment

      ! fluxes
      integer,parameter :: nofl = 79
     integer,parameter :: if_dAdsPO4AAP = 1
     integer,parameter :: if_dNitrif = 2
      integer,parameter :: if_dCnvPPOC1 = 3
      integer,parameter :: if_dCnvPPON1 = 4
      integer,parameter :: if_dCnvPPOP1 = 5
      integer,parameter :: if_dCnvPPOS1 = 6
      integer,parameter :: if_dCnvDPOC1 = 7
      integer,parameter :: if_dCnvDPON1 = 8
      integer,parameter :: if_dCnvDPOP1 = 9
      integer,parameter :: if_dCnvDPOS1 = 10
      integer,parameter :: if_dMinPOC1G = 11
      integer,parameter :: if_dMinPON1 = 12
      integer,parameter :: if_dMinPOP1 = 13
      integer,parameter :: if_dMinPOS1 = 14
      integer,parameter :: if_dCnvPPOC2 = 15
      integer,parameter :: if_dCnvPPON2 = 16
      integer,parameter :: if_dCnvPPOP2 = 17
      integer,parameter :: if_dCnvPPOS2 = 18
      integer,parameter :: if_dCnvDPOC2 = 19
      integer,parameter :: if_dCnvDPON2 = 20
      integer,parameter :: if_dCnvDPOP2 = 21
      integer,parameter :: if_dCnvDPOS2 = 22
      integer,parameter :: if_dMinPOC2G = 23
      integer,parameter :: if_dMinPON2 = 24
      integer,parameter :: if_dMinPOP2 = 25
      integer,parameter :: if_dMinPOS2 = 26
      integer,parameter :: if_dCnvPPOC3 = 27
      integer,parameter :: if_dCnvPPON3 = 28
      integer,parameter :: if_dCnvPPOP3 = 29
      integer,parameter :: if_dCnvPPOS3 = 30
      integer,parameter :: if_dCnvDPOC3 = 31
      integer,parameter :: if_dCnvDPON3 = 32
      integer,parameter :: if_dCnvDPOP3 = 33
      integer,parameter :: if_dCnvDPOS3 = 34
      integer,parameter :: if_dMinPOC3G = 35
      integer,parameter :: if_dMinPON3 = 36
      integer,parameter :: if_dMinPOP3 = 37
      integer,parameter :: if_dMinPOS3 = 38
      integer,parameter :: if_dCnvPPOC4 = 39
      integer,parameter :: if_dCnvPPON4 = 40
      integer,parameter :: if_dCnvPPOP4 = 41
      integer,parameter :: if_dCnvPPOS4 = 42
      integer,parameter :: if_dCnvDPOC4 = 43
      integer,parameter :: if_dCnvDPON4 = 44
      integer,parameter :: if_dCnvDPOP4 = 45
      integer,parameter :: if_dCnvDPOS4 = 46
      integer,parameter :: if_dMinPOC4G = 47
      integer,parameter :: if_dMinPON4 = 48
      integer,parameter :: if_dMinPOP4 = 49
      integer,parameter :: if_dMinPOS4 = 50
      integer,parameter :: if_dCnvXDOC = 51
      integer,parameter :: if_dCnvXDON = 52
      integer,parameter :: if_dCnvXDOP = 53
      integer,parameter :: if_dCnvXDOS = 54
      integer,parameter :: if_dCnvYDOC = 55
      integer,parameter :: if_dCnvYDON = 56
      integer,parameter :: if_dCnvYDOP = 57
      integer,parameter :: if_dCnvYDOS = 58
      integer,parameter :: if_dMinDOCG = 59
      integer,parameter :: if_dMinDON = 60
      integer,parameter :: if_dMinDOP = 61
      integer,parameter :: if_dMinDOS = 62
      integer,parameter :: if_dPrecipPO4 = 63
      integer,parameter :: if_dDissolPO4 = 64
      integer,parameter :: if_dDissolSi = 65
      integer,parameter :: if_dOxCon = 66
      integer,parameter :: if_dNiDen = 67
      integer,parameter :: if_dFeRed = 68
      integer,parameter :: if_dSuRed = 69
      integer,parameter :: if_dMetGenCH4 = 70
      integer,parameter :: if_dMetGenCO2 = 71
      integer,parameter :: if_dMetEbul = 72
      integer,parameter :: if_dSUDox = 73
      integer,parameter :: if_dSUDprec = 74
      integer,parameter :: if_dSUDdis = 75
      integer,parameter :: if_dMethoxDO = 76
      integer,parameter :: if_dMethoxSu = 77
      integer,parameter :: if_dPrecAPPO4 = 78
      integer,parameter :: if_dDissAPPO4 = 79

      integer,parameter :: notermax = 20
      real*8   balterm(nototsed,0:notermax), flxcum(nofl), watcum(nototsed), error
      integer  noterm(nototsed),notermtot,iterm
      character*20 baname(nototsed,notermax), duname
      character*160 moname
      real     stochi(nototsed,nofl), mass, flx
      integer  isys, ip, ifl, iseg, iatt1, iatt2
      character*6 subnam(nototsed)
      character*13 flxnam(nofl)
      integer,parameter :: one = 1

      integer, save     :: lunsedbal

      logical first
      data first /.true./
      save

      !
!******************************************************************************* INITIAL PROCESSING

      if (first) then
          ! substance names
          subnam(1) = 'CH4'
          subnam(2) = 'DOC'
          subnam(3) = 'DON'
          subnam(4) = 'DOP'
          subnam(5) = 'DOS'
          subnam(6) = 'NH4'
          subnam(7) = 'NO3'
          subnam(8) = 'OXY'
          subnam(9) = 'PO4'
          subnam(10) = 'Si'
          subnam(11) = 'SO4'
          subnam(12) = 'SUD'
          subnam(13) = 'AAP'
          subnam(14) = 'APATP'
          subnam(15) = 'FeIIIpa'
          subnam(16) = 'Opal'
          subnam(17) = 'POC1'
          subnam(18) = 'POC2'
          subnam(19) = 'POC3'
          subnam(20) = 'POC4'
          subnam(21) = 'PON1'
          subnam(22) = 'PON2'
          subnam(23) = 'PON3'
          subnam(24) = 'PON4'
          subnam(25) = 'POP1'
          subnam(26) = 'POP2'
          subnam(27) = 'POP3'
          subnam(28) = 'POP4'
          subnam(29) = 'POS1'
          subnam(30) = 'POS2'
          subnam(31) = 'POS3'
          subnam(32) = 'POS4'
          subnam(33) = 'SUP'
          subnam(34) = 'VIVP'

          ! define names (outflx.csv)
          flxnam(1) = 'dAdsPO4'
          flxnam(2) = 'dNitrif'
         flxnam(3) = 'dCnvPPOC1'
          flxnam(4) = 'dCnvPPON1'
          flxnam(5) = 'dCnvPPOP1'
          flxnam(6) = 'dCnvPPOS1'
          flxnam(7) = 'dCnvDPOC1'
          flxnam(8) = 'dCnvDPON1'
          flxnam(9) = 'dCnvDPOP1'
          flxnam(10) = 'dCnvDPOS1'
          flxnam(11) = 'dMinPOC1G'
          flxnam(12) = 'dMinPON1'
          flxnam(13) = 'dMinPOP1'
          flxnam(14) = 'dMinPOS1'
          flxnam(15) = 'dCnvPPOC2'
          flxnam(16) = 'dCnvPPON2'
          flxnam(17) = 'dCnvPPOP2'
          flxnam(18) = 'dCnvPPOS2'
          flxnam(19) = 'dCnvDPOC2'
          flxnam(20) = 'dCnvDPON2'
          flxnam(21) = 'dCnvDPOP2'
          flxnam(22) = 'dCnvDPOS2'
          flxnam(23) = 'dMinPOC2G'
          flxnam(24) = 'dMinPON2'
          flxnam(25) = 'dMinPOP2'
          flxnam(26) = 'dMinPOS2'
          flxnam(27) = 'dCnvPPOC3'
          flxnam(28) = 'dCnvPPON3'
          flxnam(29) = 'dCnvPPOP3'
          flxnam(30) = 'dCnvPPOS3'
          flxnam(31) = 'dCnvDPOC3'
          flxnam(32) = 'dCnvDPON3'
          flxnam(33) = 'dCnvDPOP3'
          flxnam(34) = 'dCnvDPOS3'
          flxnam(35) = 'dMinPOC3G'
          flxnam(36) = 'dMinPON3'
          flxnam(37) = 'dMinPOP3'
          flxnam(38) = 'dMinPOS3'
          flxnam(39) = 'dCnvPPOC4'
          flxnam(40) = 'dCnvPPON4'
          flxnam(41) = 'dCnvPPOP4'
          flxnam(42) = 'dCnvPPOS4'
          flxnam(43) = 'dCnvDPOC4'
          flxnam(44) = 'dCnvDPON4'
          flxnam(45) = 'dCnvDPOP4'
          flxnam(46) = 'dCnvDPOS4'
          flxnam(47) = 'dMinPOC4G'
          flxnam(48) = 'dMinPON4'
          flxnam(49) = 'dMinPOP4'
          flxnam(50) = 'dMinPOS4'
          flxnam(51) = 'dCnvXDOC'
          flxnam(52) = 'dCnvXDON'
          flxnam(53) = 'dCnvXDOP'
          flxnam(54) = 'dCnvXDOS'
          flxnam(55) = 'dCnvYDOC'
          flxnam(56) = 'dCnvYDON'
          flxnam(57) = 'dCnvYDOP'
          flxnam(58) = 'dCnvYDOS'
          flxnam(59) = 'dMinDOCG'
          flxnam(60) = 'dMinDON'
          flxnam(61) = 'dMinDOP'
          flxnam(62) = 'dMinDOS'
          flxnam(63) = 'dPrecipPO4'
          flxnam(64) = 'dDissolPO4'
          flxnam(65) = 'dDissolSi'
          flxnam(66) = 'dOxCon'
          flxnam(67) = 'dNiDen'
          flxnam(68) = 'dFeRed'
          flxnam(69) = 'dSuRed'
          flxnam(70) = 'dMetGenCH4'
          flxnam(71) = 'dMetGenCO2'
          flxnam(72) = 'dMetEbul'
          flxnam(73) = 'dSUDox'
          flxnam(74) = 'dSUDprec'
          flxnam(75) = 'dSUDdis'
          flxnam(76) = 'dMethoxDO'
          flxnam(77) = 'dMethoxSu'
          flxnam(78) = 'dPrecAPPO4'
          flxnam(79) = 'dDissAPPO4'

          ! set up stochi (this is a copy of the old stochi.csv)
          stochi = 0.0
          stochi(is_PO4,if_dAdsPO4AAP) = -1
          stochi(is_AAP,if_dAdsPO4AAP) = 1
          stochi(is_NH4,if_dNITRIF) = -1
          stochi(is_NO3,if_dNITRIF) = 1
          stochi(is_OXY,if_dNITRIF) = -4.571
          stochi(is_POC1,if_dCnvPPOC1) = -1
          stochi(is_POC2,if_dCnvPPOC1) = 1
          stochi(is_PON1,if_dCnvPPON1) = -1
          stochi(is_PON2,if_dCnvPPON1) = 1
          stochi(is_POP1,if_dCnvPPOP1) = -1
          stochi(is_POP2,if_dCnvPPOP1) = 1
          stochi(is_POS1,if_dCnvPPOS1) = -1
          stochi(is_POS2,if_dCnvPPOS1) = 1
          stochi(is_POC1,if_dCnvDPOC1) = -1
          stochi(is_DOC,if_dCnvDPOC1) = 1
          stochi(is_PON1,if_dCnvDPON1) = -1
          stochi(is_DON,if_dCnvDPON1) = 1
          stochi(is_POP1,if_dCnvDPOP1) = -1
          stochi(is_DOP,if_dCnvDPOP1) = 1
          stochi(is_POS1,if_dCnvDPOS1) = -1
          stochi(is_DOS,if_dCnvDPOS1) = 1
          stochi(is_POC1,if_dMinPOC1G) = -1
          stochi(is_PON1,if_dMinPON1) = -1
          stochi(is_NH4,if_dMinPON1) = 1
          stochi(is_POP1,if_dMinPOP1) = -1
          stochi(is_PO4,if_dMinPOP1) = 1
          stochi(is_POS1,if_dMinPOS1) = -1
          stochi(is_SUD,if_dMinPOS1) = 1
          stochi(is_POC2,if_dCnvPPOC2) = -1
          stochi(is_POC3,if_dCnvPPOC2) = 1
          stochi(is_PON2,if_dCnvPPON2) = -1
          stochi(is_PON3,if_dCnvPPON2) = 1
          stochi(is_POP2,if_dCnvPPOP2) = -1
          stochi(is_POP3,if_dCnvPPOP2) = 1
          stochi(is_POS2,if_dCnvPPOS2) = -1
          stochi(is_POS3,if_dCnvPPOS2) = 1
          stochi(is_POC2,if_dCnvDPOC2) = -1
          stochi(is_DOC,if_dCnvDPOC2) = 1
          stochi(is_PON2,if_dCnvDPON2) = -1
          stochi(is_DON,if_dCnvDPON2) = 1
          stochi(is_POP2,if_dCnvDPOP2) = -1
          stochi(is_DOP,if_dCnvDPOP2) = 1
          stochi(is_POS2,if_dCnvDPOS2) = -1
          stochi(is_DOS,if_dCnvDPOS2) = 1
          stochi(is_POC2,if_dMinPOC2G) = -1
          stochi(is_PON2,if_dMinPON2) = -1
          stochi(is_NH4,if_dMinPON2) = 1
          stochi(is_POP2,if_dMinPOP2) = -1
          stochi(is_PO4,if_dMinPOP2) = 1
          stochi(is_POS2,if_dMinPOS2) = -1
          stochi(is_SUD,if_dMinPOS2) = 1
          stochi(is_POC3,if_dCnvPPOC3) = -1
          stochi(is_POC4,if_dCnvPPOC3) = 1
          stochi(is_PON3,if_dCnvPPON3) = -1
          stochi(is_PON4,if_dCnvPPON3) = 1
          stochi(is_POP3,if_dCnvPPOP3) = -1
          stochi(is_POP4,if_dCnvPPOP3) = 1
          stochi(is_POS3,if_dCnvPPOS3) = -1
          stochi(is_POS4,if_dCnvPPOS3) = 1
          stochi(is_POC3,if_dCnvDPOC3) = -1
          stochi(is_DOC,if_dCnvDPOC3) = 1
          stochi(is_PON3,if_dCnvDPON3) = -1
          stochi(is_DON,if_dCnvDPON3) = 1
          stochi(is_POP3,if_dCnvDPOP3) = -1
          stochi(is_DOP,if_dCnvDPOP3) = 1
          stochi(is_POS3,if_dCnvDPOS3) = -1
          stochi(is_DOS,if_dCnvDPOS3) = 1
          stochi(is_POC3,if_dMinPOC3G) = -1
          stochi(is_PON3,if_dMinPON3) = -1
          stochi(is_NH4,if_dMinPON3) = 1
          stochi(is_POP3,if_dMinPOP3) = -1
          stochi(is_PO4,if_dMinPOP3) = 1
          stochi(is_POS3,if_dMinPOS3) = -1
          stochi(is_SUD,if_dMinPOS3) = 1
          stochi(is_POC4,if_dMinPOC4G) = -1
          stochi(is_PON4,if_dMinPON4) = -1
          stochi(is_NH4,if_dMinPON4) = 1
          stochi(is_POP4,if_dMinPOP4) = -1
          stochi(is_PO4,if_dMinPOP4) = 1
          stochi(is_POS4,if_dMinPOS4) = -1
          stochi(is_SUD,if_dMinPOS4) = 1
          stochi(is_DOC,if_dMinDOCG) = -1
          stochi(is_DON,if_dMinDON) = -1
          stochi(is_NH4,if_dMinDON) = 1
          stochi(is_DOP,if_dMinDOP) = -1
          stochi(is_PO4,if_dMinDOP) = 1
          stochi(is_DOS,if_dMinDOS) = -1
          stochi(is_SUD,if_dMinDOS) = 1
          stochi(is_PO4,if_dPrecipPO4) = -1
          stochi(is_VIVP,if_dPrecipPO4) = 1
          stochi(is_PO4,if_dDissolPO4) = 1
          stochi(is_VIVP,if_dDissolPO4) = -1
          stochi(is_Si,if_dDissolSi) = 1
          stochi(is_Opal,if_dDissolSi) = -1
          stochi(is_OXY,if_dOxCon) = -2.667
          stochi(is_NO3,if_dNiDen) = -0.933
          stochi(is_FeIIIpa,if_dFeRed) = -18.6
          !tochi(is_FeIId,if_dFeRed) = 18.6
          stochi(is_SO4,if_dSuRed) = -1.333
          stochi(is_SUD,if_dSuRed) = 1.333
          stochi(is_CH4,if_dMetGenCH4) = 1
          stochi(is_CH4,if_dMetEbul) = -1
          stochi(is_SUD,if_dSUDox) = -1
          stochi(is_OXY,if_dSUDox) = -2
          stochi(is_SO4,if_dSUDox) = 1
          stochi(is_SUD,if_dSUDprec) = -1
          stochi(is_SUP,if_dSUDprec) = 1
          stochi(is_SUD,if_dSUDdis) = 1
          stochi(is_SUP,if_dSUDdis) = -1
          stochi(is_CH4,if_dMethoxDO) = -1
          stochi(is_OXY,if_dMethoxDO) = -5.33
          stochi(is_CH4,if_dMethoxSu) = -1
          stochi(is_SO4,if_dMethoxSu) = -2.67
          stochi(is_SUD,if_dMethoxSu) = 2.67
          stochi(is_PO4,if_dPrecAPPO4) = -1
          stochi(is_APATP,if_dPrecAPPO4) = 1
          stochi(is_PO4,if_dDissAPPO4) = 1
          stochi(is_APATP,if_dDissAPPO4) = -1

          ! set up balance admin
          noterm = 2 ! storage and water column terms
          notermtot = 0
          baname = ''
          do isys = 1,nototsed
              baname(isys,1) = subnam(isys)//'_storage'
              baname(isys,2) = subnam(isys)//'_fromwater'
              do ifl = 1,nofl
                  if (abs(stochi(isys,ifl)).gt.1e-10) then
                      noterm(isys) = noterm(isys) + 1
                      baname(isys,noterm(isys)) = subnam(isys)//'_'//flxnam(ifl)
                  endif
              enddo
              noterm(isys) = noterm(isys) + 1
              baname(isys,noterm(isys)) = subnam(isys)//'_error'
              notermtot = notermtot + noterm(isys)
          enddo

          ! Header
          open (newunit=lunsedbal,file='delwaqg-bal.his',access='stream')
          moname = ''
          write (lunsedbal) moname
          write (lunsedbal) notermtot,one
          do isys = 1,nototsed
            write (lunsedbal) (baname(isys,iterm),iterm=1,noterm(isys))
          enddo
          duname = 'total'
          write (lunsedbal) one,duname

          ! initialize storage term
          balterm = 0d0
          do iseg = 1,noseg
              CALL evaluate_waq_attribute(1,IKNMRK(iseg),iatt1) ! pick up first attribute
              CALL evaluate_waq_attribute(2,IKNMRK(iseg),iatt2) ! pick up second attribute
              if (iatt1.gt.0.and. (iatt2.eq.0.or.iatt2.eq.3) ) then
                  do isys = 1,nototsed
                      ip = last + isys
                      mass  = pmsa(ipoint(ip)+(iseg-1)*increm(ip))
                      balterm(isys,1) = balterm(isys,1) + dble(mass)
                  enddo
              endif
          enddo
          flxcum = 0d0
          watcum = 0d0

          balout = nint(pmsa(ipoint(ip_balout)))
          delt = pmsa(ipoint(ip_delt))

      endif

      itime = nint(pmsa(ipoint(ip_itime)))

      if (.not.first .and. mod(itime,balout*86400).eq.0 ) then

           write (lunsedbal) itime

          ! compose balance
          do isys = 1,nototsed
            balterm(isys,0) = 0d0
          enddo
          do iseg = 1,noseg
              CALL evaluate_waq_attribute(1,IKNMRK(iseg),iatt1) ! pick up first attribute
              CALL evaluate_waq_attribute(2,IKNMRK(iseg),iatt2) ! pick up second attribute
              if (iatt1.gt.0.and. (iatt2.eq.0.or.iatt2.eq.3) ) then
                  do isys = 1,nototsed

                      ! accumulation
                      ip = last + isys
                      mass  = pmsa(ipoint(ip)+(iseg-1)*increm(ip))
                      balterm(isys,0) = balterm(isys,0) + dble(mass)   ! current mass

                  enddo
              endif
          enddo

          do isys = 1,nototsed

            ! accumulation
            balterm(isys,1) = balterm(isys,1) - balterm(isys,0)

            ! water
            balterm(isys,2) = watcum(isys)

            iterm = 2
            error = balterm(isys,1)  + balterm(isys,2)
            do ifl = 1,nofl
                if (abs(stochi(isys,ifl)).gt.1e-10) then
                    iterm = iterm + 1
                    balterm(isys,iterm) = flxcum(ifl) * dble(stochi(isys,ifl))
                    error = error + balterm(isys,iterm)
                endif
            enddo

            ! write
            write (lunsedbal) (sngl(balterm(isys,iterm)),iterm=1,noterm(isys)-1),sngl(error)

          enddo

          do isys = 1,nototsed

            ! new mass to old mass for next interval
            balterm(isys,1) = balterm(isys,0)
            do iterm = 2,notermax
                balterm(isys,iterm) = 0d0
            enddo
          enddo

          flxcum = 0d0
          watcum = 0d0

      endif

      ! accumulate fluxes

      do iseg = 1,noseg
          CALL evaluate_waq_attribute(1,IKNMRK(iseg),iatt1) ! pick up first attribute
          CALL evaluate_waq_attribute(2,IKNMRK(iseg),iatt2) ! pick up second attribute
          if (iatt1.gt.0.and. (iatt2.eq.0.or.iatt2.eq.3) ) then
              do ifl=1,nofl
                  ip = last + 2*nototsed + ifl
                  flx = pmsa(ipoint(ip)+(iseg-1)*increm(ip))
                  flxcum (ifl) = flxcum(ifl) + dble(flx*delt)
              enddo
              do isys = 1,nototsed
                  ip = last + nototsed + isys
                  flx = pmsa(ipoint(ip)+(iseg-1)*increm(ip))
                  if (isys.le.nototseddis) then
                      watcum(isys) = watcum(isys) - dble(flx*delt)
                  else
                      watcum(isys) = watcum(isys) + dble(flx*delt)
                  endif
              enddo
          endif
      enddo
      first = .false.

      return
      end

end module m_sedbal
