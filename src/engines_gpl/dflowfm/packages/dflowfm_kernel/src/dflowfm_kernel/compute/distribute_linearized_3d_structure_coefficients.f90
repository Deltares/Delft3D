!> module containing subroutine distribute_linearized_3d_structure_coefficients for distributing linearized 3D structure coefficients 
!! over the layers of the structure.
module m_distribute_linearized_3d_structure_coefficients
   contains
   !> distribute linearized 3D structure coefficients over the layers of the structure.
   subroutine distribute_linearized_3d_structure_coefficients(structure)
      use precision_basics, only: dp
      use m_1d_structures, only: t_structure
      use m_General_Structure, only: t_GeneralStructure
      use m_flowparameters, only: jastructurelayersactive
      use m_flowgeom, only: ln
      use m_flow, only: u1, s1, zws, au, fu, ru, hu
      use fm_external_forcings_data, only: ff3
      use m_get_Lbot_Ltop, only: getLbotLtop

      implicit none

      type(t_structure), pointer, intent(in) :: structure               !< Derived type containing structure information.
      
      type(t_GeneralStructure), pointer :: genstr               !< Derived type containing general structure information.
      integer :: Lf, Lb, Lt
      integer :: L0, iup, LL, kk, k1, k2
      real(kind=dp) :: hhi(3), zti(3), zbi(3)
      real(kind=dp) :: wstr, gatefraction
      real(kind=dp) :: au1, au2, au3

      genstr => structure%generalst
      do L0 = 1, structure%numlinks
         Lf = structure%linknumbers(L0)
         k1 = ln(1, Lf); k2 = ln(2, Lf) ! 1 -> 2 flow link direction

         call getLbotLtop(Lf, Lb, Lt)
         gatefraction = genstr%gateclosedfractiononlink(L0)
         
         if (jastructurelayersactive == 0) then
            do LL = Lb, Lt
               fu(LL) = fu(Lf); ru(LL) = ru(Lf)
               au(LL) = au(Lf) * (hu(LL) - hu(LL - 1)) / (hu(Lt) - hu(Lb - 1))
            end do
         else
            wstr = genstr%widthcenteronlink(L0)
            if (genstr%au(1, L0) > 0) then
               hhi(1) = genstr%au(1, L0) / (gatefraction * wstr)
               zti(1) = zbi(1) + hhi(1)
            end if
            if (genstr%au(2, L0) > 0) then
               hhi(2) = genstr%au(2, L0) / (gatefraction * wstr)
               zti(2) = zbi(2) + hhi(2)
            end if
            if (genstr%au(3, L0) > 0) then
               hhi(3) = genstr%au(3, L0) / ((1.0_dp - gatefraction) * wstr)
               zti(3) = zbi(3) + hhi(3)
            end if

            if (u1(Lf) > 0) then
               iup = 1
            else if (u1(Lf) < 0) then
               iup = 2
            else if (s1(k1) > s1(k2)) then
               iup = 1
            else
               iup = 2
            end if
            ff3(:, 0) = 0d0
            do LL = Lb, Lt
               kk = ln(iup, LL)
               if (genstr%au(1, L0) > 0) ff3(1, LL - Lb + 1) = max(0.0_dp, min(zti(1), zws(kk)) - zbi(1)) / hhi(1)
               if (genstr%au(2, L0) > 0) ff3(2, LL - Lb + 1) = max(0.0_dp, min(zti(2), zws(kk)) - zbi(2)) / hhi(2)
               if (genstr%au(3, L0) > 0) ff3(3, LL - Lb + 1) = max(0.0_dp, min(zti(3), zws(kk)) - zbi(3)) / hhi(3)
            end do

            do LL = Lb, Lt
               au1 = genstr%au(1, L0) * (ff3(1, LL - Lb + 1) - ff3(1, LL - Lb))
               au2 = genstr%au(2, L0) * (ff3(2, LL - Lb + 1) - ff3(2, LL - Lb))
               au3 = genstr%au(3, L0) * (ff3(3, LL - Lb + 1) - ff3(3, LL - Lb))
               au(LL) = au1 + au2 + au3
               if (au(LL) > 0) then
                  fu(LL) = (genstr%fu(1, L0) * au1 + genstr%fu(2, L0) * au2 + genstr%fu(3, L0) * au3) / au(LL)
                  ru(LL) = (genstr%ru(1, L0) * au1 + genstr%ru(2, L0) * au2 + genstr%ru(3, L0) * au3) / au(LL)
               else
                  fu(LL) = 0.0_dp
                  ru(LL) = 0.0_dp
               end if
            end do
        end if
      end do 
   end subroutine distribute_linearized_3d_structure_coefficients

end module m_distribute_linearized_3d_structure_coefficients

