module test_fm_external_forcings_utils
   use assertions_gtest
   use fm_external_forcings_utils, only: get_constituent_name
   use m_util_helpers, only: cstr
   implicit none(type, external)
contains

    !$f90tw TESTCODE(TEST, test_fm_external_forcings_utils,
    !$f90tw test_get_constituent_name__ignore_discharge, test_get_constituent_name__ignore_discharge,
    subroutine test_get_constituent_name__ignore_discharge() bind(C)
        use fm_external_forcings_utils, only: get_constituent_name

        integer, parameter :: STRBUFLEN = 256
        character(len=STRBUFLEN) :: constituent_name, base_quantity
        
        base_quantity = "don't touch me"

        ! Discharge is not a constituent so this call should leave `contituent_name` unchanged and `base_quantity` empty.
        call get_constituent_name("sourcesink_discharge", constituent_name, base_quantity)

        call F90_ASSERT_STREQ(cstr(constituent_name), cstr(""), cstr("Unexpected constituent_name"))
        call F90_ASSERT_STREQ(cstr(base_quantity), cstr("don't touch me"), cstr("Unexpected base_quantity"))
    end subroutine test_get_constituent_name__ignore_discharge
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_fm_external_forcings_utils,
    !$f90tw test_get_constituent_name__with_delta, test_get_constituent_name__with_delta,
    subroutine test_get_constituent_name__with_delta() bind(C)
        use fm_external_forcings_utils, only: get_constituent_name

        integer, parameter :: STRBUFLEN = 256
        character(len=STRBUFLEN) :: constituent_name, base_quantity
        
        call get_constituent_name("sourcesink_salinityDelta", constituent_name, base_quantity)

        call F90_ASSERT_STREQ(cstr(constituent_name), cstr("salinity"), cstr("Unexpected constituent_name"))
        call F90_ASSERT_STREQ(cstr(base_quantity), cstr("sourcesink_constituentDelta"), cstr("Unexpected base_quantity"))
    end subroutine test_get_constituent_name__with_delta
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_fm_external_forcings_utils,
    !$f90tw test_get_constituent_name__with_delta__strip_tracer, test_get_constituent_name__with_delta__strip_tracer,
    subroutine test_get_constituent_name__with_delta__strip_tracer() bind(C)
        use fm_external_forcings_utils, only: get_constituent_name

        integer, parameter :: STRBUFLEN = 256
        character(len=STRBUFLEN) :: constituent_name, base_quantity
        
        call get_constituent_name("sourcesink_tracerFooDelta", constituent_name, base_quantity)

        call F90_ASSERT_STREQ(cstr(constituent_name), cstr("Foo"), cstr("Unexpected constituent_name"))
        call F90_ASSERT_STREQ(cstr(base_quantity), cstr("sourcesink_constituentDelta"), cstr("Unexpected base_quantity"))
    end subroutine test_get_constituent_name__with_delta__strip_tracer
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_fm_external_forcings_utils,
    !$f90tw test_get_constituent_name__with_delta__strip_sedfrac, test_get_constituent_name__with_delta__strip_sedfrac,
    subroutine test_get_constituent_name__with_delta__strip_sedfrac() bind(C)
        use fm_external_forcings_utils, only: get_constituent_name

        integer, parameter :: STRBUFLEN = 256
        character(len=STRBUFLEN) :: constituent_name, base_quantity
        
        call get_constituent_name("sourcesink_sedFracBarDelta", constituent_name, base_quantity)

        call F90_ASSERT_STREQ(cstr(constituent_name), cstr("Bar"), cstr("Unexpected constituent_name"))
        call F90_ASSERT_STREQ(cstr(base_quantity), cstr("sourcesink_constituentDelta"), cstr("Unexpected base_quantity"))
    end subroutine test_get_constituent_name__with_delta__strip_sedfrac
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_fm_external_forcings_utils,
    !$f90tw test_get_constituent_name__without_delta, test_get_constituent_name__without_delta,
    subroutine test_get_constituent_name__without_delta() bind(C)
        use fm_external_forcings_utils, only: get_constituent_name

        integer, parameter :: STRBUFLEN = 256
        character(len=STRBUFLEN) :: constituent_name, base_quantity
        
        call get_constituent_name("sourcesink_sedFracBar", constituent_name, base_quantity)

        call F90_ASSERT_STREQ(cstr(constituent_name), cstr("Bar"), cstr("Unexpected constituent_name"))
        call F90_ASSERT_STREQ(cstr(base_quantity), cstr("sourcesink_constituent"), cstr("Unexpected base_quantity"))
    end subroutine test_get_constituent_name__without_delta
    !$f90tw)

end module test_fm_external_forcings_utils
