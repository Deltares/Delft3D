#!/bin/bash
ifx -c -fPIC stdlib_kinds.f90 precision_basics.f90 precision.f90 waq_precision.f90 logger_helper.f90 m_extract_waq_attribute.f90
ifx -shared -fPIC  decayt.f90 waq_precision.o logger_helper.o m_extract_waq_attribute.o -o decayt.so

