set (mkl_path $ENV{ONEAPI_ROOT}/mkl/latest/lib/intel64/)
install (DIRECTORY ${mkl_path} DESTINATION lib
  FILES_MATCHING
  PATTERN "libmkl_core.so*"
  PATTERN "libmkl_avx*.so*"
  PATTERN "libmkl_def*.so*"
  PATTERN "libmkl_intel_thread.so*"
  PATTERN "libmkl_sequential.so*"
  PATTERN "intel64" EXCLUDE
)
