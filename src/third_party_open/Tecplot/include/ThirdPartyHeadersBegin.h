// Avoid warnings in headers that are out of our control
#if defined(_MSC_VER)
  #pragma warning (push, 0)
  // Explicitly disable warnings that we've turned on in HelperMacros.cmake
  #pragma warning (disable:4191)
  #pragma warning (disable:4242)
  #pragma warning (disable:4263)
  #pragma warning (disable:4264)
  #pragma warning (disable:4265)
  #pragma warning (disable:4266)
  #pragma warning (disable:4302)
  #pragma warning (disable:4905)
  #pragma warning (disable:4906)
  #pragma warning (disable:4928)
#if (_MSC_VER > 1600)
// Disable code analysis warnings for third party headers.
  #include <codeanalysis\warnings.h>
  #pragma warning (push)
  #pragma warning (disable : ALL_CODE_ANALYSIS_WARNINGS)
#endif

#endif

