// file      : examples/cxx/tree/performance/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <string>
#include <sstream>
#include <iostream>

#include <xercesc/util/PlatformUtils.hpp>

using namespace std;

// See parsing.cxx
//
bool
parsing (const char* file, unsigned long iter, bool validate);

// See serialization.cxx
//
bool
serialization (const char* file, unsigned long iter);

int
main (int argc, char* argv[])
{
  if (argc < 2)
  {
    cerr << "usage: " << argv[0] << " [-v] [-i <count>] test.xml" << endl
         << "\t -v turn on validation (default is off)" << endl
         << "\t -i number of iterations to perform (default is 1000)" << endl;
    return 1;
  }

  bool validate (false);
  unsigned long iter (1000);
  const char* file (0);

  // Parse command line arguments.
  //
  for (int i (1); i < argc; ++i)
  {
    std::string arg (argv[i]);

    if (arg == "-v")
    {
      validate = true;
    }
    else if (arg == "-i")
    {
      if (++i == argc)
      {
        cerr << "argument expected for the -i option" << endl;
        return 1;
      }

      iter = 0;
      istringstream is (argv[i]);
      is >> iter;

      if (iter == 0)
      {
        cerr << "invalid argument for the -i option" << endl;
        return 1;
      }
    }
    else
    {
      file = argv[i];
      break;
    }
  }

  if (file == 0)
  {
    cerr << "no input file specified" << endl;
    return 1;
  }

  int r (0);

  xercesc::XMLPlatformUtils::Initialize ();

  // Call parsing and serialization tests.
  //
  if (!parsing (file, iter, validate) || !serialization (file, iter))
    r = 1;

  xercesc::XMLPlatformUtils::Terminate ();

  return r;
}
