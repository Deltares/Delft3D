import unittest
import sys

if __name__ == "__main__":
    loader = unittest.TestLoader()
    suite = loader.discover("wrappers/python/test")
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    sys.exit(0 if result.wasSuccessful() else 1)
