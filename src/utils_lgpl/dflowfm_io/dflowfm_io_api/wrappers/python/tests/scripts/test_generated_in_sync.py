import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "scripts"))

from generate_model import ModelGenerator
from generate_schema import SchemaGenerator


class TestGeneratedFilesInSync(unittest.TestCase):
    """The committed generated files must equal a fresh regeneration — guards against generator drift."""

    def _assert_in_sync(self, generator):
        for module in generator.build():
            self.assertEqual(
                module.path.read_text(encoding="utf-8"),
                module.source,
                f"{module.path.name} is stale vs its generator; run scripts/ to regenerate and commit it",
            )

    def test_model_in_sync(self):
        self._assert_in_sync(ModelGenerator())

    def test_schema_in_sync(self):
        self._assert_in_sync(SchemaGenerator())


if __name__ == "__main__":
    unittest.main()
