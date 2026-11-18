"""Shared infrastructure for the dflowfm_io code generators.

Both generators read a single source of truth (the C header or ``mdu.json``) and emit committed
Python modules. This module holds what they have in common: a small builder that accumulates
generated source, a value object describing a module to write, and a base class that writes the
modules and reports what it wrote.
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass
from pathlib import Path


class SourceBuilder:
    """Accumulates lines of generated Python source and renders them to a single string."""

    def __init__(self) -> None:
        self._lines: list[str] = []

    def line(self, text: str = "") -> "SourceBuilder":
        """Append a single line."""
        self._lines.append(text)
        return self

    def extend(self, texts: list[str]) -> "SourceBuilder":
        """Append several lines."""
        self._lines.extend(texts)
        return self

    def blank(self, count: int = 1) -> "SourceBuilder":
        """Append ``count`` blank lines."""
        self._lines.extend([""] * count)
        return self

    def render(self) -> str:
        """Return the accumulated lines as text with a trailing newline."""
        return "\n".join(self._lines) + "\n"


@dataclass(frozen=True)
class GeneratedModule:
    """A module the generator will write: its path, its source, and a one-line console summary."""

    path: Path
    source: str
    summary: str


class Generator(ABC):
    """Base for a generator: subclasses parse the source of truth and build the modules; the base
    writes each one and prints what it wrote."""

    def run(self) -> None:
        """Build the modules, write them to disk, and report each on stdout."""
        for module in self.build():
            module.path.write_text(module.source, encoding="utf-8")
            print(f"Wrote {module.summary} to {module.path}")

    @abstractmethod
    def build(self) -> list[GeneratedModule]:
        """Parse the source of truth and return the modules to write."""
