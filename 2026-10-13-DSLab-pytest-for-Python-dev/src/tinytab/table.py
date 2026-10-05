"""A tiny table that can be formatted and rendered as text or HTML."""

from __future__ import annotations

import csv
from html import escape
from pathlib import Path
from typing import Callable


class Table:
    """A small, column-oriented table.

    Parameters
    ----------
    data
        A dictionary mapping column names to lists of values. All columns must
        have the same length.
    """

    def __init__(self, data: dict[str, list]):
        lengths = {len(v) for v in data.values()}
        if len(lengths) > 1:
            raise ValueError("All columns must have the same length")

        self._data = {k: list(v) for k, v in data.items()}
        self._formatters: dict[str, Callable[[object], str]] = {}

    @property
    def columns(self) -> list[str]:
        return list(self._data)

    @property
    def n_rows(self) -> int:
        return len(next(iter(self._data.values()), []))

    def __repr__(self) -> str:
        return f"<Table: {self.n_rows} rows x {len(self.columns)} columns>"

    def fmt(self, column: str, fn: Callable[[object], str]) -> Table:
        """Attach a formatting function to a column. Returns a new Table."""
        if column not in self._data:
            raise KeyError(f"No column named {column!r}")

        new = Table(self._data)
        new._formatters = {**self._formatters, column: fn}
        return new

    def _cell(self, column: str, i: int) -> str:
        value = self._data[column][i]
        if value is None:
            return ""
        fn = self._formatters.get(column, str)
        return fn(value)

    def _rows(self) -> list[list[str]]:
        return [[self._cell(col, i) for col in self.columns] for i in range(self.n_rows)]

    def as_text(self) -> str:
        """Render the table as a Markdown-style text table."""
        rows = self._rows()
        widths = [
            max(len(col), *(len(r[j]) for r in rows)) if rows else len(col)
            for j, col in enumerate(self.columns)
        ]

        def line(cells: list[str]) -> str:
            return "| " + " | ".join(c.ljust(w) for c, w in zip(cells, widths)) + " |"

        out = [line(self.columns), "|" + "|".join("-" * (w + 2) for w in widths) + "|"]
        out += [line(r) for r in rows]
        return "\n".join(out)

    def as_html(self) -> str:
        """Render the table as an HTML <table>."""
        head = "".join(f"<th>{escape(c)}</th>" for c in self.columns)
        body = "\n".join(
            "    <tr>" + "".join(f"<td>{escape(c)}</td>" for c in row) + "</tr>"
            for row in self._rows()
        )
        return (
            '<table class="tinytab">\n'
            f"  <thead>\n    <tr>{head}</tr>\n  </thead>\n"
            f"  <tbody>\n{body}\n  </tbody>\n"
            "</table>"
        )

    def to_csv(self, path: str | Path) -> None:
        """Write the (unformatted) table to a CSV file."""
        with open(path, "w", newline="") as f:
            writer = csv.writer(f)
            writer.writerow(self.columns)
            for i in range(self.n_rows):
                writer.writerow(self._data[col][i] for col in self.columns)

    @classmethod
    def from_csv(cls, path: str | Path) -> Table:
        """Read a table from a CSV file. All values are read as strings."""
        with open(path, newline="") as f:
            rows = list(csv.DictReader(f))
        if not rows:
            raise ValueError(f"No data rows found in {path}")
        return cls({col: [r[col] for r in rows] for col in rows[0]})
