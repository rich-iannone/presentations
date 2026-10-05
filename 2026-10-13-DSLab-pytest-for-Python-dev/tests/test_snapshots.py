import pytest
from syrupy.extensions.single_file import SingleFileSnapshotExtension, WriteMode

from tinytab import Table, fmt_currency, fmt_number


# Snapshot tests: the first run (with `--snapshot-update`) records the output to
# tests/__snapshots__/; later runs compare against what was recorded
def test_as_text_snapshot(snapshot, menu):
    assert menu.as_text() == snapshot


def test_as_text_formatted_snapshot(snapshot, menu):
    tbl = menu.fmt("price", fmt_currency).fmt("sold", lambda x: fmt_number(x, decimals=0))
    assert tbl.as_text() == snapshot


# Escaping matters for HTML, and a snapshot makes it easy to review
def test_as_html_escapes(snapshot):
    tbl = Table({"name": ["<b>bold</b>", "Tom & Jerry"]})
    assert tbl.as_html() == snapshot


# Storing a snapshot as its own .html file means it can be opened in a browser
class HTMLSnapshotExtension(SingleFileSnapshotExtension):
    file_extension = "html"
    _write_mode = WriteMode.TEXT


@pytest.fixture
def snapshot_html(snapshot):
    return snapshot.use_extension(HTMLSnapshotExtension)


def test_as_html_snapshot(snapshot_html, menu):
    assert menu.fmt("price", fmt_currency).as_html() == snapshot_html
