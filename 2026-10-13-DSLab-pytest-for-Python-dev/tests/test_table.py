import pytest

from tinytab import Table, fmt_currency


# Fixtures: `menu` comes from conftest.py, pytest injects it by name
def test_table_shape(menu):
    assert menu.columns == ["item", "price", "sold"]
    assert menu.n_rows == 3


def test_table_repr(menu):
    assert repr(menu) == "<Table: 3 rows x 3 columns>"


def test_table_unequal_columns():
    with pytest.raises(ValueError, match="same length"):
        Table({"a": [1, 2], "b": [1]})


def test_fmt_unknown_column(menu):
    with pytest.raises(KeyError, match="no_such_col"):
        menu.fmt("no_such_col", str)


def test_fmt_returns_new_table(menu):
    formatted = menu.fmt("price", fmt_currency)

    assert formatted is not menu
    assert "$3.50" in formatted.as_text()
    assert "$3.50" not in menu.as_text()


# A fixture defined locally, built on top of another fixture
@pytest.fixture
def formatted_menu(menu):
    return menu.fmt("price", fmt_currency)


def test_missing_values_render_blank(formatted_menu):
    last_line = formatted_menu.as_text().splitlines()[-1]
    assert last_line.startswith("| croissant")
    assert last_line.rstrip(" |").endswith("$4.00")


# Built-in fixtures: `tmp_path` gives each test a fresh temporary directory
def test_csv_round_trip(tmp_path, menu):
    path = tmp_path / "menu.csv"
    menu.to_csv(path)

    assert path.exists()

    result = Table.from_csv(path)
    assert result.columns == menu.columns
    assert result.n_rows == menu.n_rows


def test_from_csv_empty_file(tmp_path):
    path = tmp_path / "empty.csv"
    path.write_text("a,b\n")

    with pytest.raises(ValueError, match="No data rows"):
        Table.from_csv(path)
