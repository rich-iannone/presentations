import pytest

from tinytab import Table


@pytest.fixture
def menu():
    """A small table of cafe menu items, shared across test files."""
    return Table(
        {
            "item": ["coffee", "bagel", "croissant"],
            "price": [3.5, 2.25, 4.0],
            "sold": [120, 45, None],
        }
    )
