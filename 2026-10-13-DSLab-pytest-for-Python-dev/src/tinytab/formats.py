"""Functions for formatting single values as strings."""

from __future__ import annotations

CURRENCY_SYMBOLS = {
    "USD": "$",
    "EUR": "€",
    "GBP": "£",
    "JPY": "¥",
}


def _check_number(x: object) -> None:
    # `bool` is a subclass of `int`, but formatting `True` as "1.00" is almost always a mistake
    if isinstance(x, bool) or not isinstance(x, (int, float)):
        raise TypeError(f"Expected a number, got {type(x).__name__}: {x!r}")


def fmt_number(x: float, decimals: int = 2, sep_mark: str = ",") -> str:
    """Format a number with a fixed number of decimal places and a thousands separator.

    >>> fmt_number(1234.567)
    '1,234.57'
    """
    _check_number(x)
    if decimals < 0:
        raise ValueError("`decimals` must be zero or greater")

    out = f"{x:,.{decimals}f}"
    if sep_mark != ",":
        out = out.replace(",", sep_mark)
    return out


def fmt_percent(x: float, decimals: int = 1) -> str:
    """Format a proportion as a percentage.

    >>> fmt_percent(0.256)
    '25.6%'
    """
    _check_number(x)
    return fmt_number(x * 100, decimals=decimals) + "%"


def fmt_currency(x: float, currency: str = "USD") -> str:
    """Format a number as a currency value.

    >>> fmt_currency(1234.5)
    '$1,234.50'
    """
    _check_number(x)
    currency = currency.upper()
    if currency not in CURRENCY_SYMBOLS:
        raise ValueError(f"Unknown currency code: {currency!r}")

    decimals = 0 if currency == "JPY" else 2
    symbol = CURRENCY_SYMBOLS[currency]
    sign = "-" if x < 0 else ""
    return f"{sign}{symbol}{fmt_number(abs(x), decimals=decimals)}"
