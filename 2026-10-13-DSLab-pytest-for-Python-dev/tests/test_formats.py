import pytest

from tinytab import fmt_currency, fmt_number, fmt_percent


# 1. The simplest possible test: call a function, assert on the result
def test_fmt_number_default():
    assert fmt_number(1234.567) == "1,234.57"


def test_fmt_number_decimals():
    assert fmt_number(1234.567, decimals=0) == "1,235"
    assert fmt_number(1234.567, decimals=1) == "1,234.6"


# 2. Parametrize: one test function, many cases
@pytest.mark.parametrize(
    "x, expected",
    [
        (0, "0.00"),
        (1, "1.00"),
        (-1, "-1.00"),
        (1_000_000, "1,000,000.00"),
        (0.005, "0.01"),
    ],
)
def test_fmt_number_many(x, expected):
    assert fmt_number(x) == expected


@pytest.mark.parametrize(
    "x, currency, expected",
    [
        (1234.5, "USD", "$1,234.50"),
        (1234.5, "EUR", "€1,234.50"),
        (1234.5, "eur", "€1,234.50"),
        (1234.5, "JPY", "¥1,234"),
        (-5, "GBP", "-£5.00"),
    ],
    ids=["usd", "eur", "lowercase-code", "jpy-no-decimals", "negative"],
)
def test_fmt_currency(x, currency, expected):
    assert fmt_currency(x, currency=currency) == expected


def test_fmt_percent():
    assert fmt_percent(0.256) == "25.6%"
    assert fmt_percent(1) == "100.0%"


# 3. Testing that errors are raised (and with the right message)
def test_fmt_currency_unknown_code():
    with pytest.raises(ValueError, match="Unknown currency code"):
        fmt_currency(10, currency="XYZ")


@pytest.mark.parametrize("bad", ["10", None, True, [1, 2]])
def test_formatters_reject_non_numbers(bad):
    with pytest.raises(TypeError, match="Expected a number"):
        fmt_number(bad)


def test_fmt_number_negative_decimals():
    with pytest.raises(ValueError):
        fmt_number(1.0, decimals=-1)


# 4. Documenting a known gap with xfail (`xfail_strict = true` means it fails once fixed)
@pytest.mark.xfail(reason="fmt_number has no `dec_mark=` argument yet")
def test_fmt_number_european_style():
    assert fmt_number(1234.5, sep_mark=".", dec_mark=",") == "1.234,50"
