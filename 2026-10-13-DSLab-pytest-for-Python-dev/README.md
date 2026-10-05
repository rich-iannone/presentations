# tinytab

A tiny library for formatting values and rendering small tables. It exists to
demonstrate testing a Python library with pytest.

```python
from tinytab import Table, fmt_currency

tbl = Table({"item": ["coffee", "bagel"], "price": [3.5, 2.25]})
print(tbl.fmt("price", fmt_currency).as_text())
```
