.PHONY: help install test test-all test-slow test-update test-cov clean

help: ## show this help
	@grep -E '^[a-zA-Z_-]+:.*?## ' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-12s %s\n", $$1, $$2}'

install: ## create .venv and install tinytab + dev dependencies
	uv sync

test: ## run the test suite (skips slow tests)
	uv run pytest

test-all: ## run all tests, including slow ones
	uv run pytest -m ""

test-slow: ## run only the slow tests
	uv run pytest -m slow

test-update: ## accept snapshot changes
	uv run pytest --snapshot-update

test-cov: ## run tests with a coverage report (HTML in htmlcov/)
	uv run pytest --cov --cov-report=term-missing --cov-report=html

clean: ## remove test, coverage, and Python artifacts
	rm -rf .pytest_cache .coverage htmlcov
	find . -name '__pycache__' -not -path './.venv/*' -exec rm -rf {} +
