-include .env
-include .env.local
export

.PHONY: build
build:
	@stack build

.PHONY: test
test:
	@stack test

.PHONY: watch-test
watch-test:
	@stack test --file-watch

.PHONY: run
run:
	@stack run ${ARGS}

.PHONY: run-query
run-query:
	@op run --no-masking --cache --env-file=.env.local -- stack run imapquery ${ARGS}

.PHONY: run-filter
run-filter:
	@op run --no-masking --cache --env-file=.env.local -- stack run imapfilter ${ARGS}

.PHONY: docs
docs:
	@stack haddock --no-haddock-deps --haddock-arguments '-o docs'

.PHONY: redocs
redocs:
	@stack haddock --reconfigure
	@${MAKE} docs

browse-docs: docs
	@open -b com.apple.Safari docs/index.html

.PHONY: coverage
coverage:
	@stack test --coverage
	@stack hpc report --all --destdir coverage

.PHONY: watch-coverage
watch-coverage:
	@stack test --coverage --file-watch

browse-coverage: coverage
	@open -b com.apple.Safari coverage/hpc_index.html

benchmark:
	@stack bench --ba '--output benchmark.html'

browse-benchmark:
	@open -b com.apple.Safari benchmark.html 2> /dev/null || true
