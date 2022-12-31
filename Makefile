build:
	stack build

run-sample:
	stack run ./sample.yaml

test-watch:
	ghcid --command 'stack ghci piggies-finance-tracker --test --main-is piggies-finance-tracker:test:piggies-finance-tracker-test' --test 'main' --warnings
