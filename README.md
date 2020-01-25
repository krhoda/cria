# Cria -- A Little Alpaca API Client:
### WORK IN PROGRESS

Cria, beyond being a [baby Alpaca](https://en.wikipedia.org/wiki/Cria), is a set of helper libraries around the [Alpaca API v2](https://docs.alpaca.markets/api-documentation/api-v2/) using [Servant](https://github.com/haskell-servant/servant/) to do most of the heavy lifting. Includes thorough integration tests via the `paper-market` instance that employs real data but uses fake money. Not all routes have been implemented, but as soon as one is, it is added to the integration tests -- the documentation linked isn't perfect so I try to make sure I get every combination of route to at least get the types right, maybe this could become property-based? Currently the tests are an all or nothing run, ideally, would move them to a test runner or something.

To use the tests via `app/Main.hs` yourself, there is an expectation that you set: `ALPACA_KEY` and `ALPACA_SECRET`, or grab them some other way and simply use `runAPITest` from the `IntegrationTest` package.

More details forthcoming...

Next Steps:
* Test newly added Bar routes.
* Add missing Routes to Todo (meta-todo).
* Complete missing routes + tests.
* Finalize public REST API.
* Report differences in Alpaca Docs and my Tests to Alpaca.
* Create Documentation.
* Create test runner.
* Add streaming capability for Alpaca Account.
* Add streaming capability for Market Data streaming from Alpaca's 3rd Party Providers.
