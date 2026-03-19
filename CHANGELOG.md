# Changelog

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/) and
[Keep a CHANGELOG](http://keepachangelog.com).


## v0.3.0 - 2026-03-19

Elixir versions < 1.15 and OTP versions < 25 are no longer supported.

### Changed

- Replace `RDF.Data` protocol implementation with new `RDF.Data.Source` protocol
  of RDF.ex v3.0
- Compound now behaves as a named graph with `graph_name` = compound id; this
  affects `RDF.Data.merge/2` which now returns a `Dataset` when merging compounds
  or graphs with different names

### Removed

- `RTC.Compound.predicates/1`, `RTC.Compound.objects/1`, and `RTC.Compound.resources/1` -
  use the corresponding `RDF.Data` functions instead; if only asserted or unasserted
  triples are relevant, apply the functions on `asserted_graph/1` or `unasserted_graph/1`


[Compare v0.2.0...v0.3.0](https://github.com/rtc-org/rtc-ex/compare/v0.2.0...v0.3.0)



## v0.2.0 - 2024-08-07

This version upgrades to RDF.ex v2.0.

### Added 

- `RTC.Compound.change_graph_name/1` which changes the graph name that will be 
  used in serializations to RDF (as opposed to `change_name/1` which, as an
  alias of `reset_id/1`, also changes the id of the compound itself) 


[Compare v0.1.0...v0.2.0](https://github.com/rtc-org/rtc-ex/compare/v0.1.0...v0.2.0)



## v0.1.0 - 2023-03-31

Initial release
