# Changelog

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/) and
[Keep a CHANGELOG](http://keepachangelog.com).


## Unreleased

This version upgrades to RDF.ex v2.0.

### Added 

- `RTC.Compound.change_graph_name/1` which changes the graph name that will be 
  used in serializations to RDF (as opposed to `change_name/1` which, as an
  alias of `reset_id/1`, also changes the id of the compound itself) 

[Compare v0.1.0...HEAD](https://github.com/rtc-org/rtc-ex/compare/v0.1.0...HEAD)


## v0.1.0 - 2023-03-31

Initial release
