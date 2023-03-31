# RTC.ex

[![Hex.pm](https://img.shields.io/hexpm/v/rtc.svg?style=flat-square)](https://hex.pm/packages/rtc)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/rtc/)
[![License](https://img.shields.io/hexpm/l/rtc.svg)](https://github.com/rtc-org/rtc-ex/blob/master/LICENSE.md)

[![ExUnit Tests](https://github.com/rtc-org/rtc-ex/actions/workflows/elixir-build-and-test.yml/badge.svg)](https://github.com/rtc-org/rtc-ex/actions/workflows/elixir-build-and-test.yml)
[![Dialyzer](https://github.com/rtc-org/rtc-ex/actions/workflows/elixir-dialyzer.yml/badge.svg)](https://github.com/rtc-org/rtc-ex/actions/workflows/elixir-dialyzer.yml)
[![Quality Checks](https://github.com/rtc-org/rtc-ex/actions/workflows/elixir-quality-checks.yml/badge.svg)](https://github.com/rtc-org/rtc-ex/actions/workflows/elixir-quality-checks.yml)

An implementation of [RDF Triple Compounds](https://rtc-org.github.io/spec) in Elixir.

RDF Triple Compounds (RTC) is a spec which defines a vocabulary around the concept
of _triple compounds_, nestable sub-graphs embedded in RDF-star graphs.

For a guide and more information on its underlying projects, go to <https://rdf-elixir.dev/rtc-ex/>.


## Usage

Create a compound:

```elixir
iex> [
...>   EX.Employee38 
...>   |> EX.firstName("John")
...>   |> EX.familyName("Smith")
...>   |> EX.jobTitle("Assistant Designer"),
...>   EX.Employee39
...>   |> EX.firstName("Jane")
...>   |> EX.familyName("Doe")
...>   |> EX.jobTitle("HR Manager")
...> ]
...> |> RTC.Compound.new(EX.Compound,
...>   prefixes: [ex: EX],    
...>   annotations: %{EX.dataSource() => EX.DataSource}
...> )
#RTC.Compound<id: ~I<http://example.com/Compound>
  @prefix ex: <http://example.com/> .
  @prefix rtc: <https://w3id.org/rtc#> .

  ex:Compound
      ex:dataSource ex:DataSource ;
      rtc:elements 
        << ex:Employee38 ex:familyName "Smith" >>, 
        << ex:Employee38 ex:firstName "John" >>, 
        << ex:Employee38 ex:jobTitle "Assistant Designer" >>, 
        << ex:Employee39 ex:familyName "Doe" >>, 
        << ex:Employee39 ex:firstName "Jane" >>, 
        << ex:Employee39 ex:jobTitle "HR Manager" >> .

  ex:Employee38
      ex:familyName "Smith" ;
      ex:firstName "John" ;
      ex:jobTitle "Assistant Designer" .

  ex:Employee39
      ex:familyName "Doe" ;
      ex:firstName "Jane" ;
      ex:jobTitle "HR Manager" .
>
```

Add it to a triple store:

```elixir
compound
|> RTC.Compound.to_rdf()
|> SPARQL.Client.insert_data("http://example.com/sparql")
```

And fetch the compound back from a triple store:

```elixir
RTC.Compound.from_sparql("http://example.com/sparql", EX.Compound)
```


## Contributing

See [CONTRIBUTING](CONTRIBUTING.md) for details.


## Consulting

If you need help with your Elixir and Linked Data projects, just contact [NinjaConcept](https://www.ninjaconcept.com/) via <contact@ninjaconcept.com>.


## License and Copyright

(c) 2023-present Marcel Otto. MIT Licensed, see [LICENSE](LICENSE.md) for details.
