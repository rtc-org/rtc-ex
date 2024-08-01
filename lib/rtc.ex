defmodule RTC do
  @moduledoc """
  RDF Triple Compounds.

  This library provides the `RTC.Compound` struct for working with
  RDF Triple Compounds.

  See the [RTC spec](https://w3id.org/rtc) for a general introduction into the
  vocabulary and the idea behind it and the [guide on RTC.ex](https://rdf-elixir.dev/rtc-ex/)
  for an introduction on working with this library.
  """

  alias RDF.Resource.Generator
  import RDF.Namespace

  def id, do: Generator.generate(generator_config())
  def id(args), do: Generator.generate(generator_config(), args)

  defp generator_config do
    Application.get_env(:rtc, :id, generator: RDF.BlankNode)
  end

  act_as_namespace(RTC.NS.RTC)
end
