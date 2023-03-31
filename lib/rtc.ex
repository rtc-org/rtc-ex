defmodule RTC do
  @moduledoc """
  RDF Triple Compounds.

  This library provides the `RTC.Compound` struct for working with
  RDF Triple Compounds.

  See the [RTC spec](https://rtc-org.github.io/spec) for a general introduction into the
  vocabulary and the idea behind it and the [guide on RTC.ex](https://rdf-elixir.dev/rtc-ex/)
  for an introduction on working with this library.
  """

  alias RDF.Resource.Generator

  def id, do: Generator.generate(generator_config())
  def id(args), do: Generator.generate(generator_config(), args)

  defp generator_config do
    Application.get_env(:rtc, :id, generator: RDF.BlankNode)
  end

  ############################################################################
  # These alias functions for the RTC.NS.RTC namespace are mandatory.
  # Without them the property functions are inaccessible, since the namespace
  # can't be aliased, because it gets in conflict with the root namespace
  # of the project.

  defdelegate elementOf(), to: RTC.NS.RTC
  defdelegate elementOf(subject, objects), to: RTC.NS.RTC
  defdelegate element_of(), to: RTC.NS.RTC
  defdelegate element_of(subject, objects), to: RTC.NS.RTC

  defdelegate elements(), to: RTC.NS.RTC
  defdelegate elements(subject, objects), to: RTC.NS.RTC

  defdelegate subCompoundOf(), to: RTC.NS.RTC
  defdelegate subCompoundOf(subject, objects), to: RTC.NS.RTC
  defdelegate sub_compound_of(), to: RTC.NS.RTC
  defdelegate sub_compound_of(subject, objects), to: RTC.NS.RTC
end
