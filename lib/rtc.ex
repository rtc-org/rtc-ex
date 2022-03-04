defmodule RTC do
  @moduledoc """
  RDF Triple Compounds
  """

  alias RDF.Resource.Generator

  @id_generator Application.get_env(:rtc, :id, generator: RDF.BlankNode)
                |> Generator.config()

  def id, do: Generator.generate(@id_generator, nil)
  def id(args), do: Generator.generate(@id_generator, args)

  ############################################################################
  # These alias functions for the RTC.NS.RTC namespace are mandatory.
  # Without them the property functions are inaccessible, since the namespace
  # can't be aliased, because it gets in conflict with the root namespace
  # of the project.

  defdelegate elementOf(), to: RTC.NS.RTC
  defdelegate elementOf(subject, objects), to: RTC.NS.RTC

  defdelegate elements(), to: RTC.NS.RTC
  defdelegate elements(subject, objects), to: RTC.NS.RTC

  defdelegate subCompoundOf(), to: RTC.NS.RTC
  defdelegate subCompoundOf(subject, objects), to: RTC.NS.RTC
end
