defmodule RTC.NS do
  @moduledoc """
  `RDF.Vocabulary.Namespace`s for the used vocabularies.
  """

  use RDF.Vocabulary.Namespace

  @vocabdoc """
  The RTC vocabulary.

  Since this module has the same basename as the top-level module, you can't
  alias it. Therefore, the top-level `RTC` module has delegators for all of the
  property functions in this module, so you can use them directly on the
  top-level module without an alias.

  See <https://w3id.org/rtc>
  """
  defvocab RTC,
    base_iri: "https://w3id.org/rtc#",
    file: "rtc.ttl",
    alias: [
      element_of: "elementOf",
      sub_compound_of: "subCompoundOf"
    ]
end
