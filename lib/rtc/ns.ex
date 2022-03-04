defmodule RTC.NS do
  @moduledoc """
  `RDF.Vocabulary.Namespace`s for the used vocabularies.
  """

  use RDF.Vocabulary.Namespace

  @vocabdoc """
  The RTC vocabulary.
  """
  defvocab RTC,
    base_iri: "https://w3id.org/rtc#",
    file: "rtc.ttl",
    alias: [
      element_of: "elementOf",
      sub_compound_of: "subCompoundOf"
    ]
end
