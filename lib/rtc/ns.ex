defmodule RTC.NS do
  @moduledoc """
  `RDF.Vocabulary.Namespace`s for the used vocabularies.
  """

  use RDF.Vocabulary.Namespace

  @vocabdoc """
  The RTC vocabulary.
  """
  defvocab RTC,
    base_iri: "http://example.com/rtc#",
    file: "rtc.ttl"
end
