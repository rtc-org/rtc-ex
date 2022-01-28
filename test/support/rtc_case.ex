defmodule RTC.Case do
  use ExUnit.CaseTemplate

  use RDF.Vocabulary.Namespace
  defvocab EX, base_iri: "http://example.com/", terms: [], strict: false
  defvocab FOAF, base_iri: "http://xmlns.com/foaf/0.1/", terms: [], strict: false

  using do
    quote do
      alias RDF.{Dataset, Graph, Description, IRI, XSD, PrefixMap, PropertyMap, NS}
      alias RDF.NS.{RDFS, OWL}
      alias unquote(__MODULE__).{EX, FOAF}

      import RDF, only: [iri: 1, literal: 1, bnode: 1]
      import unquote(__MODULE__)

      import RDF.Sigils

      @compile {:no_warn_undefined, RTC.Case.EX}
      @compile {:no_warn_undefined, RTC.Case.FOAF}
    end
  end
end
