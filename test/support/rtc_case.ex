defmodule RTC.Case do
  use ExUnit.CaseTemplate

  using do
    quote do
      alias RTC.Compound

      alias RDF.{
        Dataset,
        Graph,
        Description,
        Triple,
        IRI,
        BlankNode,
        XSD,
        PrefixMap,
        PropertyMap,
        NS
      }

      alias RDF.NS.{RDFS, OWL}
      alias RTC.TestNS.{EX, FOAF}

      import RDF, only: [iri: 1, literal: 1, bnode: 1]
      import unquote(__MODULE__)

      import RDF.Sigils

      import RTC.Factories

      @compile {:no_warn_undefined, RTC.TestNS.EX}
      @compile {:no_warn_undefined, RTC.TestNS.FOAF}
    end
  end
end
