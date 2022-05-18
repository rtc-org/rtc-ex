defmodule RTC.Case do
  use ExUnit.CaseTemplate

  use RDF.Vocabulary.Namespace
  defvocab EX, base_iri: "http://example.com/", terms: [], strict: false
  defvocab FOAF, base_iri: "http://xmlns.com/foaf/0.1/", terms: [], strict: false

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
      alias unquote(__MODULE__).{EX, FOAF}

      import RDF, only: [iri: 1, literal: 1, bnode: 1]
      import unquote(__MODULE__)

      import RDF.Sigils

      @compile {:no_warn_undefined, RTC.Case.EX}
      @compile {:no_warn_undefined, RTC.Case.FOAF}
    end
  end

  alias RTC.Compound
  alias RDF.Graph

  def graph(), do: graph(EX.Compound)
  def graph(nil), do: graph(nil, [])
  def graph(name) when is_atom(name), do: name |> RDF.iri() |> graph()
  def graph(%id_type{} = name) when id_type in [RDF.IRI, RDF.BlankNode], do: graph(name, [])
  def graph(data), do: graph(EX.Compound, data)

  def graph(name, data) do
    RDF.graph(
      name: name,
      prefixes: RDF.default_prefixes(rtc: RTC.NS.RTC),
      init: data
    )
  end

  @triples [{EX.S1, EX.P1, EX.O1}, {EX.S2, EX.P2, EX.O2}]
  def triples(), do: @triples

  @other_triples [{EX.S3, EX.P3, EX.O3}, {EX.S4, EX.P4, EX.O4}]
  def other_triples(), do: @other_triples

  @more_triples [{EX.S5, EX.P5, EX.O5}, {EX.S6, EX.P6, EX.O6}]
  def more_triples(), do: @more_triples

  @flat_compound %Compound{
    graph:
      Graph.new(@triples, name: EX.Compound, prefixes: RDF.default_prefixes(rtc: RTC.NS.RTC)),
    annotations: RDF.description(EX.Compound)
  }
  def flat_compound(), do: @flat_compound

  @empty_compound %Compound{
    graph: Graph.new(name: EX.Compound, prefixes: RDF.default_prefixes(rtc: RTC.NS.RTC)),
    annotations: RDF.description(EX.Compound)
  }
  def empty_compound(), do: @empty_compound

  @sub_compound Compound.new(@other_triples, EX.SubCompound)
  def sub_compound(), do: @sub_compound

  @nested_compound %Compound{
    graph:
      Graph.new(@triples, name: EX.Compound, prefixes: RDF.default_prefixes(rtc: RTC.NS.RTC)),
    sub_compounds: %{Compound.id(@sub_compound) => @sub_compound},
    annotations: RDF.description(EX.Compound)
  }
  def nested_compound(), do: @nested_compound

  @deeply_nested_compound %Compound{
    graph:
      Graph.new(@triples, name: EX.Compound, prefixes: RDF.default_prefixes(rtc: RTC.NS.RTC)),
    sub_compounds: %{
      Compound.id(@sub_compound) =>
        Compound.put_sub_compound(@sub_compound, Compound.new(@more_triples, EX.DeepCompound))
    },
    annotations: RDF.description(EX.Compound)
  }
  def deeply_nested_compound(), do: @deeply_nested_compound

  @compound_with_duplicate_triple_in_sub_compound %Compound{
    graph:
      Graph.new([RDF.triple({EX.S3, EX.P3, EX.O3}) | @triples],
        name: EX.Compound,
        prefixes: RDF.default_prefixes(rtc: RTC.NS.RTC)
      ),
    sub_compounds: %{Compound.id(@sub_compound) => @sub_compound},
    annotations: RDF.description(EX.Compound)
  }
  def compound_with_duplicate_triple_in_sub_compound(),
    do: @compound_with_duplicate_triple_in_sub_compound

  @compound_with_super_compound %Compound{
    @flat_compound
    | super_compounds: %{
        RDF.iri(EX.SuperCompound) =>
          RDF.description(EX.SuperCompound, init: {EX.inherited_p(), EX.inherited_o()})
      }
  }
  def compound_with_super_compound(), do: @compound_with_super_compound
end
