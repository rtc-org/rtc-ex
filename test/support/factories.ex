defmodule RTC.Factories do
  alias RTC.Compound
  alias RTC.TestNS.EX
  alias RDF.Graph

  @compile {:no_warn_undefined, RTC.TestNS.EX}

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

  def flat_compound(triples \\ @triples) do
    %Compound{
      asserted:
        Graph.new(triples, name: EX.Compound, prefixes: RDF.default_prefixes(rtc: RTC.NS.RTC)),
      annotations: RDF.description(EX.Compound)
    }
  end

  def unasserted_flat_compound(triples \\ @triples) do
    %Compound{empty_compound() | unasserted: Graph.new(triples)}
  end

  def mixed_asserted_flat_compound(
        asserted \\ [{EX.S1, EX.P1, EX.O1}],
        unasserted \\ [{EX.S2, EX.P2, EX.O2}]
      ) do
    %Compound{flat_compound(asserted) | unasserted: Graph.new(unasserted)}
  end

  def empty_compound(), do: flat_compound([])

  def sub_compound(triples \\ @other_triples), do: Compound.new(triples, EX.SubCompound)

  def unasserted_sub_compound(triples \\ @other_triples),
    do: Compound.new(triples, EX.SubCompound, assertion_mode: :unasserted)

  def nested_compound(triples \\ @triples, nested \\ sub_compound())

  def nested_compound(triples, %Compound{} = nested_compound) do
    %Compound{
      flat_compound(triples)
      | sub_compounds: %{Compound.id(nested_compound) => nested_compound}
    }
  end

  def nested_compound(triples, nested_triples) do
    nested_compound(triples, sub_compound(nested_triples))
  end

  def empty_nested_compound() do
    nested_compound([], sub_compound([]))
  end

  def unasserted_nested_compound(
        triples \\ @triples,
        nested_compound \\ unasserted_sub_compound()
      ) do
    %Compound{
      unasserted_flat_compound(triples)
      | sub_compounds: %{Compound.id(nested_compound) => nested_compound}
    }
  end

  def deeply_nested_compound(
        triples \\ @triples,
        nested_triples \\ @other_triples,
        deep_nested_triples \\ @more_triples
      ) do
    %Compound{
      flat_compound(triples)
      | sub_compounds: %{
          Compound.id(sub_compound()) =>
            Compound.put_sub_compound(
              sub_compound(nested_triples),
              Compound.new(deep_nested_triples, EX.DeepCompound)
            )
        }
    }
  end

  def compound_with_duplicate_triple_in_sub_compound(
        triples \\ @triples,
        nested_triples \\ @other_triples
      ) do
    if Enum.empty?(nested_triples) do
      raise "nested_triples must not be empty"
    end

    nested_compound([hd(nested_triples) | triples])
  end

  def compound_with_super_compound(triples \\ @triples) do
    %Compound{
      flat_compound(triples)
      | super_compounds: %{
          RDF.iri(EX.SuperCompound) =>
            RDF.description(EX.SuperCompound, init: {EX.inherited_p(), EX.inherited_o()})
        }
    }
  end
end
