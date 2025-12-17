defmodule RTC.Compound.RDFDataSourceTest do
  use RTC.Case

  alias RDF.Data.Source

  test "structure_type/1" do
    assert Source.structure_type(flat_compound()) == :graph
  end

  test "subject/1" do
    assert Source.subject(flat_compound()) == nil
  end

  test "graph_name/1" do
    assert Source.graph_name(flat_compound()) == RDF.iri(EX.Compound)
  end

  describe "derive/3" do
    test "with :description" do
      assert Source.derive(flat_compound(), :description, subject: EX.S) ==
               {:ok, RDF.Description.new(EX.S)}

      assert Source.derive(flat_compound(), :description, []) ==
               {:error, :no_subject}
    end

    test "with :graph" do
      compound =
        Compound.new(triples(), EX.Compound,
          prefixes: [ex: EX],
          base_iri: "http://example.com/"
        )

      assert Source.derive(compound, :graph, []) ==
               {:ok,
                RDF.Graph.new(
                  name: EX.Compound,
                  prefixes: [ex: EX],
                  base_iri: "http://example.com/"
                )}

      assert Source.derive(compound, :graph, name: EX.NewGraph) ==
               {:ok,
                RDF.Graph.new(
                  name: EX.NewGraph,
                  prefixes: [ex: EX],
                  base_iri: "http://example.com/"
                )}

      assert Source.derive(compound, :graph, preserve_metadata: false) ==
               {:ok, RDF.Graph.new()}

      assert Source.derive(compound, :graph, preserve_metadata: false, name: EX.NewGraph) ==
               {:ok, RDF.Graph.new(name: EX.NewGraph)}
    end

    test "with :dataset" do
      assert Source.derive(flat_compound(), :dataset, []) ==
               {:ok, RDF.Dataset.new()}
    end
  end

  test "reduce/3" do
    assert {:done, result} =
             Source.reduce(flat_compound(), {:cont, []}, fn triple, acc ->
               {:cont, [triple | acc]}
             end)

    assert MapSet.new(result) == MapSet.new(triples() |> Enum.map(&Triple.new/1))
  end

  test "description/2" do
    assert Source.description(flat_compound(), EX.S1) ==
             {:ok, RDF.description(EX.S1, init: {EX.P1, EX.O1})}

    assert Source.description(flat_compound(), EX.Other) == :error
  end

  test "graph/2" do
    assert Source.graph(flat_compound(), EX.Compound) ==
             {:ok, Compound.graph(flat_compound())}

    assert Source.graph(flat_compound(), nil) == :error
    assert Source.graph(flat_compound(), EX.Other) == :error
  end

  test "graph_names/1" do
    assert Source.graph_names(flat_compound()) == {:ok, [RDF.iri(EX.Compound)]}
  end

  test "subjects/1" do
    assert {:ok, subjects} = Source.subjects(flat_compound())
    assert MapSet.new(subjects) == MapSet.new([RDF.iri(EX.S1), RDF.iri(EX.S2)])

    assert {:ok, subjects} = Source.subjects(nested_compound())

    assert MapSet.new(subjects) ==
             MapSet.new([RDF.iri(EX.S1), RDF.iri(EX.S2), RDF.iri(EX.S3), RDF.iri(EX.S4)])
  end

  test "statement_count/1" do
    assert Source.statement_count(flat_compound()) == {:ok, 2}
    assert Source.statement_count(nested_compound()) == {:ok, 4}
  end

  test "description_count/1" do
    assert Source.description_count(flat_compound()) == {:ok, 2}
    assert Source.description_count(nested_compound()) == {:ok, 4}
  end

  test "graph_count/1" do
    assert Source.graph_count(flat_compound()) == {:ok, 1}
  end

  test "add/2" do
    assert Source.add(flat_compound(), {EX.S3, EX.P3, EX.O3}) ==
             {:ok, Compound.add(flat_compound(), {EX.S3, EX.P3, EX.O3})}

    assert Source.add(flat_compound(), [{EX.S3, EX.P3, EX.O3}, {EX.S4, EX.P4, EX.O4}]) ==
             {:ok, Compound.add(flat_compound(), [{EX.S3, EX.P3, EX.O3}, {EX.S4, EX.P4, EX.O4}])}
  end

  test "delete/2" do
    assert Source.delete(flat_compound(), {EX.S1, EX.P1, EX.O1}) ==
             {:ok, Compound.delete(flat_compound(), {EX.S1, EX.P1, EX.O1})}

    assert Source.delete(flat_compound(), triples()) ==
             {:ok, empty_compound()}
  end

  describe "iteration scope" do
    test "includes asserted triples" do
      assert MapSet.new(RDF.Data.statements(flat_compound())) ==
               MapSet.new(triples(), &Triple.new/1)
    end

    test "includes unasserted triples" do
      assert MapSet.new(RDF.Data.statements(unasserted_flat_compound())) ==
               MapSet.new(triples(), &Triple.new/1)
    end

    test "includes both asserted and unasserted triples" do
      assert MapSet.new(RDF.Data.statements(mixed_asserted_flat_compound())) ==
               MapSet.new(triples(), &Triple.new/1)
    end

    test "includes sub-compound triples (flattened)" do
      assert MapSet.new(RDF.Data.statements(nested_compound())) ==
               MapSet.new(triples() ++ other_triples(), &Triple.new/1)
    end

    test "includes unasserted triples from sub-compounds" do
      assert MapSet.new(RDF.Data.statements(unasserted_nested_compound())) ==
               MapSet.new(triples() ++ other_triples(), &Triple.new/1)
    end

    test "does NOT include annotations" do
      compound = Compound.add_annotations(flat_compound(), {EX.annotation_p(), EX.annotation_o()})

      assert MapSet.new(RDF.Data.statements(compound)) ==
               MapSet.new(triples(), &Triple.new/1)
    end
  end

  describe "RDF.Data" do
    test "reduce/3" do
      result = RDF.Data.reduce(flat_compound(), [], fn stmt, acc -> [stmt | acc] end)
      assert MapSet.new(result) == MapSet.new(triples(), &Triple.new/1)
    end

    test "reduce_while/3" do
      assert RDF.Data.reduce_while(flat_compound(), 0, fn _stmt, acc ->
               if acc >= 1, do: {:halt, acc}, else: {:cont, acc + 1}
             end) == 1
    end

    test "map/2" do
      assert RDF.Data.map(flat_compound(), fn {s, _p, o} -> {s, EX.new(), o} end) ==
               graph([{EX.S1, EX.new(), EX.O1}, {EX.S2, EX.new(), EX.O2}])
    end

    test "map_reduce/3" do
      assert RDF.Data.map_reduce(flat_compound(), 0, fn {s, _p, o}, acc ->
               {{s, EX.new(), o}, acc + 1}
             end) ==
               {graph([{EX.S1, EX.new(), EX.O1}, {EX.S2, EX.new(), EX.O2}]), 2}
    end

    test "filter/2" do
      assert RDF.Data.filter(flat_compound(), fn {s, _p, _o} -> s == RDF.iri(EX.S1) end) ==
               graph([{EX.S1, EX.P1, EX.O1}])
    end

    test "reject/2" do
      assert RDF.Data.reject(flat_compound(), fn {s, _p, _o} -> s == RDF.iri(EX.S1) end) ==
               graph([{EX.S2, EX.P2, EX.O2}])
    end

    test "take/2" do
      assert %RDF.Graph{} = result = RDF.Data.take(flat_compound(), 1)
      assert RDF.Data.statement_count(result) == 1
    end

    test "delete/2" do
      assert RDF.Data.delete(flat_compound(), {EX.S1, EX.P1, EX.O1}) ==
               flat_compound([{EX.S2, EX.P2, EX.O2}])
    end

    test "pop/1" do
      assert {{_, _, _}, %Compound{} = remaining} = RDF.Data.pop(flat_compound())
      assert RDF.Data.statement_count(remaining) == 1

      assert RDF.Data.pop(empty_compound()) == {nil, empty_compound()}
    end

    test "merge/1" do
      # Same ID → Compound
      compound1 = Compound.new({EX.S1, EX.P1, EX.O1}, EX.Compound)
      compound2 = Compound.new({EX.S2, EX.P2, EX.O2}, EX.Compound)

      assert RDF.Data.merge([compound1, compound2]) == flat_compound()

      # Different IDs → Dataset (different named graphs)
      compound1 = Compound.new({EX.S1, EX.P1, EX.O1}, EX.Compound1)
      compound2 = Compound.new({EX.S2, EX.P2, EX.O2}, EX.Compound2)

      assert RDF.Data.merge([compound1, compound2]) ==
               RDF.dataset([
                 {EX.S1, EX.P1, EX.O1, EX.Compound1},
                 {EX.S2, EX.P2, EX.O2, EX.Compound2}
               ])
    end

    test "merge/2" do
      # Same ID → Compound
      compound1 = Compound.new({EX.S1, EX.P1, EX.O1}, EX.Compound)
      compound2 = Compound.new({EX.S2, EX.P2, EX.O2}, EX.Compound)

      assert RDF.Data.merge(compound1, compound2) == flat_compound()

      # Merge with unnamed Graph → Dataset (different graph names)
      graph = RDF.graph({EX.S3, EX.P3, EX.O3})

      assert RDF.Data.merge(compound1, graph) ==
               RDF.dataset([
                 {EX.S1, EX.P1, EX.O1, EX.Compound},
                 {EX.S3, EX.P3, EX.O3}
               ])
    end

    test "statements/1" do
      assert MapSet.new(RDF.Data.statements(flat_compound())) ==
               MapSet.new(triples(), &Triple.new/1)
    end

    test "triples/1" do
      assert MapSet.new(RDF.Data.triples(flat_compound())) ==
               MapSet.new(triples(), &Triple.new/1)
    end

    test "quads/1" do
      assert MapSet.new(RDF.Data.quads(flat_compound())) ==
               MapSet.new(triples(), fn {s, p, o} -> Quad.new(s, p, o, RDF.iri(EX.Compound)) end)
    end

    test "default_graph/1" do
      # Compound is a named graph, so default_graph returns an empty Graph
      assert RDF.Data.default_graph(flat_compound()) == RDF.Graph.new()
    end

    test "graph/2" do
      assert RDF.Data.graph(flat_compound(), nil) == RDF.Graph.new()
      assert RDF.Data.graph(flat_compound(), EX.Compound) == graph(triples())
      assert RDF.Data.graph(flat_compound(), EX.Other) == RDF.Graph.new(name: EX.Other)
    end

    test "graph/3" do
      assert RDF.Data.graph(flat_compound(), nil, :default) == :default
      assert RDF.Data.graph(flat_compound(), EX.Other, nil) == nil
      assert RDF.Data.graph(flat_compound(), EX.Compound, nil) == graph(triples())
    end

    test "graphs/1" do
      assert RDF.Data.graphs(flat_compound()) == [flat_compound()]
    end

    test "graph_names/1" do
      assert RDF.Data.graph_names(flat_compound()) == [RDF.iri(EX.Compound)]
    end

    test "descriptions/1" do
      assert RDF.Data.descriptions(flat_compound()) == [
               RDF.description(EX.S1, init: {EX.P1, EX.O1}),
               RDF.description(EX.S2, init: {EX.P2, EX.O2})
             ]
    end

    test "description/2" do
      assert RDF.Data.description(flat_compound(), EX.S1) ==
               RDF.description(EX.S1, init: {EX.P1, EX.O1})
    end

    test "description/3" do
      assert RDF.Data.description(flat_compound(), EX.S1, :default) ==
               RDF.Data.description(flat_compound(), EX.S1)

      assert RDF.Data.description(flat_compound(), EX.Other, nil) == nil
      assert RDF.Data.description(flat_compound(), EX.Other, :default) == :default
    end

    test "subjects/1" do
      assert MapSet.new(RDF.Data.subjects(flat_compound())) ==
               MapSet.new([RDF.iri(EX.S1), RDF.iri(EX.S2)])
    end

    test "predicates/1" do
      assert MapSet.new(RDF.Data.predicates(flat_compound())) ==
               MapSet.new([RDF.iri(EX.P1), RDF.iri(EX.P2)])
    end

    test "objects/1" do
      assert MapSet.new(RDF.Data.objects(flat_compound())) ==
               MapSet.new([RDF.iri(EX.O1), RDF.iri(EX.O2)])
    end

    test "object_resources/1" do
      assert MapSet.new(RDF.Data.object_resources(flat_compound())) ==
               MapSet.new([RDF.iri(EX.O1), RDF.iri(EX.O2)])
    end

    test "resources/1" do
      assert MapSet.new(RDF.Data.resources(flat_compound())) ==
               MapSet.new([
                 RDF.iri(EX.S1),
                 RDF.iri(EX.S2),
                 RDF.iri(EX.O1),
                 RDF.iri(EX.O2)
               ])

      assert MapSet.new(RDF.Data.resources(flat_compound(), predicates: true)) ==
               MapSet.new([
                 RDF.iri(EX.S1),
                 RDF.iri(EX.S2),
                 RDF.iri(EX.P1),
                 RDF.iri(EX.P2),
                 RDF.iri(EX.O1),
                 RDF.iri(EX.O2)
               ])
    end

    test "count/1" do
      assert RDF.Data.count(flat_compound()) == 2
      assert RDF.Data.count(nested_compound()) == 4
    end

    test "graph_count/1" do
      assert RDF.Data.graph_count(flat_compound()) == 1
    end

    test "statement_count/1" do
      assert RDF.Data.statement_count(flat_compound()) == 2
      assert RDF.Data.statement_count(nested_compound()) == 4
    end

    test "subject_count/1" do
      assert RDF.Data.subject_count(flat_compound()) == 2
      assert RDF.Data.subject_count(nested_compound()) == 4
    end

    test "predicate_count/1" do
      assert RDF.Data.predicate_count(flat_compound()) == 2
    end

    test "empty?/1" do
      refute RDF.Data.empty?(flat_compound())
      assert RDF.Data.empty?(empty_compound())
    end

    test "equal?/2" do
      assert RDF.Data.equal?(flat_compound(), Compound.graph(flat_compound()))
      assert RDF.Data.equal?(Compound.graph(flat_compound()), flat_compound())
    end

    test "include?/2" do
      assert RDF.Data.include?(flat_compound(), {EX.S1, EX.P1, EX.O1})
      refute RDF.Data.include?(flat_compound(), {EX.S1, EX.P1, EX.Other})
    end

    test "describes?/2" do
      assert RDF.Data.describes?(flat_compound(), EX.S1)
      refute RDF.Data.describes?(flat_compound(), EX.Other)
    end

    test "to_graph/1" do
      assert RDF.Data.to_graph(flat_compound()) == flat_compound()

      assert RDF.Data.to_graph(flat_compound(), native: true) ==
               RDF.graph(triples(), name: EX.Compound)
    end

    test "to_dataset/1" do
      expected_dataset =
        RDF.dataset([
          {EX.S1, EX.P1, EX.O1, EX.Compound},
          {EX.S2, EX.P2, EX.O2, EX.Compound}
        ])

      assert RDF.Data.to_dataset(flat_compound()) == expected_dataset
      assert RDF.Data.to_dataset(flat_compound(), native: true) == expected_dataset
    end
  end
end
