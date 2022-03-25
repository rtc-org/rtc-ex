defmodule RTC.SPARQLTest do
  @moduledoc """
  These tests require a Ontotext GraphDB to be running on port 7200 and are
  therefor ignored by default. They can be run via

      $ mix test.sparql_client

  """

  use RTC.Case
  @moduletag :sparql_client

  alias RTC.Compound

  @repository_id "__RTC_TEST_SUITE__"
  @host "http://localhost:7200"
  @endpoint "#{@host}/repositories/#{@repository_id}"
  @update_endpoint "#{@host}/repositories/#{@repository_id}/statements"

  setup_all do
    # We have to start this manually, since we can't define Hackney properly
    # as a test dependency, because it's a hard dependency of JSON-LD.ex at the
    # moment.
    Application.ensure_all_started(:hackney)
    :ok
  end

  setup do
    create_repository()
    on_exit(&delete_repository/0)
    :ok
  end

  describe "from_sparql!/3" do
    test "when the compound is not present" do
      assert from_sparql!(EX.Compound) == empty_compound()
    end

    test "retrieves a compound from a graph when it's annotated via rtc:elements" do
      RDF.graph()
      |> Graph.add(triples())
      |> Graph.add(EX.Compound |> RTC.elements(triples()))
      |> insert()

      assert from_sparql!(EX.Compound) ==
               Compound.new(triples(), EX.Compound)
    end

    test "retrieves a compound from a graph when it's annotated via rtc:elementOf" do
      RDF.graph()
      |> Graph.add(triples(), add_annotations: {RTC.elementOf(), RDF.iri(EX.Compound)})
      |> insert_segregated()

      assert from_sparql!(EX.Compound) ==
               Compound.new(triples(), EX.Compound)
    end

    test "retrieves a compound from a graph when it's annotated via rtc:elements and rtc:elementOf" do
      triples =
        [first | rest] = [{EX.S1, EX.P1, EX.O1}, {EX.S2, EX.P2, EX.O2}, {EX.S3, EX.P3, EX.O3}]

      RDF.graph(name: RDF.iri(EX.Compound))
      |> Graph.add(first, add_annotations: {RTC.elementOf(), RDF.iri(EX.Compound)})
      |> Graph.add(rest)
      |> Graph.add(EX.Compound |> RTC.elements(rest))
      |> insert_segregated()

      assert from_sparql!(EX.Compound) ==
               Compound.new(triples, EX.Compound)
    end

    test "retrieves a nested compound (via rtc:elements)" do
      RDF.graph(name: RDF.iri(EX.Compound))
      |> Graph.add(triples())
      |> Graph.add(EX.Compound |> RTC.elements(triples()))
      |> Graph.add(other_triples())
      |> Graph.add(EX.SubCompound |> RTC.elements(other_triples()))
      |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
      |> insert_segregated()

      assert from_sparql!(EX.Compound) ==
               Compound.new(triples(), EX.Compound, sub_compound: sub_compound())
    end

    test "retrieves a nested compound (via rtc:elementOf)" do
      RDF.graph(name: RDF.iri(EX.Compound))
      |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
      |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
      |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
      |> insert_segregated()

      assert from_sparql!(EX.Compound) ==
               Compound.new(triples(), EX.Compound, sub_compound: sub_compound())
    end

    test "retrieves annotations (rtc:elements)" do
      RDF.graph()
      |> Graph.add(triples())
      |> Graph.add(EX.Compound |> RTC.elements(triples()) |> EX.foo(EX.Bar))
      |> insert()

      assert from_sparql!(EX.Compound) ==
               Compound.new(triples(), EX.Compound, annotations: {EX.foo(), EX.Bar})
    end

    test "retrieves annotations (rtc:elementOf)" do
      RDF.graph()
      |> Graph.add(triples(), add_annotations: {RTC.elementOf(), RDF.iri(EX.Compound)})
      |> Graph.add(EX.Compound |> EX.foo(EX.Bar))
      |> insert_segregated()

      assert from_sparql!(EX.Compound) ==
               Compound.new(triples(), EX.Compound, annotations: {EX.foo(), EX.Bar})
    end

    test "retrieves annotations (sub-compounds)" do
      RDF.graph(name: RDF.iri(EX.Compound))
      |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
      |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
      |> Graph.add(EX.Compound |> EX.foo1(EX.Bar1))
      |> Graph.add(EX.SubCompound |> RTC.subCompoundOf(EX.Compound) |> EX.foo2(EX.Bar2))
      |> insert_segregated()

      assert from_sparql!(EX.Compound) ==
               Compound.new(triples(), EX.Compound,
                 sub_compound: Compound.add_annotations(sub_compound(), {EX.foo2(), EX.Bar2}),
                 annotations: {EX.foo1(), EX.Bar1}
               )
    end

    test "retrieves a deeply nested compound (via rtc:elements)" do
      RDF.graph(name: RDF.iri(EX.Compound))
      |> Graph.add(triples())
      |> Graph.add(EX.Compound |> RTC.elements(triples()))
      |> Graph.add(other_triples())
      |> Graph.add(EX.SubCompound |> RTC.elements(other_triples()))
      |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
      |> Graph.add(more_triples())
      |> Graph.add(EX.DeepCompound |> RTC.elements(more_triples()))
      |> Graph.add({EX.DeepCompound, RTC.subCompoundOf(), EX.SubCompound})
      |> insert_segregated()

      assert from_sparql!(EX.Compound) == deeply_nested_compound()
    end

    test "retrieves a deeply nested compound (via rtc:elementOf)" do
      RDF.graph(name: RDF.iri(EX.Compound))
      |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
      |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
      |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
      |> Graph.add(more_triples(), add_annotations: {RTC.elementOf(), EX.DeepCompound})
      |> Graph.add({EX.DeepCompound, RTC.subCompoundOf(), EX.SubCompound})
      |> insert_segregated()

      assert from_sparql!(EX.Compound) == deeply_nested_compound()
    end

    test "cyclic sub-compounds raise an error" do
      RDF.graph(name: RDF.iri(EX.Compound))
      |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
      |> Graph.add({EX.Compound, RTC.subCompoundOf(), EX.Compound})
      |> insert_segregated()

      assert_raise RuntimeError, "circle in sub-compound #{RDF.iri(EX.Compound)}", fn ->
        from_sparql!(EX.Compound)
      end

      RDF.graph(name: RDF.iri(EX.Compound))
      |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
      |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
      |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
      |> Graph.add({EX.Compound, RTC.subCompoundOf(), EX.SubCompound})
      |> insert_segregated()

      assert_raise RuntimeError, "circle in sub-compound #{RDF.iri(EX.Compound)}", fn ->
        from_sparql!(EX.Compound)
      end
    end
  end

  defp create_repository do
    HTTPoison.put(
      @endpoint,
      """
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
      @prefix rep: <http://www.openrdf.org/config/repository#>.
      @prefix sr: <http://www.openrdf.org/config/repository/sail#>.
      @prefix sail: <http://www.openrdf.org/config/sail#>.
      @prefix ms: <http://www.openrdf.org/config/sail/memory#>.

      [] a rep:Repository ;
         rep:repositoryID "#{@repository_id}" ;
         rdfs:label "#{@repository_id}" ;
         rep:repositoryImpl [
            rep:repositoryType "openrdf:SailRepository" ;
            sr:sailImpl [
               sail:sailType "openrdf:MemoryStore" ;
               ms:persist false ;
               ms:syncDelay 120
            ]
         ].
      """,
      [{"Content-Type", "text/turtle"}],
      recv_timeout: 2000
    )
  end

  defp delete_repository do
    HTTPoison.delete(@endpoint)
  end

  defp insert(%Compound{} = compound) do
    compound
    # We need the elements-style since GraphDB doesn't support the annotation syntax used in the elementOf-style
    |> Compound.to_rdf(element_style: :elements)
    |> insert()
  end

  defp insert(data) do
    :ok = SPARQL.Client.insert_data(data, @update_endpoint)
  end

  # This function works around GraphDBs inability to handle the annotation syntax
  defp insert_segregated(graph) do
    {graph, annotations} =
      graph
      |> Graph.descriptions()
      |> Enum.reduce({graph, Graph.new()}, fn
        %{subject: triple} = description, {graph, annotations} when is_tuple(triple) ->
          {
            Graph.delete_descriptions(graph, description.subject),
            Graph.add(annotations, description)
          }

        _, {graph, annotations} ->
          {graph, annotations}
      end)

    insert(graph)
    insert(annotations)
  end

  defp from_sparql!(id) do
    opts = [accept_header: "application/x-turtlestar", result_format: :turtle]
    Compound.from_sparql!(@endpoint, id, opts)
  end
end
