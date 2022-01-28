defmodule RTC.CompoundTest do
  use RTC.Case

  doctest RTC.Compound

  alias RTC.Compound

  @triples [{EX.S1, EX.P1, EX.O1}, {EX.S2, EX.P2, EX.O2}]
  @flat_compound %Compound{
    elements: %{
      RDF.triple({EX.S1, EX.P1, EX.O1}) => nil,
      RDF.triple({EX.S2, EX.P2, EX.O2}) => nil
    },
    annotations: RDF.description(EX.Compound)
  }

  @other_triples [{EX.S3, EX.P3, EX.O3}, {EX.S4, EX.P4, EX.O4}]
  @other_compound Compound.new(@other_triples, EX.NestedCompound)

  @nested_compound %Compound{
    elements: %{
      RDF.triple({EX.S1, EX.P1, EX.O1}) => nil,
      RDF.triple({EX.S2, EX.P2, EX.O2}) => nil,
      Compound.id(@other_compound) => @other_compound
    },
    annotations: RDF.description(EX.Compound)
  }

  describe "new/2" do
    test "constructs a compound from a list of triples" do
      assert Compound.new(@triples, EX.Compound) == @flat_compound
    end

    test "constructs a compound from a description" do
      description = RDF.description(EX.S, init: [{EX.P1, EX.O1}, {EX.P2, EX.O2}])

      assert Compound.new(description, EX.Compound) ==
               %Compound{
                 elements: %{
                   RDF.triple({EX.S, EX.P1, EX.O1}) => nil,
                   RDF.triple({EX.S, EX.P2, EX.O2}) => nil
                 },
                 annotations: RDF.description(EX.Compound)
               }
    end

    test "constructs a compound from a graph" do
      assert @triples |> RDF.graph() |> Compound.new(EX.Compound) == @flat_compound
    end

    test "constructs a nested compound" do
      assert Compound.new([@other_compound | @triples], EX.Compound) ==
               @nested_compound
    end
  end

  describe "new/3" do
    test "compound annotations are stored" do
      annotations = %{EX.ap1() => EX.AO1, EX.ap2() => EX.AO2}

      assert Compound.new(@triples, EX.Compound, annotations: annotations) ==
               %Compound{
                 @flat_compound
                 | annotations: RDF.description(EX.Compound, init: annotations)
               }
    end

    test "annotations are not added to nested compounds" do
      nested_compound =
        Compound.new([{EX.S3, EX.P3, EX.O3}, {EX.S4, EX.P4, EX.O4}], EX.NestedCompound)

      triples = [{EX.S1, EX.P1, EX.O1}, nested_compound, {EX.S2, EX.P2, EX.O2}]
      annotations = %{EX.ap1() => EX.AO1, EX.ap2() => EX.AO2}

      assert Compound.new(triples, EX.Compound, annotations: annotations) ==
               %Compound{
                 elements: %{
                   RDF.triple({EX.S1, EX.P1, EX.O1}) => nil,
                   RDF.triple({EX.S2, EX.P2, EX.O2}) => nil,
                   RDF.iri(EX.NestedCompound) => nested_compound
                 },
                 annotations: RDF.description(EX.Compound, init: annotations)
               }
    end
  end

  describe "from_rdf/2" do
    test "when the compound is not present" do
      assert Compound.from_rdf(RDF.graph(), EX.Compound) ==
               Compound.new([], EX.Compound)
    end

    test "gets a compound back from a graph when it's annotated via rtc:elements" do
      graph =
        RDF.graph()
        |> Graph.add(@triples)
        |> Graph.add(EX.Compound |> RTC.elements(@triples))

      assert Compound.from_rdf(graph, EX.Compound) ==
               Compound.new(@triples, EX.Compound)
    end

    test "gets a compound back from a graph when it's annotated via rtc:elementOf" do
      graph =
        RDF.graph()
        |> Graph.add(@triples, add_annotations: {RTC.elementOf(), RDF.iri(EX.Compound)})

      assert Compound.from_rdf(graph, EX.Compound) ==
               Compound.new(@triples, EX.Compound)
    end

    test "gets a compound back from a graph when it's annotated via rtc:elements and rtc:elementOf" do
      triples =
        [first | rest] = [{EX.S1, EX.P1, EX.O1}, {EX.S2, EX.P2, EX.O2}, {EX.S3, EX.P3, EX.O3}]

      graph =
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add(first, add_annotations: {RTC.elementOf(), RDF.iri(EX.Compound)})
        |> Graph.add(rest)
        |> Graph.add(EX.Compound |> RTC.elements(rest))

      assert Compound.from_rdf(graph, EX.Compound) ==
               Compound.new(triples, EX.Compound)
    end

    test "gets a nested compound back (via rtc:elements)" do
      graph =
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add(@triples)
        |> Graph.add(EX.Compound |> RTC.elements(@triples))
        |> Graph.add(@other_triples)
        |> Graph.add(EX.NestedCompound |> RTC.elements(@other_triples))
        |> Graph.add({EX.NestedCompound, RTC.elementOf(), EX.Compound})

      assert Compound.from_rdf(graph, EX.Compound) ==
               Compound.new([@other_compound | @triples], EX.Compound)
    end

    test "gets a nested compound back (via rtc:elementOf)" do
      graph =
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add(@triples, add_annotations: {RTC.elementOf(), EX.Compound})
        |> Graph.add(@other_triples, add_annotations: {RTC.elementOf(), EX.NestedCompound})
        |> Graph.add({EX.NestedCompound, RTC.elementOf(), EX.Compound})

      assert Compound.from_rdf(graph, EX.Compound) ==
               Compound.new([@other_compound | @triples], EX.Compound)
    end

    test "when a non-existing resource is used as a nested compound" do
      graph =
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add(@triples)
        |> Graph.add(EX.Compound |> RTC.elements([EX.UndefinedResource | @triples]))

      assert Compound.from_rdf(graph, EX.Compound) ==
               Compound.new([Compound.new([], EX.UndefinedResource) | @triples], EX.Compound)

      graph =
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add(@triples, add_annotations: {RTC.elementOf(), EX.Compound})
        |> Graph.add({EX.UndefinedResource, RTC.elementOf(), EX.Compound})

      assert Compound.from_rdf(graph, EX.Compound) ==
               Compound.new([Compound.new([], EX.UndefinedResource) | @triples], EX.Compound)
    end
  end

  describe "to_rdf/1" do
    test "returns a graph with the triples and the RTC annotations with rtc:elementOf" do
      assert Compound.to_rdf(@flat_compound) ==
               RDF.graph(name: RDF.iri(EX.Compound))
               |> Graph.add(@triples, add_annotations: {RTC.elementOf(), RDF.iri(EX.Compound)})
    end

    test "with nested compound (with rtc:elementOf)" do
      assert Compound.to_rdf(@nested_compound) ==
               RDF.graph(name: RDF.iri(EX.Compound))
               |> Graph.add(@triples, add_annotations: {RTC.elementOf(), EX.Compound})
               |> Graph.add(@other_triples, add_annotations: {RTC.elementOf(), EX.NestedCompound})
               |> Graph.add({EX.NestedCompound, RTC.elementOf(), EX.Compound})
    end
  end
end
