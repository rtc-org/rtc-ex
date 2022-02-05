defmodule RTC.CompoundTest do
  use RTC.Case

  doctest RTC.Compound

  alias RTC.Compound

  @triples [{EX.S1, EX.P1, EX.O1}, {EX.S2, EX.P2, EX.O2}]
  @flat_compound %Compound{
    elements:
      MapSet.new([
        RDF.triple({EX.S1, EX.P1, EX.O1}),
        RDF.triple({EX.S2, EX.P2, EX.O2})
      ]),
    sub_compounds: MapSet.new(),
    annotations: RDF.description(EX.Compound)
  }

  @other_triples [{EX.S3, EX.P3, EX.O3}, {EX.S4, EX.P4, EX.O4}]
  @sub_compound Compound.new(@other_triples, EX.SubCompound)

  @nested_compound %Compound{
    elements:
      MapSet.new([
        RDF.triple({EX.S1, EX.P1, EX.O1}),
        RDF.triple({EX.S2, EX.P2, EX.O2})
      ]),
    sub_compounds: MapSet.new([@sub_compound]),
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
                 elements:
                   MapSet.new([
                     RDF.triple({EX.S, EX.P1, EX.O1}),
                     RDF.triple({EX.S, EX.P2, EX.O2})
                   ]),
                 sub_compounds: MapSet.new(),
                 annotations: RDF.description(EX.Compound)
               }
    end

    test "constructs a compound from a graph" do
      assert @triples |> RDF.graph() |> Compound.new(EX.Compound) == @flat_compound
    end

    test "duplicate triples" do
      assert Compound.new(@triples ++ @triples, EX.Compound) == @flat_compound
    end

    test "constructs a nested compound" do
      assert Compound.new(@triples, EX.Compound, sub_compound: @sub_compound) ==
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

    test "annotations are not added to sub-compounds" do
      nested_compound =
        Compound.new([{EX.S3, EX.P3, EX.O3}, {EX.S4, EX.P4, EX.O4}], EX.SubCompound)

      annotations = %{EX.ap1() => EX.AO1, EX.ap2() => EX.AO2}

      assert Compound.new(@triples, EX.Compound,
               sub_compound: nested_compound,
               annotations: annotations
             ) ==
               %Compound{
                 elements:
                   MapSet.new([
                     RDF.triple({EX.S1, EX.P1, EX.O1}),
                     RDF.triple({EX.S2, EX.P2, EX.O2})
                   ]),
                 sub_compounds: MapSet.new([nested_compound]),
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
        |> Graph.add(EX.SubCompound |> RTC.elements(@other_triples))
        |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})

      assert Compound.from_rdf(graph, EX.Compound) ==
               Compound.new(@triples, EX.Compound, sub_compound: @sub_compound)
    end

    test "gets a nested compound back (via rtc:elementOf)" do
      graph =
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add(@triples, add_annotations: {RTC.elementOf(), EX.Compound})
        |> Graph.add(@other_triples, add_annotations: {RTC.elementOf(), EX.SubCompound})
        |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})

      assert Compound.from_rdf(graph, EX.Compound) ==
               Compound.new(@triples, EX.Compound, sub_compound: @sub_compound)
    end
  end

  describe "to_rdf/1" do
    test "returns a graph with the triples and the RTC annotations with rtc:elementOf" do
      assert Compound.to_rdf(@flat_compound) ==
               RDF.graph(name: RDF.iri(EX.Compound))
               |> Graph.add(@triples, add_annotations: {RTC.elementOf(), RDF.iri(EX.Compound)})
    end

    test "with sub-compound" do
      assert Compound.to_rdf(@nested_compound) ==
               RDF.graph(name: RDF.iri(EX.Compound))
               |> Graph.add(@triples, add_annotations: {RTC.elementOf(), EX.Compound})
               |> Graph.add(@other_triples, add_annotations: {RTC.elementOf(), EX.SubCompound})
               |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
    end
  end
end
