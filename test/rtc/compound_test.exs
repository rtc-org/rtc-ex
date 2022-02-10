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

  @empty_compound %Compound{
    elements: MapSet.new(),
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

  @compound_with_duplicate_element_in_sub_compound %Compound{
    elements:
      MapSet.new([
        RDF.triple({EX.S1, EX.P1, EX.O1}),
        RDF.triple({EX.S2, EX.P2, EX.O2}),
        RDF.triple({EX.S3, EX.P3, EX.O3})
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
      assert Compound.from_rdf(RDF.graph(), EX.Compound) == @empty_compound
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

    test "cyclic sub-compounds raise an error" do
      assert_raise RuntimeError, "circle in sub-compound #{RDF.iri(EX.Compound)}", fn ->
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add(@triples, add_annotations: {RTC.elementOf(), EX.Compound})
        |> Graph.add({EX.Compound, RTC.subCompoundOf(), EX.Compound})
        |> Compound.from_rdf(EX.Compound)
      end

      assert_raise RuntimeError, "circle in sub-compound #{RDF.iri(EX.SubCompound)}", fn ->
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add(@triples, add_annotations: {RTC.elementOf(), EX.Compound})
        |> Graph.add(@other_triples, add_annotations: {RTC.elementOf(), EX.SubCompound})
        |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
        |> Graph.add({EX.Compound, RTC.subCompoundOf(), EX.SubCompound})
        |> Compound.from_rdf(EX.Compound)
      end
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

    test "an empty compound" do
      assert Compound.to_rdf(@empty_compound) == RDF.graph(name: RDF.iri(EX.Compound))
    end

    test "a compound with a duplicate elements in a sub-compound" do
      assert Compound.to_rdf(@compound_with_duplicate_element_in_sub_compound) ==
               RDF.graph(name: RDF.iri(EX.Compound))
               |> Graph.add([{EX.S3, EX.P3, EX.O3} | @triples],
                 add_annotations: {RTC.elementOf(), EX.Compound}
               )
               |> Graph.add(@other_triples, add_annotations: {RTC.elementOf(), EX.SubCompound})
               |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
    end
  end

  describe "element_set/1" do
    test "returns the element triples as a list" do
      assert Compound.element_set(@flat_compound) ==
               Enum.map(@triples, &RDF.triple/1) |> MapSet.new()
    end

    test "includes the elements of nested compounds" do
      assert Compound.element_set(@nested_compound) ==
               Enum.map(@triples ++ @other_triples, &RDF.triple/1) |> MapSet.new()
    end

    test "an empty compound" do
      assert Compound.element_set(@empty_compound) == MapSet.new()
    end

    test "a compound with a duplicate elements in a sub-compound" do
      assert Compound.element_set(@compound_with_duplicate_element_in_sub_compound) ==
               Enum.map(@triples ++ @other_triples, &RDF.triple/1) |> MapSet.new()
    end
  end

  describe "elements/1" do
    test "returns the element triples as a list" do
      assert Compound.elements(@flat_compound) == Enum.map(@triples, &RDF.triple/1)
    end

    test "includes the elements of nested compounds" do
      assert Compound.elements(@nested_compound) ==
               Enum.map(@triples ++ @other_triples, &RDF.triple/1)
    end

    test "an empty compound" do
      assert Compound.elements(@empty_compound) == []
    end

    test "a compound with a duplicate elements in a sub-compound" do
      assert Compound.elements(@compound_with_duplicate_element_in_sub_compound) ==
               Enum.map(@triples ++ @other_triples, &RDF.triple/1)
    end
  end

  describe "element?/2" do
    test "returns whether the element is a member of the compound" do
      Enum.each(@triples, fn triple ->
        assert Compound.element?(@flat_compound, triple) == true
      end)

      assert Compound.element?(@flat_compound, {EX.S2, EX.P2, EX.O3}) == false
    end

    test "takes nested compound into account" do
      Enum.each(@triples ++ @other_triples, fn triple ->
        assert Compound.element?(@nested_compound, triple) == true
      end)

      assert Compound.element?(@nested_compound, {EX.S2, EX.P2, EX.O3}) == false
    end
  end

  describe "size/1" do
    test "an empty compound" do
      assert Compound.size(@empty_compound) == 0
    end

    test "a flat compound" do
      assert Compound.size(@flat_compound) == 2
    end

    test "a nested compound" do
      assert Compound.size(@nested_compound) == 4
    end

    test "a compound with a duplicate elements in a sub-compound" do
      assert Compound.size(@compound_with_duplicate_element_in_sub_compound) == 4
    end
  end

  describe "Enumerable protocol" do
    test "Enum.count" do
      assert Enum.count(@nested_compound) == 4
    end

    test "Enum.member?" do
      Enum.each(@triples, fn triple ->
        assert Enum.member?(@flat_compound, triple) == true
      end)

      assert Enum.member?(@flat_compound, {EX.S2, EX.P2, EX.O3}) == false
    end

    test "Enum.reduce" do
      assert Enum.reduce(@flat_compound, [], fn triple, acc -> [triple | acc] end) ==
               @triples |> Enum.map(&RDF.triple/1) |> Enum.reverse()

      assert Enum.reduce(@nested_compound, [], fn triple, acc -> [triple | acc] end) ==
               (@triples ++ @other_triples) |> Enum.map(&RDF.triple/1) |> Enum.reverse()
    end

    test "Enum.at (for Enumerable.slice/1)" do
      assert Enum.at(@flat_compound, 0) == {RDF.iri(EX.S1), RDF.iri(EX.P1), RDF.iri(EX.O1)}
      assert Enum.at(@flat_compound, 1) == {RDF.iri(EX.S2), RDF.iri(EX.P2), RDF.iri(EX.O2)}
      assert Enum.at(@flat_compound, 2) == nil
      assert Enum.at(@nested_compound, 0) == {RDF.iri(EX.S1), RDF.iri(EX.P1), RDF.iri(EX.O1)}
      assert Enum.at(@nested_compound, 1) == {RDF.iri(EX.S2), RDF.iri(EX.P2), RDF.iri(EX.O2)}
      assert Enum.at(@nested_compound, 2) == {RDF.iri(EX.S3), RDF.iri(EX.P3), RDF.iri(EX.O3)}
      assert Enum.at(@nested_compound, 3) == {RDF.iri(EX.S4), RDF.iri(EX.P4), RDF.iri(EX.O4)}
      assert Enum.at(@nested_compound, 4) == nil
    end
  end
end
