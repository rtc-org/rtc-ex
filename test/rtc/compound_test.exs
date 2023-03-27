defmodule RTC.CompoundTest do
  use RTC.Case

  doctest RTC.Compound

  describe "new with an explicitly given id" do
    test "constructs a compound with asserted triples" do
      # from a list of triples
      assert Compound.new(triples(), EX.Compound) == flat_compound()

      # from a description
      description = RDF.description(EX.S, init: [{EX.P1, EX.O1}, {EX.P2, EX.O2}])

      assert Compound.new(description, EX.Compound) ==
               flat_compound([{EX.S, EX.P1, EX.O1}, {EX.S, EX.P2, EX.O2}])

      # from a graph
      assert triples() |> RDF.graph() |> Compound.new(EX.Compound) == flat_compound()
    end

    test "constructs a compound with unasserted triples via :unasserted opt" do
      # from a list of triples
      assert Compound.new([], EX.Compound, unasserted: triples()) == unasserted_flat_compound()

      # from a description
      description = RDF.description(EX.S, init: [{EX.P1, EX.O1}, {EX.P2, EX.O2}])

      assert Compound.new([], EX.Compound, unasserted: description) ==
               unasserted_flat_compound([{EX.S, EX.P1, EX.O1}, {EX.S, EX.P2, EX.O2}])

      # from a graph
      assert Compound.new([], EX.Compound, unasserted: RDF.graph(triples())) ==
               unasserted_flat_compound()
    end

    test "constructs a compound with unasserted triples via :assertion_mode opt" do
      # from a list of triples
      assert Compound.new(triples(), EX.Compound, assertion_mode: :unasserted) ==
               unasserted_flat_compound()

      # from a description
      description = RDF.description(EX.S, init: [{EX.P1, EX.O1}, {EX.P2, EX.O2}])

      assert Compound.new(description, EX.Compound, assertion_mode: :unasserted) ==
               unasserted_flat_compound([{EX.S, EX.P1, EX.O1}, {EX.S, EX.P2, EX.O2}])

      # from a graph
      assert triples() |> RDF.graph() |> Compound.new(EX.Compound, assertion_mode: :unasserted) ==
               unasserted_flat_compound()
    end

    test "constructs a compound with asserted and unasserted triples" do
      assert Compound.new([{EX.S1, EX.P1, EX.O1}], EX.Compound,
               unasserted: [{EX.S2, EX.P2, EX.O2}]
             ) ==
               mixed_asserted_flat_compound()

      assert Compound.new([], EX.Compound,
               asserted: {EX.S1, EX.P1, EX.O1},
               unasserted: [{EX.S2, EX.P2, EX.O2}]
             ) ==
               mixed_asserted_flat_compound()
    end

    test "the same triple as both asserted and unasserted" do
      assert Compound.new([], EX.Compound, asserted: triples(), unasserted: triples()) ==
               flat_compound()

      assert Compound.new(triples(), EX.Compound, unasserted: [{EX.S2, EX.P2, EX.O2}]) ==
               flat_compound()

      assert Compound.new(triples(), EX.Compound,
               asserted: [{EX.S2, EX.P2, EX.O2}],
               assertion_mode: :unasserted
             ) ==
               unasserted_flat_compound()
    end

    test "duplicate triples" do
      assert Compound.new(triples() ++ triples(), EX.Compound) == flat_compound()
    end

    test "constructs a nested compound" do
      assert Compound.new(triples(), EX.Compound, sub_compounds: sub_compound()) ==
               nested_compound()
    end

    test "compound annotations are stored" do
      annotations = %{EX.ap1() => EX.AO1, EX.ap2() => EX.AO2}

      assert Compound.new(triples(), EX.Compound, annotations: annotations) ==
               %Compound{
                 flat_compound()
                 | annotations: RDF.description(EX.Compound, init: annotations)
               }
    end

    test "with annotations given as a description about another resource" do
      annotations = Description.new(EX.Other, init: %{EX.ap1() => EX.AO1, EX.ap2() => EX.AO2})

      assert Compound.new(triples(), EX.Compound, annotations: annotations) ==
               %Compound{
                 flat_compound()
                 | annotations: RDF.description(EX.Compound, init: annotations)
               }
    end

    test "annotations are not added to sub-compounds" do
      nested_compound =
        Compound.new([{EX.S3, EX.P3, EX.O3}, {EX.S4, EX.P4, EX.O4}], EX.SubCompound)

      annotations = %{EX.ap1() => EX.AO1, EX.ap2() => EX.AO2}

      assert Compound.new(triples(), EX.Compound,
               sub_compounds: nested_compound,
               annotations: annotations
             ) ==
               %Compound{
                 asserted: graph(triples()),
                 sub_compounds: %{Compound.id(nested_compound) => nested_compound},
                 annotations: RDF.description(EX.Compound, init: annotations)
               }
    end

    test "super_compounds opt" do
      assert Compound.new(triples(), EX.Compound,
               super_compounds:
                 RDF.description(EX.SuperCompound, init: {EX.inherited_p(), EX.inherited_o()})
             ) ==
               compound_with_super_compound()

      assert Compound.new(triples(), EX.Compound, super_compounds: EX.SuperCompound) ==
               Compound.new(triples(), EX.Compound,
                 super_compounds: RDF.description(EX.SuperCompound)
               )

      assert Compound.new(triples(), EX.Compound,
               super_compounds: [EX.SuperCompound1, EX.SuperCompound2]
             ) ==
               Compound.put_super_compound(flat_compound(), [
                 Description.new(EX.SuperCompound1),
                 Description.new(EX.SuperCompound2)
               ])

      assert Compound.new(triples(), EX.Compound,
               super_compounds:
                 Compound.new([], EX.SuperCompound,
                   annotations: {EX.inherited_p(), EX.inherited_o()}
                 )
             ) ==
               compound_with_super_compound()
    end

    test "with blank node id" do
      assert Compound.new(triples(), ~B"Compound") ==
               %Compound{
                 asserted: Graph.new(triples(), prefixes: RDF.default_prefixes(rtc: RTC.NS.RTC)),
                 annotations: RDF.description(~B"Compound")
               }
    end

    test "graph opts" do
      opts = [
        name: EX.Graph,
        base_iri: "http://base_iri/",
        prefixes: [ex: EX],
        init: {EX.Ignored, EX.p(), "init"}
      ]

      assert Compound.new(triples(), EX.Compound, opts) ==
               %Compound{
                 asserted: Graph.new(triples(), opts),
                 annotations: RDF.description(EX.Compound)
               }
    end
  end

  describe "new without an explicitly given id" do
    test "generates a blank node as id" do
      assert %Compound{} = compound = Compound.new(triples())
      assert %BlankNode{} = id = Compound.id(compound)
      assert Compound.reset_id(flat_compound(), id) == compound

      description = RDF.description(EX.S, init: [{EX.P1, EX.O1}, {EX.P2, EX.O2}])

      assert %Compound{} = compound = Compound.new(description)
      assert %BlankNode{} = id = Compound.id(compound)

      assert compound ==
               %Compound{
                 asserted:
                   graph(
                     nil,
                     [
                       RDF.triple({EX.S, EX.P1, EX.O1}),
                       RDF.triple({EX.S, EX.P2, EX.O2})
                     ]
                   ),
                 annotations: RDF.description(id)
               }

      assert %Compound{} = compound = triples() |> RDF.graph() |> Compound.new()
      assert %BlankNode{} = id = Compound.id(compound)
      assert Compound.reset_id(flat_compound(), id) == compound
    end

    test "constructs a nested compound" do
      assert %Compound{} = compound = Compound.new(triples(), sub_compounds: sub_compound())
      assert %BlankNode{} = id = Compound.id(compound)
      assert Compound.reset_id(nested_compound(), id) == compound
    end

    test "constructs a nested compound from a nested list of triples" do
      nested_triples = [{EX.S2, EX.P2, EX.O2}]
      triples = [{EX.S1, EX.P1, EX.O1}, nested_triples, {EX.S3, EX.P3, EX.O3}]

      assert %Compound{} = compound = Compound.new(triples, EX.Compound)
      assert [%Compound{} = sub_compound] = Map.values(compound.sub_compounds)
      assert %BlankNode{} = id = Compound.id(sub_compound)
      assert sub_compound == Compound.new(nested_triples, id)

      assert %Compound{} = compound = Compound.new(triples)
      assert %BlankNode{} = Compound.id(compound)
      assert [%Compound{} = sub_compound] = Map.values(compound.sub_compounds)
      assert %BlankNode{} = id = Compound.id(sub_compound)
      assert sub_compound == Compound.new(nested_triples, id)
    end

    test "assertion mode of sub-compounds" do
      triples = [{EX.S1, EX.P1, EX.O1}, other_triples()]

      assert %Compound{} =
               compound = Compound.new(triples, EX.Compound, assertion_mode: :unasserted)

      assert [%Compound{} = sub_compound] = Map.values(compound.sub_compounds)
      assert %BlankNode{} = id = Compound.id(sub_compound)
      assert sub_compound == Compound.new(other_triples(), id, assertion_mode: :unasserted)

      assert %Compound{} = compound = Compound.new([], EX.Compound, unasserted: triples)

      assert [%Compound{} = sub_compound] = Map.values(compound.sub_compounds)
      assert %BlankNode{} = id = Compound.id(sub_compound)
      assert sub_compound == Compound.new(other_triples(), id, assertion_mode: :unasserted)

      assert %Compound{} = compound = Compound.new([], EX.Compound, asserted: triples)

      assert [%Compound{} = sub_compound] = Map.values(compound.sub_compounds)
      assert %BlankNode{} = id = Compound.id(sub_compound)
      assert sub_compound == Compound.new(other_triples(), id, assertion_mode: :asserted)
    end

    test "graph opts" do
      opts = [
        name: EX.Graph,
        base_iri: "http://base_iri/",
        prefixes: [ex: EX],
        init: {EX.Ignored, EX.p(), "init"}
      ]

      assert %Compound{} = compound = Compound.new(triples(), opts)

      assert compound == %Compound{
               asserted: Graph.new(triples(), opts),
               annotations: RDF.description(Compound.id(compound))
             }
    end
  end

  test "reset_id/2" do
    assert %Compound{} = compound = Compound.reset_id(flat_compound(), EX.new_id())
    assert Compound.id(compound) == EX.new_id()
    assert compound.asserted.name == EX.new_id()

    assert %Compound{} = compound = Compound.reset_id(flat_compound(), ~B"new_id")
    assert Compound.id(compound) == ~B"new_id"
    assert compound.asserted.name == nil
  end

  describe "from_rdf/2" do
    test "when the compound is not present" do
      assert Compound.from_rdf(RDF.graph(), EX.Compound) == empty_compound()
    end

    test "retrieves a compound from a graph when it's annotated via rtc:elements" do
      graph =
        RDF.graph()
        |> Graph.add(triples())
        |> Graph.add(EX.Compound |> RTC.elements(triples()))

      assert Compound.from_rdf(graph, EX.Compound) ==
               Compound.new(triples(), EX.Compound)
    end

    test "retrieves a compound from a graph when it's annotated via rtc:elementOf" do
      graph =
        RDF.graph()
        |> Graph.add(triples(), add_annotations: {RTC.elementOf(), RDF.iri(EX.Compound)})

      assert Compound.from_rdf(graph, EX.Compound) ==
               Compound.new(triples(), EX.Compound)
    end

    test "retrieves a compound from a graph when it's annotated via rtc:elements and rtc:elementOf" do
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

    test "retrieves a nested compound (via rtc:elements)" do
      graph =
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add(triples())
        |> Graph.add(EX.Compound |> RTC.elements(triples()))
        |> Graph.add(other_triples())
        |> Graph.add(EX.SubCompound |> RTC.elements(other_triples()))
        |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})

      assert Compound.from_rdf(graph, EX.Compound) == nested_compound()
    end

    test "retrieves a nested compound (via rtc:elementOf)" do
      graph =
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
        |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
        |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})

      assert Compound.from_rdf(graph, EX.Compound) == nested_compound()
    end

    test "retrieves a deeply nested compound (via rtc:elements)" do
      graph =
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add(triples())
        |> Graph.add(EX.Compound |> RTC.elements(triples()))
        |> Graph.add(other_triples())
        |> Graph.add(EX.SubCompound |> RTC.elements(other_triples()))
        |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
        |> Graph.add(more_triples())
        |> Graph.add(EX.DeepCompound |> RTC.elements(more_triples()))
        |> Graph.add({EX.DeepCompound, RTC.subCompoundOf(), EX.SubCompound})

      assert Compound.from_rdf(graph, EX.Compound) == deeply_nested_compound()
    end

    test "retrieves a deeply nested compound (via rtc:elementOf)" do
      graph =
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
        |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
        |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
        |> Graph.add(more_triples(), add_annotations: {RTC.elementOf(), EX.DeepCompound})
        |> Graph.add({EX.DeepCompound, RTC.subCompoundOf(), EX.SubCompound})

      assert Compound.from_rdf(graph, EX.Compound) == deeply_nested_compound()
    end

    test "retrieves annotations" do
      graph =
        RDF.graph()
        |> Graph.add(triples())
        |> Graph.add(EX.Compound |> RTC.elements(triples()) |> EX.foo(EX.Bar))

      assert Compound.from_rdf(graph, EX.Compound) ==
               Compound.new(triples(), EX.Compound, annotations: {EX.foo(), EX.Bar})

      graph =
        RDF.graph()
        |> Graph.add(triples(), add_annotations: {RTC.elementOf(), RDF.iri(EX.Compound)})
        |> Graph.add(EX.Compound |> EX.foo(EX.Bar))

      assert Compound.from_rdf(graph, EX.Compound) ==
               Compound.new(triples(), EX.Compound, annotations: {EX.foo(), EX.Bar})

      graph =
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
        |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
        |> Graph.add(EX.Compound |> EX.foo1(EX.Bar1))
        |> Graph.add(EX.SubCompound |> RTC.subCompoundOf(EX.Compound) |> EX.foo2(EX.Bar2))

      assert Compound.from_rdf(graph, EX.Compound) ==
               Compound.new(triples(), EX.Compound,
                 sub_compounds: Compound.add_annotations(sub_compound(), {EX.foo2(), EX.Bar2}),
                 annotations: {EX.foo1(), EX.Bar1}
               )
    end

    test "annotations from super-compounds" do
      graph =
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
        |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
        |> Graph.add(EX.Compound |> EX.foo1(EX.Bar1))
        |> Graph.add(EX.SubCompound |> RTC.subCompoundOf(EX.Compound) |> EX.foo2(EX.Bar2))

      assert Compound.from_rdf(graph, EX.SubCompound) ==
               Compound.new(other_triples(), EX.SubCompound,
                 annotations: {EX.foo2(), EX.Bar2},
                 super_compounds: Description.new(EX.Compound, init: {EX.foo1(), EX.Bar1})
               )

      graph =
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add(triples())
        |> Graph.add(other_triples())
        |> Graph.add(EX.Compound |> RTC.elements(triples()) |> EX.foo1(EX.Bar1))
        |> Graph.add(
          EX.SubCompound
          |> RTC.subCompoundOf(EX.Compound)
          |> RTC.elements(other_triples())
          |> EX.foo2(EX.Bar2)
        )

      assert Compound.from_rdf(graph, EX.SubCompound) ==
               Compound.new(other_triples(), EX.SubCompound,
                 annotations: {EX.foo2(), EX.Bar2},
                 super_compounds: Description.new(EX.Compound, init: {EX.foo1(), EX.Bar1})
               )
    end

    test "annotations from different super-compounds" do
      graph =
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add({EX.S, EX.P, EX.O})
        |> Graph.add(triples())
        |> Graph.add(other_triples())
        |> Graph.add(EX.Compound |> RTC.elements(triples()) |> EX.foo1(EX.Bar1))
        |> Graph.add(
          EX.OtherSuperCompound
          |> RTC.elements({EX.S, EX.P, EX.O})
          |> RTC.subCompoundOf([EX.SuperSuperCompound])
          |> EX.foo2(EX.Bar2)
        )
        |> Graph.add(
          EX.SuperSuperCompound
          |> RTC.elements({EX.S, EX.P, EX.O})
          |> EX.foo22(EX.Bar22)
        )
        |> Graph.add(
          EX.SubCompound
          |> RTC.subCompoundOf([EX.Compound, EX.OtherSuperCompound])
          |> RTC.elements(other_triples())
          |> EX.foo3(EX.Bar3)
        )

      assert Compound.from_rdf(graph, EX.SubCompound) ==
               Compound.new(other_triples(), EX.SubCompound,
                 annotations: {EX.foo3(), EX.Bar3},
                 super_compounds: [
                   Description.new(EX.Compound, init: {EX.foo1(), EX.Bar1}),
                   Description.new(EX.OtherSuperCompound,
                     init: [{EX.foo2(), EX.Bar2}, {EX.foo22(), EX.Bar22}]
                   )
                 ]
               )
    end

    test "annotations in sub-compounds" do
      graph =
        RDF.graph(name: RDF.iri(EX.Compound))
        |> Graph.add({EX.S, EX.P, EX.O})
        |> Graph.add(triples())
        |> Graph.add(other_triples())
        |> Graph.add(
          EX.Compound
          |> RTC.elements(triples())
          |> EX.foo1(EX.Bar1)
        )
        |> Graph.add(
          EX.SubCompound
          |> RTC.subCompoundOf([EX.Compound])
          |> RTC.elements({EX.S, EX.P, EX.O})
          |> EX.foo21(EX.Bar21)
        )
        |> Graph.add(
          EX.OtherSuperCompound
          |> RTC.elements({EX.S, EX.P, EX.O})
          |> EX.foo22(EX.Bar22)
        )
        |> Graph.add(
          EX.SubSubCompound
          |> RTC.subCompoundOf([EX.SubCompound, EX.OtherSuperCompound])
          |> RTC.elements(other_triples())
          |> EX.foo3(EX.Bar3)
        )

      assert Compound.from_rdf(graph, EX.Compound) ==
               Compound.new(triples(), EX.Compound,
                 annotations: {EX.foo1(), EX.Bar1},
                 sub_compounds:
                   Compound.new({EX.S, EX.P, EX.O}, EX.SubCompound,
                     annotations: {EX.foo21(), EX.Bar21},
                     sub_compounds:
                       Compound.new(other_triples(), EX.SubSubCompound,
                         annotations: {EX.foo3(), EX.Bar3},
                         super_compounds:
                           Compound.new({EX.S, EX.P, EX.O}, EX.OtherSuperCompound,
                             annotations: {EX.foo22(), EX.Bar22}
                           )
                       )
                   )
               )
    end

    test "cyclic sub-compounds raise an error" do
      assert_raise RuntimeError, "circle in sub-compound #{RDF.iri(EX.Compound)}", fn ->
        RDF.graph()
        |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
        |> Graph.add({EX.Compound, RTC.subCompoundOf(), EX.Compound})
        |> Compound.from_rdf(EX.Compound)
      end

      assert_raise RuntimeError, "circle in sub-compound #{RDF.iri(EX.SubCompound)}", fn ->
        RDF.graph()
        |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
        |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
        |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
        |> Graph.add({EX.Compound, RTC.subCompoundOf(), EX.SubCompound})
        |> Compound.from_rdf(EX.Compound)
      end
    end

    test "unasserted statements" do
      assert graph()
             |> Graph.add_annotations(triples(), {RTC.elementOf(), RDF.iri(EX.Compound)})
             |> Compound.from_rdf(EX.Compound) ==
               unasserted_flat_compound()

      assert graph()
             |> Graph.add({EX.S1, EX.P1, EX.O1},
               add_annotations: {RTC.elementOf(), RDF.iri(EX.Compound)}
             )
             |> Graph.add_annotations(
               {EX.S2, EX.P2, EX.O2},
               {RTC.elementOf(), RDF.iri(EX.Compound)}
             )
             |> Compound.from_rdf(EX.Compound) ==
               mixed_asserted_flat_compound()

      assert graph()
             |> Graph.add_annotations(triples(), {RTC.elementOf(), RDF.iri(EX.Compound)})
             |> Graph.add_annotations(
               other_triples(),
               {RTC.elementOf(), RDF.iri(EX.SubCompound)}
             )
             |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
             |> Compound.from_rdf(EX.Compound) ==
               unasserted_nested_compound()
    end
  end

  describe "to_rdf" do
    test "uses the graph defaults provided with new/3" do
      opts = [
        name: EX.Graph,
        base_iri: "http://base_iri/",
        prefixes: [ex: EX]
      ]

      compound = Compound.new(triples(), EX.Compound, opts)

      assert Compound.to_rdf(compound) ==
               RDF.graph(opts)
               |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
    end

    test "overwrites the graph defaults provided with new/3" do
      opts = [
        name: EX.Overwritten,
        base_iri: "http://base_iri/overwritten",
        prefixes: [owl: RDF.NS.OWL]
      ]

      compound = Compound.new(triples(), EX.Compound, opts)

      assert Compound.to_rdf(compound,
               name: EX.Graph,
               base_iri: "http://base_iri/",
               prefixes: [ex: EX]
             ) ==
               RDF.graph(
                 name: EX.Graph,
                 base_iri: "http://base_iri/",
                 prefixes: [ex: EX, owl: RDF.NS.OWL]
               )
               |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
    end
  end

  describe "to_rdf with :element_of style" do
    test "returns a graph with the triples and the RTC annotations with rtc:elementOf" do
      assert Compound.to_rdf(flat_compound(), element_style: :element_of) ==
               Graph.add(graph(), triples(),
                 add_annotations: {RTC.elementOf(), RDF.iri(EX.Compound)}
               )
    end

    test "with sub-compound" do
      assert Compound.to_rdf(nested_compound(), element_style: :element_of) ==
               graph()
               |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
               |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
               |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
    end

    test "with super-compounds" do
      assert Compound.new(triples(), EX.Compound,
               annotations: {EX.ap1(), EX.AO1},
               super_compounds:
                 Compound.new([], EX.SuperCompound, annotations: {EX.ap0(), EX.AO0}),
               sub_compounds:
                 Compound.new(other_triples(), EX.SubCompound,
                   annotations: {EX.ap2(), EX.AO2},
                   super_compounds:
                     Compound.new([], EX.OtherSuperCompound, annotations: {EX.ap3(), EX.AO3})
                 )
             )
             |> Compound.to_rdf(element_style: :element_of) ==
               graph()
               |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
               |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
               |> Graph.add(EX.Compound |> RTC.subCompoundOf(EX.SuperCompound) |> EX.ap1(EX.AO1))
               |> Graph.add(
                 EX.SubCompound
                 |> RTC.subCompoundOf([EX.Compound, EX.OtherSuperCompound])
                 |> EX.ap2(EX.AO2)
               )
    end

    test "the empty compound" do
      assert Compound.to_rdf(empty_compound(), element_style: :element_of) ==
               graph()
    end

    test "a compound with a duplicate triples in a sub-compound" do
      assert Compound.to_rdf(compound_with_duplicate_triple_in_sub_compound(),
               element_style: :element_of
             ) ==
               graph()
               |> Graph.add([{EX.S3, EX.P3, EX.O3} | triples()],
                 add_annotations: {RTC.elementOf(), EX.Compound}
               )
               |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
               |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
    end

    test "annotations are included" do
      assert Compound.new(triples(), EX.Compound,
               sub_compounds: Compound.add_annotations(sub_compound(), {EX.foo2(), EX.Bar2}),
               annotations: {EX.foo1(), EX.Bar1}
             )
             |> Compound.to_rdf(element_style: :element_of) ==
               graph()
               |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
               |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
               |> Graph.add(EX.Compound |> EX.foo1(EX.Bar1))
               |> Graph.add(EX.SubCompound |> RTC.subCompoundOf(EX.Compound) |> EX.foo2(EX.Bar2))
    end

    test "setting RDF.Graph.new/1 opts" do
      assert Compound.to_rdf(flat_compound(),
               element_style: :element_of,
               name: EX.Graph,
               prefixes: [ex: EX]
             ) ==
               graph(EX.Graph)
               |> Graph.add_prefixes(ex: EX)
               |> Graph.add(triples(), add_annotations: {RTC.elementOf(), RDF.iri(EX.Compound)})
    end

    test "unasserted triples" do
      assert Compound.to_rdf(unasserted_flat_compound(), element_style: :element_of) ==
               Graph.add_annotations(graph(), triples(), {RTC.elementOf(), RDF.iri(EX.Compound)})

      assert Compound.to_rdf(mixed_asserted_flat_compound(), element_style: :element_of) ==
               graph()
               |> Graph.add({EX.S1, EX.P1, EX.O1},
                 add_annotations: {RTC.elementOf(), RDF.iri(EX.Compound)}
               )
               |> Graph.add_annotations(
                 {EX.S2, EX.P2, EX.O2},
                 {RTC.elementOf(), RDF.iri(EX.Compound)}
               )

      assert Compound.to_rdf(unasserted_nested_compound(), element_style: :element_of) ==
               graph()
               |> Graph.add_annotations(triples(), {RTC.elementOf(), RDF.iri(EX.Compound)})
               |> Graph.add_annotations(
                 other_triples(),
                 {RTC.elementOf(), RDF.iri(EX.SubCompound)}
               )
               |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
    end
  end

  describe "to_rdf with :elements style" do
    test "returns a graph with the triples and the RTC annotations with rtc:elements" do
      assert Compound.to_rdf(flat_compound(), element_style: :elements) ==
               graph()
               |> Graph.add(triples())
               |> Graph.add(EX.Compound |> RTC.elements(triples()))
    end

    test "with sub-compound" do
      assert Compound.to_rdf(nested_compound(), element_style: :elements) ==
               graph()
               |> Graph.add(triples())
               |> Graph.add(EX.Compound |> RTC.elements(triples()))
               |> Graph.add(other_triples())
               |> Graph.add(
                 EX.SubCompound
                 |> RTC.elements(other_triples())
                 |> RTC.subCompoundOf(EX.Compound)
               )
    end

    test "the empty compound" do
      assert Compound.to_rdf(empty_compound(), element_style: :elements) ==
               graph()
    end

    test "a compound with a duplicate triples in a sub-compound" do
      triples = [{EX.S3, EX.P3, EX.O3} | triples()]

      assert Compound.to_rdf(compound_with_duplicate_triple_in_sub_compound(),
               element_style: :elements
             ) ==
               graph()
               |> Graph.add(triples)
               |> Graph.add(EX.Compound |> RTC.elements(triples))
               |> Graph.add(other_triples())
               |> Graph.add(
                 EX.SubCompound
                 |> RTC.elements(other_triples())
                 |> RTC.subCompoundOf(EX.Compound)
               )
    end

    test "annotations are included" do
      assert Compound.new(triples(), EX.Compound,
               sub_compounds: Compound.add_annotations(sub_compound(), {EX.foo2(), EX.Bar2}),
               annotations: {EX.foo1(), EX.Bar1}
             )
             |> Compound.to_rdf(element_style: :elements) ==
               graph()
               |> Graph.add(triples())
               |> Graph.add(EX.Compound |> RTC.elements(triples()) |> EX.foo1(EX.Bar1))
               |> Graph.add(other_triples())
               |> Graph.add(
                 EX.SubCompound
                 |> RTC.elements(other_triples())
                 |> RTC.subCompoundOf(EX.Compound)
                 |> EX.foo2(EX.Bar2)
               )
    end

    test "setting RDF.Graph.new/1 opts" do
      assert Compound.to_rdf(flat_compound(),
               element_style: :elements,
               name: EX.Graph,
               prefixes: [ex: EX]
             ) ==
               graph(EX.Graph)
               |> Graph.add_prefixes(ex: EX)
               |> Graph.add(triples())
               |> Graph.add(EX.Compound |> RTC.elements(triples()))
    end

    test "unasserted triples" do
      assert Compound.to_rdf(unasserted_flat_compound(), element_style: :elements) ==
               Graph.add(graph(), EX.Compound |> RTC.elements(triples()))

      assert Compound.to_rdf(mixed_asserted_flat_compound(), element_style: :elements) ==
               graph()
               |> Graph.add({EX.S1, EX.P1, EX.O1})
               |> Graph.add(EX.Compound |> RTC.elements({EX.S1, EX.P1, EX.O1}))
               |> Graph.add(EX.Compound |> RTC.elements({EX.S2, EX.P2, EX.O2}))

      assert Compound.to_rdf(unasserted_nested_compound(), element_style: :elements) ==
               graph()
               |> Graph.add(EX.Compound |> RTC.elements(triples()))
               |> Graph.add(EX.SubCompound |> RTC.elements(other_triples()))
               |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
    end
  end

  describe "asserted_graph/1" do
    test "the empty compound" do
      assert Compound.asserted_graph(empty_compound()) == graph()
    end

    test "a flat compound" do
      assert Compound.asserted_graph(flat_compound()) == graph(triples())
    end

    test "a nested compound" do
      assert Compound.asserted_graph(nested_compound()) ==
               graph(triples() ++ other_triples())
    end

    test "compound with blank node id" do
      bnode = RDF.bnode("compound")
      assert Compound.new([], bnode) |> Compound.asserted_graph() == graph(nil)
    end

    test "uses the graph defaults provided with new/3" do
      opts = [
        name: EX.Graph,
        base_iri: "http://base_iri/",
        prefixes: [ex: EX]
      ]

      compound = Compound.new(triples(), EX.Compound, opts)

      assert Compound.asserted_graph(compound) ==
               RDF.graph(triples(), opts)
    end

    test "overwrites the graph defaults provided with new/3" do
      opts = [
        name: EX.Overwritten,
        base_iri: "http://base_iri/overwritten",
        prefixes: [owl: RDF.NS.OWL]
      ]

      compound = Compound.new(triples(), EX.Compound, opts)

      assert Compound.asserted_graph(compound,
               name: EX.Graph,
               base_iri: "http://base_iri/",
               prefixes: [ex: EX]
             ) ==
               RDF.graph(
                 triples(),
                 name: EX.Graph,
                 base_iri: "http://base_iri/",
                 prefixes: [ex: EX, owl: RDF.NS.OWL]
               )
    end
  end

  describe "unasserted_graph/1" do
    test "the empty compound" do
      assert Compound.unasserted_graph(empty_compound()) == graph()
    end

    test "a flat compound" do
      assert Compound.unasserted_graph(unasserted_flat_compound()) == graph(triples())
    end

    test "a nested compound" do
      assert Compound.unasserted_graph(unasserted_nested_compound()) ==
               graph(triples() ++ other_triples())
    end

    test "compound with blank node id" do
      bnode = RDF.bnode("compound")
      assert Compound.new([], bnode) |> Compound.unasserted_graph() == graph(nil)
    end

    test "uses the graph defaults provided with new/3" do
      opts = [
        name: EX.Graph,
        base_iri: "http://base_iri/",
        prefixes: [ex: EX],
        assertion_mode: :unasserted
      ]

      compound = Compound.new(triples(), EX.Compound, opts)

      assert Compound.unasserted_graph(compound) ==
               RDF.graph(triples(), opts)
    end

    test "overwrites the graph defaults provided with new/3" do
      opts = [
        name: EX.Overwritten,
        base_iri: "http://base_iri/overwritten",
        prefixes: [owl: RDF.NS.OWL],
        assertion_mode: :unasserted
      ]

      compound = Compound.new(triples(), EX.Compound, opts)

      assert Compound.unasserted_graph(compound,
               name: EX.Graph,
               base_iri: "http://base_iri/",
               prefixes: [ex: EX]
             ) ==
               RDF.graph(
                 triples(),
                 name: EX.Graph,
                 base_iri: "http://base_iri/",
                 prefixes: [ex: EX, owl: RDF.NS.OWL]
               )
    end
  end

  describe "graph/1" do
    test "assertion_mode :all (default)" do
      assert Compound.graph(flat_compound()) == graph(triples())
      assert Compound.graph(unasserted_flat_compound()) == graph(triples())
      assert Compound.graph(mixed_asserted_flat_compound()) == graph(triples())

      assert Compound.graph(nested_compound()) ==
               graph(triples() ++ other_triples())

      assert Compound.graph(unasserted_nested_compound()) ==
               graph(triples() ++ other_triples())
    end

    test "assertion_mode :asserted" do
      assert Compound.graph(flat_compound(), assertion_mode: :asserted) == graph(triples())

      assert Compound.graph(unasserted_flat_compound(), assertion_mode: :asserted) ==
               graph()

      assert Compound.graph(mixed_asserted_flat_compound(), assertion_mode: :asserted) ==
               graph({EX.S1, EX.P1, EX.O1})

      assert Compound.graph(nested_compound(), assertion_mode: :asserted) ==
               graph(triples() ++ other_triples())

      assert Compound.graph(unasserted_nested_compound(), assertion_mode: :asserted) ==
               graph()
    end

    test "assertion_mode :unasserted" do
      assert Compound.graph(flat_compound(), assertion_mode: :unasserted) == graph()

      assert Compound.graph(unasserted_flat_compound(), assertion_mode: :unasserted) ==
               graph(triples())

      assert Compound.graph(mixed_asserted_flat_compound(), assertion_mode: :unasserted) ==
               graph({EX.S2, EX.P2, EX.O2})

      assert Compound.graph(nested_compound(), assertion_mode: :unasserted) ==
               graph()

      assert Compound.graph(unasserted_nested_compound(), assertion_mode: :unasserted) ==
               graph(triples() ++ other_triples())
    end

    test "compound with blank node id" do
      bnode = RDF.bnode("compound")
      assert Compound.new([], bnode) |> Compound.graph() == graph(nil)
    end

    test "uses the graph defaults provided with new/3" do
      opts = [
        name: EX.Graph,
        base_iri: "http://base_iri/",
        prefixes: [ex: EX]
      ]

      compound = Compound.new(triples(), EX.Compound, opts)

      assert Compound.graph(compound) ==
               RDF.graph(triples(), opts)
    end

    test "overwrites the graph defaults provided with new/3" do
      opts = [
        name: EX.Overwritten,
        base_iri: "http://base_iri/overwritten",
        prefixes: [owl: RDF.NS.OWL]
      ]

      compound = Compound.new(triples(), EX.Compound, opts)

      assert Compound.graph(compound,
               name: EX.Graph,
               base_iri: "http://base_iri/",
               prefixes: [ex: EX]
             ) ==
               RDF.graph(
                 triples(),
                 name: EX.Graph,
                 base_iri: "http://base_iri/",
                 prefixes: [ex: EX, owl: RDF.NS.OWL]
               )
    end
  end

  describe "triples/1" do
    test "returns the triple elements as a list" do
      assert Compound.triples(flat_compound()) == Enum.map(triples(), &RDF.triple/1)

      assert Compound.triples(flat_compound(), assertion_mode: :asserted) ==
               Enum.map(triples(), &RDF.triple/1)

      assert Compound.triples(flat_compound(), assertion_mode: :unasserted) == []

      assert Compound.triples(unasserted_flat_compound(), assertion_mode: :unasserted) ==
               Enum.map(triples(), &RDF.triple/1)
    end

    test "includes the triples of nested compounds" do
      assert Compound.triples(nested_compound()) ==
               Enum.map(triples() ++ other_triples(), &RDF.triple/1)
    end

    test "the empty compound" do
      assert Compound.triples(empty_compound()) == []
    end

    test "a compound with duplicate triple elements in a sub-compound" do
      assert Compound.triples(compound_with_duplicate_triple_in_sub_compound()) ==
               Enum.map(triples() ++ other_triples(), &RDF.triple/1)
    end
  end

  test "get/3" do
    s1_description_1t = Description.new(EX.S1, init: {EX.P1, EX.O1})
    s1_description_2t = Description.new(EX.S1, init: [{EX.P1, [EX.O1, EX.O2]}])

    assert Compound.get(flat_compound(), EX.S1) == s1_description_1t

    assert Compound.get(flat_compound(), EX.S1, nil, assertion_mode: :asserted) ==
             s1_description_1t

    assert Compound.get(flat_compound(), EX.S1, nil, assertion_mode: :unasserted) ==
             nil

    assert Compound.get(flat_compound(), EX.S1, :foo, assertion_mode: :unasserted) ==
             :foo

    assert Compound.get(unasserted_flat_compound(), EX.S1) == s1_description_1t

    assert Compound.get(unasserted_flat_compound(), EX.S1, nil, assertion_mode: :unasserted) ==
             s1_description_1t

    assert Compound.get(unasserted_flat_compound(), EX.S1, nil, assertion_mode: :asserted) ==
             nil

    assert Compound.get(mixed_asserted_flat_compound(), EX.S1) == s1_description_1t

    mixed_asserted_flat_compound =
      mixed_asserted_flat_compound(
        [{EX.S1, EX.P1, EX.O1}],
        [{EX.S1, EX.P1, EX.O2}]
      )

    assert Compound.get(mixed_asserted_flat_compound, EX.S1) == s1_description_2t

    assert Compound.get(mixed_asserted_flat_compound, EX.S1, nil, assertion_mode: :asserted) ==
             s1_description_1t

    assert Compound.get(mixed_asserted_flat_compound, EX.S1, nil, assertion_mode: :unasserted) ==
             Description.new(EX.S1, init: {EX.P1, EX.O2})

    nested_compound = nested_compound(triples(), sub_compound({EX.S1, EX.P2, EX.O2}))

    assert Compound.get(nested_compound, EX.S1) ==
             Description.new(EX.S1, init: [{EX.P1, EX.O1}, {EX.P2, EX.O2}])

    assert Compound.get(nested_compound, EX.S1, nil, assertion_mode: :asserted) ==
             Description.new(EX.S1, init: [{EX.P1, EX.O1}, {EX.P2, EX.O2}])

    assert Compound.get(nested_compound, EX.S1, nil, assertion_mode: :unasserted) ==
             nil
  end

  describe "empty?/1" do
    test "with empty compound" do
      assert Compound.empty?(empty_compound()) == true
    end

    test "with a non-empty compound" do
      assert Compound.empty?(flat_compound()) == false
      assert Compound.empty?(nested_compound()) == false
    end

    test "with an empty compound, that has non-empty sub-compounds" do
      assert Compound.empty?(Compound.new([], EX.Compound, sub_compounds: sub_compound())) ==
               false
    end

    test "assertion_mode" do
      assert Compound.empty?(flat_compound(), assertion_mode: :unasserted) == true
      assert Compound.empty?(unasserted_flat_compound(), assertion_mode: :asserted) == true
      assert Compound.empty?(mixed_asserted_flat_compound(), assertion_mode: :asserted) == false
      assert Compound.empty?(mixed_asserted_flat_compound(), assertion_mode: :unasserted) == false

      assert Compound.empty?(Compound.new([], EX.Compound, sub_compounds: sub_compound()),
               assertion_mode: :unasserted
             ) ==
               true
    end
  end

  describe "include?/2" do
    test "returns whether the triple is an element of the compound" do
      Enum.each(triples(), fn triple ->
        assert Compound.include?(flat_compound(), triple) == true
      end)

      assert Compound.include?(flat_compound(), triples()) == true

      assert Compound.include?(flat_compound(), {EX.S2, EX.P2, EX.O3}) == false
    end

    test "takes nested compound into account" do
      Enum.each(triples() ++ other_triples(), fn triple ->
        assert Compound.include?(nested_compound(), triple) == true
      end)

      assert Compound.include?(nested_compound(), triples() ++ other_triples()) == true

      assert Compound.include?(nested_compound(), {EX.S2, EX.P2, EX.O3}) == false
    end

    test "assertion_mode" do
      assert Compound.include?(flat_compound(), triples(), assertion_mode: :asserted)
      refute Compound.include?(flat_compound(), triples(), assertion_mode: :unasserted)

      assert Compound.include?(unasserted_flat_compound(), triples())
      assert Compound.include?(unasserted_flat_compound(), triples(), assertion_mode: :unasserted)
      refute Compound.include?(unasserted_flat_compound(), triples(), assertion_mode: :asserted)

      assert Compound.include?(mixed_asserted_flat_compound(), triples())

      assert Compound.include?(mixed_asserted_flat_compound(), {EX.S1, EX.P1, EX.O1},
               assertion_mode: :asserted
             )

      refute Compound.include?(mixed_asserted_flat_compound(), {EX.S2, EX.P2, EX.O2},
               assertion_mode: :asserted
             )

      refute Compound.include?(mixed_asserted_flat_compound(), {EX.S1, EX.P1, EX.O1},
               assertion_mode: :unasserted
             )

      assert Compound.include?(mixed_asserted_flat_compound(), {EX.S2, EX.P2, EX.O2},
               assertion_mode: :unasserted
             )

      assert Compound.include?(nested_compound(), triples() ++ other_triples())

      assert Compound.include?(nested_compound(), triples() ++ other_triples(),
               assertion_mode: :asserted
             )

      refute Compound.include?(nested_compound(), triples(), assertion_mode: :unasserted)
      refute Compound.include?(nested_compound(), other_triples(), assertion_mode: :unasserted)
    end
  end

  test "describes?/3" do
    assert Compound.describes?(flat_compound(), EX.S1)
    assert Compound.describes?(flat_compound(), EX.S2)
    assert Compound.describes?(flat_compound(), EX.S1, assertion_mode: :asserted)
    refute Compound.describes?(flat_compound(), EX.S1, assertion_mode: :unasserted)

    assert Compound.describes?(unasserted_flat_compound(), EX.S1)
    assert Compound.describes?(unasserted_flat_compound(), EX.S1, assertion_mode: :unasserted)
    refute Compound.describes?(unasserted_flat_compound(), EX.S1, assertion_mode: :asserted)

    assert Compound.describes?(mixed_asserted_flat_compound(), EX.S1)
    assert Compound.describes?(mixed_asserted_flat_compound(), EX.S2)
    assert Compound.describes?(mixed_asserted_flat_compound(), EX.S1, assertion_mode: :asserted)
    refute Compound.describes?(mixed_asserted_flat_compound(), EX.S2, assertion_mode: :asserted)
    refute Compound.describes?(mixed_asserted_flat_compound(), EX.S1, assertion_mode: :unasserted)
    assert Compound.describes?(mixed_asserted_flat_compound(), EX.S2, assertion_mode: :unasserted)

    assert Compound.describes?(nested_compound(), EX.S1)
    assert Compound.describes?(nested_compound(), EX.S4)
    assert Compound.describes?(nested_compound(), EX.S1, assertion_mode: :asserted)
    assert Compound.describes?(nested_compound(), EX.S3, assertion_mode: :asserted)
    refute Compound.describes?(nested_compound(), EX.S1, assertion_mode: :unasserted)
    refute Compound.describes?(nested_compound(), EX.S3, assertion_mode: :unasserted)
    refute Compound.describes?(nested_compound(), EX.S4, assertion_mode: :unasserted)
  end

  describe "triple_count/1" do
    test "the empty compound" do
      assert Compound.triple_count(empty_compound()) == 0
    end

    test "a flat compound" do
      assert Compound.triple_count(flat_compound()) == 2
    end

    test "a nested compound" do
      assert Compound.triple_count(nested_compound()) == 4
    end

    test "a compound with duplicate triple elements in a sub-compound" do
      assert Compound.triple_count(compound_with_duplicate_triple_in_sub_compound()) == 4
    end

    test "assertion_mode" do
      assert Compound.triple_count(flat_compound(), assertion_mode: :unasserted) == 0
      assert Compound.triple_count(unasserted_flat_compound(), assertion_mode: :asserted) == 0
      assert Compound.triple_count(mixed_asserted_flat_compound(), assertion_mode: :asserted) == 1

      assert Compound.triple_count(mixed_asserted_flat_compound(), assertion_mode: :unasserted) ==
               1

      assert Compound.triple_count(Compound.new([], EX.Compound, sub_compounds: sub_compound()),
               assertion_mode: :unasserted
             ) == 0
    end
  end

  test "subjects/2" do
    assert Compound.subjects(flat_compound()) ==
             MapSet.new([RDF.iri(EX.S1), RDF.iri(EX.S2)])

    assert Compound.subjects(flat_compound(), assertion_mode: :asserted) ==
             MapSet.new([RDF.iri(EX.S1), RDF.iri(EX.S2)])

    assert Compound.subjects(flat_compound(), assertion_mode: :unasserted) ==
             MapSet.new()

    assert Compound.subjects(unasserted_flat_compound()) ==
             MapSet.new([RDF.iri(EX.S1), RDF.iri(EX.S2)])

    assert Compound.subjects(unasserted_flat_compound(), assertion_mode: :unasserted) ==
             MapSet.new([RDF.iri(EX.S1), RDF.iri(EX.S2)])

    assert Compound.subjects(unasserted_flat_compound(), assertion_mode: :asserted) ==
             MapSet.new()

    assert Compound.subjects(mixed_asserted_flat_compound()) ==
             MapSet.new([RDF.iri(EX.S1), RDF.iri(EX.S2)])

    assert Compound.subjects(mixed_asserted_flat_compound(), assertion_mode: :asserted) ==
             MapSet.new([RDF.iri(EX.S1)])

    assert Compound.subjects(mixed_asserted_flat_compound(), assertion_mode: :unasserted) ==
             MapSet.new([RDF.iri(EX.S2)])

    assert Compound.subjects(nested_compound()) ==
             MapSet.new([RDF.iri(EX.S1), RDF.iri(EX.S2), RDF.iri(EX.S3), RDF.iri(EX.S4)])

    assert Compound.subjects(nested_compound(), assertion_mode: :asserted) ==
             MapSet.new([RDF.iri(EX.S1), RDF.iri(EX.S2), RDF.iri(EX.S3), RDF.iri(EX.S4)])

    assert Compound.subjects(nested_compound(), assertion_mode: :unasserted) ==
             MapSet.new()
  end

  test "predicates/2" do
    assert Compound.predicates(flat_compound()) ==
             MapSet.new([RDF.iri(EX.P1), RDF.iri(EX.P2)])

    assert Compound.predicates(flat_compound(), assertion_mode: :asserted) ==
             MapSet.new([RDF.iri(EX.P1), RDF.iri(EX.P2)])

    assert Compound.predicates(flat_compound(), assertion_mode: :unasserted) ==
             MapSet.new()

    assert Compound.predicates(unasserted_flat_compound()) ==
             MapSet.new([RDF.iri(EX.P1), RDF.iri(EX.P2)])

    assert Compound.predicates(unasserted_flat_compound(), assertion_mode: :unasserted) ==
             MapSet.new([RDF.iri(EX.P1), RDF.iri(EX.P2)])

    assert Compound.predicates(unasserted_flat_compound(), assertion_mode: :asserted) ==
             MapSet.new()

    assert Compound.predicates(mixed_asserted_flat_compound()) ==
             MapSet.new([RDF.iri(EX.P1), RDF.iri(EX.P2)])

    assert Compound.predicates(mixed_asserted_flat_compound(), assertion_mode: :asserted) ==
             MapSet.new([RDF.iri(EX.P1)])

    assert Compound.predicates(mixed_asserted_flat_compound(), assertion_mode: :unasserted) ==
             MapSet.new([RDF.iri(EX.P2)])

    assert Compound.predicates(nested_compound()) ==
             MapSet.new([RDF.iri(EX.P1), RDF.iri(EX.P2), RDF.iri(EX.P3), RDF.iri(EX.P4)])

    assert Compound.predicates(nested_compound(), assertion_mode: :asserted) ==
             MapSet.new([RDF.iri(EX.P1), RDF.iri(EX.P2), RDF.iri(EX.P3), RDF.iri(EX.P4)])

    assert Compound.predicates(nested_compound(), assertion_mode: :unasserted) ==
             MapSet.new()
  end

  test "objects/2" do
    assert Compound.objects(flat_compound()) ==
             MapSet.new([RDF.iri(EX.O1), RDF.iri(EX.O2)])

    assert Compound.objects(flat_compound(), assertion_mode: :asserted) ==
             MapSet.new([RDF.iri(EX.O1), RDF.iri(EX.O2)])

    assert Compound.objects(flat_compound(), assertion_mode: :unasserted) ==
             MapSet.new()

    assert Compound.objects(unasserted_flat_compound()) ==
             MapSet.new([RDF.iri(EX.O1), RDF.iri(EX.O2)])

    assert Compound.objects(unasserted_flat_compound(), assertion_mode: :unasserted) ==
             MapSet.new([RDF.iri(EX.O1), RDF.iri(EX.O2)])

    assert Compound.objects(unasserted_flat_compound(), assertion_mode: :asserted) ==
             MapSet.new()

    assert Compound.objects(mixed_asserted_flat_compound()) ==
             MapSet.new([RDF.iri(EX.O1), RDF.iri(EX.O2)])

    assert Compound.objects(mixed_asserted_flat_compound(), assertion_mode: :asserted) ==
             MapSet.new([RDF.iri(EX.O1)])

    assert Compound.objects(mixed_asserted_flat_compound(), assertion_mode: :unasserted) ==
             MapSet.new([RDF.iri(EX.O2)])

    assert Compound.objects(nested_compound()) ==
             MapSet.new([RDF.iri(EX.O1), RDF.iri(EX.O2), RDF.iri(EX.O3), RDF.iri(EX.O4)])

    assert Compound.objects(nested_compound(), assertion_mode: :asserted) ==
             MapSet.new([RDF.iri(EX.O1), RDF.iri(EX.O2), RDF.iri(EX.O3), RDF.iri(EX.O4)])

    assert Compound.objects(nested_compound(), assertion_mode: :unasserted) ==
             MapSet.new()
  end

  test "resources/2" do
    all_resources =
      MapSet.new([
        RDF.iri(EX.S1),
        RDF.iri(EX.S2),
        RDF.iri(EX.P1),
        RDF.iri(EX.P2),
        RDF.iri(EX.O1),
        RDF.iri(EX.O2)
      ])

    assert Compound.resources(flat_compound()) == all_resources
    assert Compound.resources(flat_compound(), assertion_mode: :asserted) == all_resources

    assert Compound.resources(flat_compound(), assertion_mode: :unasserted) ==
             MapSet.new()

    assert Compound.resources(unasserted_flat_compound()) == all_resources

    assert Compound.resources(unasserted_flat_compound(), assertion_mode: :unasserted) ==
             all_resources

    assert Compound.resources(unasserted_flat_compound(), assertion_mode: :asserted) ==
             MapSet.new()

    assert Compound.resources(mixed_asserted_flat_compound()) ==
             all_resources

    assert Compound.resources(mixed_asserted_flat_compound(), assertion_mode: :asserted) ==
             MapSet.new([RDF.iri(EX.S1), RDF.iri(EX.P1), RDF.iri(EX.O1)])

    assert Compound.resources(mixed_asserted_flat_compound(), assertion_mode: :unasserted) ==
             MapSet.new([RDF.iri(EX.S2), RDF.iri(EX.P2), RDF.iri(EX.O2)])

    all_resources_with_nested =
      MapSet.union(
        all_resources,
        MapSet.new([
          RDF.iri(EX.S3),
          RDF.iri(EX.S4),
          RDF.iri(EX.P3),
          RDF.iri(EX.P4),
          RDF.iri(EX.O3),
          RDF.iri(EX.O4)
        ])
      )

    assert Compound.resources(nested_compound()) == all_resources_with_nested

    assert Compound.resources(nested_compound(), assertion_mode: :asserted) ==
             all_resources_with_nested

    assert Compound.resources(nested_compound(), assertion_mode: :unasserted) ==
             MapSet.new()
  end

  describe "add/2" do
    test "adding asserted triples with default assertion_mode" do
      # a single triple
      assert empty_compound()
             |> Compound.add({EX.S1, EX.P1, EX.O1})
             |> Compound.add({EX.S2, EX.P2, EX.O2}) == flat_compound()

      # RDF.ex data structures
      [
        EX.Foo |> EX.bar(42) |> EX.baz(EX.O),
        RDF.graph(EX.Foo |> EX.bar(42) |> EX.baz(EX.O))
      ]
      |> Enum.each(fn data ->
        assert Compound.add(flat_compound(), data) ==
                 Compound.add(flat_compound(), RDF.Data.statements(data))
      end)

      # a list of triples
      assert Compound.add(empty_compound(), [{EX.S1, EX.P1, EX.O1}, {EX.S2, EX.P2, EX.O2}]) ==
               flat_compound()

      # a list of RDF.Descriptions and RDF.Graphs
      assert Compound.add(empty_compound(), [
               RDF.description(EX.S1, init: {EX.P1, EX.O1}),
               RDF.graph({EX.S2, EX.P2, EX.O2})
             ]) == flat_compound()
    end

    test "unasserted triples with assertion_mode" do
      assert empty_compound()
             |> Compound.add({EX.S1, EX.P1, EX.O1}, assertion_mode: :unasserted)
             |> Compound.add({EX.S2, EX.P2, EX.O2}, assertion_mode: :unasserted) ==
               unasserted_flat_compound()

      assert Compound.add(
               empty_compound(),
               [
                 {EX.S1, EX.P1, EX.O1},
                 RDF.description(EX.S2, init: {EX.P2, EX.O2})
               ],
               assertion_mode: :unasserted
             ) ==
               unasserted_flat_compound()
    end

    test "asserted triples with asserted opt" do
      assert empty_compound()
             |> Compound.add([], asserted: {EX.S1, EX.P1, EX.O1})
             |> Compound.add([], asserted: {EX.S2, EX.P2, EX.O2}) ==
               flat_compound()

      assert Compound.add(empty_compound(), [],
               asserted: [
                 {EX.S1, EX.P1, EX.O1},
                 RDF.graph({EX.S2, EX.P2, EX.O2})
               ]
             ) ==
               flat_compound()
    end

    test "unasserted triples with unasserted opt" do
      assert empty_compound()
             |> Compound.add([], unasserted: {EX.S1, EX.P1, EX.O1})
             |> Compound.add([], unasserted: {EX.S2, EX.P2, EX.O2}) ==
               unasserted_flat_compound()

      assert Compound.add(empty_compound(), [],
               unasserted: [
                 {EX.S1, EX.P1, EX.O1},
                 RDF.graph({EX.S2, EX.P2, EX.O2})
               ]
             ) ==
               unasserted_flat_compound()
    end

    test "asserted and unasserted triples" do
      assert empty_compound()
             |> Compound.add([], asserted: {EX.S1, EX.P1, EX.O1})
             |> Compound.add([], unasserted: {EX.S2, EX.P2, EX.O2}) ==
               mixed_asserted_flat_compound()

      assert empty_compound()
             |> Compound.add([{EX.S2, EX.P2, EX.O2}], assertion_mode: :unasserted)
             |> Compound.add([{EX.S1, EX.P1, EX.O1}]) ==
               mixed_asserted_flat_compound()
    end

    test "an already included triple" do
      assert Compound.add(flat_compound(), [{EX.S2, EX.P2, EX.O2}]) == flat_compound()
    end

    test "an already included triple in the opposite assertion_mode" do
      assert Compound.add(flat_compound(), [{EX.S2, EX.P2, EX.O2}], assertion_mode: :unasserted) ==
               mixed_asserted_flat_compound()

      assert Compound.add(unasserted_flat_compound(), [{EX.S1, EX.P1, EX.O1}],
               assertion_mode: :asserted
             ) ==
               mixed_asserted_flat_compound()

      assert Compound.add(flat_compound(), [], unasserted: {EX.S2, EX.P2, EX.O2}) ==
               mixed_asserted_flat_compound()

      assert Compound.add(mixed_asserted_flat_compound(), [], asserted: {EX.S2, EX.P2, EX.O2}) ==
               flat_compound()
    end

    test "the same triple as both asserted and unasserted" do
      assert Compound.add(empty_compound(), [], asserted: triples(), unasserted: triples()) ==
               flat_compound()

      assert Compound.add(empty_compound(), triples(), unasserted: [{EX.S2, EX.P2, EX.O2}]) ==
               flat_compound()
    end

    test "a triple that is an element of a sub-compound" do
      assert Compound.add(nested_compound(), [{EX.S3, EX.P3, EX.O3}]) ==
               compound_with_duplicate_triple_in_sub_compound()
    end
  end

  describe "delete/2" do
    test "all triples with default assertion_mode :all" do
      # a single triple
      assert mixed_asserted_flat_compound()
             |> Compound.delete({EX.S1, EX.P1, EX.O1})
             |> Compound.delete({EX.S2, EX.P2, EX.O2}) ==
               empty_compound()

      # a list of triples
      assert Compound.delete(
               mixed_asserted_flat_compound(),
               [
                 {EX.S1, EX.P1, EX.O1},
                 RDF.description(EX.S2, init: {EX.P2, EX.O2})
               ]
             ) ==
               empty_compound()
    end

    test "only asserted triples with assertion_mode" do
      # a single triple
      assert flat_compound()
             |> Compound.delete({EX.S1, EX.P1, EX.O1}, assertion_mode: :asserted)
             |> Compound.delete({EX.S2, EX.P2, EX.O2}, assertion_mode: :asserted) ==
               empty_compound()

      # a list of triples
      assert Compound.delete(flat_compound(), [{EX.S1, EX.P1, EX.O1}, {EX.S2, EX.P2, EX.O2}],
               assertion_mode: :asserted
             ) ==
               empty_compound()

      # unasserted are not touched
      assert Compound.delete(
               unasserted_flat_compound(),
               [
                 {EX.S1, EX.P1, EX.O1},
                 {EX.S2, EX.P2, EX.O2}
               ],
               assertion_mode: :asserted
             ) ==
               unasserted_flat_compound()

      # with RDF.ex data structures
      [
        EX.Foo |> EX.bar(42) |> EX.baz(EX.O),
        RDF.graph(EX.Foo |> EX.bar(42) |> EX.baz(EX.O))
      ]
      |> Enum.each(fn data ->
        assert flat_compound()
               |> Compound.add(data)
               |> Compound.delete(data, assertion_mode: :asserted) ==
                 flat_compound()
      end)
    end

    test "only unasserted triples with assertion_mode" do
      # a single triple
      assert unasserted_flat_compound()
             |> Compound.delete({EX.S1, EX.P1, EX.O1}, assertion_mode: :unasserted)
             |> Compound.delete({EX.S2, EX.P2, EX.O2}, assertion_mode: :unasserted) ==
               empty_compound()

      # asserted are not touched
      assert flat_compound()
             |> Compound.delete({EX.S1, EX.P1, EX.O1}, assertion_mode: :unasserted)
             |> Compound.delete({EX.S2, EX.P2, EX.O2}, assertion_mode: :unasserted) ==
               flat_compound()

      # a list of triples
      assert Compound.delete(
               unasserted_flat_compound(),
               [
                 {EX.S1, EX.P1, EX.O1},
                 RDF.description(EX.S2, init: {EX.P2, EX.O2})
               ],
               assertion_mode: :unasserted
             ) ==
               empty_compound()
    end

    test "only asserted triples with asserted opt" do
      assert flat_compound()
             |> Compound.delete([], asserted: {EX.S1, EX.P1, EX.O1})
             |> Compound.delete([], asserted: {EX.S2, EX.P2, EX.O2}) ==
               empty_compound()

      # unasserted are not touched
      assert unasserted_flat_compound()
             |> Compound.delete([], asserted: {EX.S1, EX.P1, EX.O1})
             |> Compound.delete([], asserted: {EX.S2, EX.P2, EX.O2}) ==
               unasserted_flat_compound()

      assert Compound.delete(flat_compound(), [],
               asserted: [
                 {EX.S1, EX.P1, EX.O1},
                 RDF.graph({EX.S2, EX.P2, EX.O2})
               ]
             ) ==
               empty_compound()
    end

    test "only unasserted triples with unasserted opt" do
      assert unasserted_flat_compound()
             |> Compound.delete([], unasserted: {EX.S1, EX.P1, EX.O1})
             |> Compound.delete([], unasserted: {EX.S2, EX.P2, EX.O2}) ==
               empty_compound()

      assert Compound.delete(unasserted_flat_compound(), [],
               unasserted: [
                 {EX.S1, EX.P1, EX.O1},
                 RDF.graph({EX.S2, EX.P2, EX.O2})
               ]
             ) ==
               empty_compound()
    end

    test "particular asserted and unasserted triples only" do
      assert mixed_asserted_flat_compound()
             |> Compound.delete([], asserted: {EX.S1, EX.P1, EX.O1})
             |> Compound.delete([], unasserted: {EX.S2, EX.P2, EX.O2}) ==
               empty_compound()

      assert mixed_asserted_flat_compound()
             |> Compound.delete([{EX.S2, EX.P2, EX.O2}], assertion_mode: :unasserted)
             |> Compound.delete([{EX.S1, EX.P1, EX.O1}]) ==
               empty_compound()
    end

    test "a triple that is not an element" do
      assert Compound.delete(empty_compound(), [{EX.S2, EX.P2, EX.O2}]) == empty_compound()
    end

    test "a triple that is an element of a sub-compound" do
      assert Compound.delete(nested_compound(), [
               {EX.S3, EX.P3, EX.O3},
               {EX.S4, EX.P4, EX.O4}
             ]) ==
               Compound.put_sub_compound(flat_compound(), Compound.new([], EX.SubCompound))

      assert Compound.delete(compound_with_duplicate_triple_in_sub_compound(), [
               {EX.S3, EX.P3, EX.O3},
               {EX.S4, EX.P4, EX.O4}
             ]) ==
               Compound.put_sub_compound(flat_compound(), Compound.new([], EX.SubCompound))

      assert Compound.delete(unasserted_nested_compound(), [],
               unasserted: [
                 {EX.S3, EX.P3, EX.O3},
                 {EX.S4, EX.P4, EX.O4}
               ]
             ) ==
               Compound.put_sub_compound(
                 unasserted_flat_compound(),
                 Compound.new([], EX.SubCompound)
               )
    end
  end

  describe "delete_descriptions/2" do
    test "a single subject" do
      assert flat_compound()
             |> Compound.delete_descriptions(EX.S1)
             |> Compound.delete_descriptions(EX.S2) == empty_compound()
    end

    test "a list of subjects" do
      assert Compound.delete_descriptions(flat_compound(), [EX.S1, EX.S2]) ==
               empty_compound()
    end

    test "a subject that is not present in the given compound" do
      assert Compound.delete_descriptions(empty_compound(), EX.S2) == empty_compound()
    end

    test "a subject that is only present in a sub-compound" do
      assert Compound.new([], EX.Compound, sub_compounds: Compound.new(triples(), EX.Sub))
             |> Compound.delete_descriptions([EX.S1, EX.S2]) ==
               Compound.new([], EX.Compound, sub_compounds: Compound.new([], EX.Sub))

      assert Compound.new(triples(), EX.Compound, sub_compounds: Compound.new(triples(), EX.Sub))
             |> Compound.delete_descriptions([EX.S1, EX.S2]) ==
               Compound.new([], EX.Compound, sub_compounds: Compound.new([], EX.Sub))
    end

    test "assertion_mode" do
      assert Compound.delete_descriptions(flat_compound(), [EX.S1, EX.S2], assertion_mode: :all) ==
               empty_compound()

      assert Compound.delete_descriptions(flat_compound(), [EX.S1, EX.S2],
               assertion_mode: :asserted
             ) ==
               empty_compound()

      assert Compound.delete_descriptions(flat_compound(), [EX.S1, EX.S2],
               assertion_mode: :unasserted
             ) ==
               flat_compound()

      assert Compound.delete_descriptions(unasserted_flat_compound(), [EX.S1, EX.S2],
               assertion_mode: :asserted
             ) ==
               unasserted_flat_compound()

      assert Compound.delete_descriptions(unasserted_flat_compound(), [EX.S1, EX.S2],
               assertion_mode: :unasserted
             ) ==
               empty_compound()

      assert Compound.delete_descriptions(unasserted_flat_compound(), [EX.S1, EX.S2],
               assertion_mode: :all
             ) ==
               empty_compound()

      assert Compound.delete_descriptions(mixed_asserted_flat_compound(), [EX.S1, EX.S2],
               assertion_mode: :all
             ) ==
               empty_compound()

      assert Compound.delete_descriptions(mixed_asserted_flat_compound(), [EX.S1, EX.S2],
               assertion_mode: :asserted
             ) ==
               Compound.delete_descriptions(mixed_asserted_flat_compound(), EX.S1)

      assert Compound.delete_descriptions(mixed_asserted_flat_compound(), [EX.S1, EX.S2],
               assertion_mode: :unasserted
             ) ==
               Compound.delete_descriptions(mixed_asserted_flat_compound(), EX.S2)

      assert Compound.delete_descriptions(nested_compound(), [EX.S1, EX.S2, EX.S3, EX.S4],
               assertion_mode: :all
             ) ==
               empty_nested_compound()

      assert Compound.delete_descriptions(nested_compound(), [EX.S1, EX.S2, EX.S3, EX.S4],
               assertion_mode: :asserted
             ) ==
               empty_nested_compound()

      assert Compound.delete_descriptions(nested_compound(), [EX.S1, EX.S2, EX.S3, EX.S4],
               assertion_mode: :unasserted
             ) ==
               nested_compound()

      assert Compound.delete_descriptions(
               unasserted_nested_compound(),
               [EX.S1, EX.S2, EX.S3, EX.S4],
               assertion_mode: :unasserted
             ) ==
               empty_nested_compound()
    end
  end

  describe "pop/2" do
    test "a single subject" do
      assert Compound.pop(flat_compound(), EX.S1) ==
               {Description.new(EX.S1, init: {EX.P1, EX.O1}),
                flat_compound({EX.S2, EX.P2, EX.O2})}
    end

    test "a subjects that is not an element" do
      assert Compound.pop(empty_compound(), EX.S2) == {nil, empty_compound()}
    end

    test "a subjects that is an element of a sub-compound" do
      assert Compound.pop(nested_compound(), EX.S1) ==
               {Description.new(EX.S1, init: {EX.P1, EX.O1}),
                nested_compound({EX.S2, EX.P2, EX.O2}, sub_compound())}

      assert Compound.pop(nested_compound(), EX.S3) ==
               {Description.new(EX.S3, init: {EX.P3, EX.O3}),
                nested_compound(triples(), sub_compound({EX.S4, EX.P4, EX.O4}))}

      assert nested_compound([{EX.S4, EX.P4, EX.O5} | triples()], sub_compound())
             |> Compound.pop(EX.S4) ==
               {Description.new(EX.S4, init: {EX.P4, [EX.O4, EX.O5]}),
                nested_compound(triples(), sub_compound({EX.S3, EX.P3, EX.O3}))}
    end

    test "assertion_mode" do
      assert Compound.pop(flat_compound(), EX.S1, assertion_mode: :asserted) ==
               {Description.new(EX.S1, init: {EX.P1, EX.O1}),
                flat_compound({EX.S2, EX.P2, EX.O2})}

      assert Compound.pop(flat_compound(), EX.S1, assertion_mode: :unasserted) ==
               {nil, flat_compound()}

      assert Compound.pop(unasserted_flat_compound(), EX.S1, assertion_mode: :asserted) ==
               {nil, unasserted_flat_compound()}

      assert Compound.pop(unasserted_flat_compound(), EX.S1, assertion_mode: :unasserted) ==
               {Description.new(EX.S1, init: {EX.P1, EX.O1}),
                unasserted_flat_compound({EX.S2, EX.P2, EX.O2})}

      assert Compound.pop(unasserted_flat_compound(), EX.S1, assertion_mode: :all) ==
               {Description.new(EX.S1, init: {EX.P1, EX.O1}),
                unasserted_flat_compound({EX.S2, EX.P2, EX.O2})}

      mixed_asserted_flat_compound =
        mixed_asserted_flat_compound(triples(), [{EX.S2, EX.P3, EX.O3}])

      assert Compound.pop(mixed_asserted_flat_compound, EX.S2, assertion_mode: :all) ==
               {Description.new(EX.S2, init: [{EX.P2, EX.O2}, {EX.P3, EX.O3}]),
                mixed_asserted_flat_compound([{EX.S1, EX.P1, EX.O1}], [])}

      assert Compound.pop(mixed_asserted_flat_compound, EX.S2, assertion_mode: :asserted) ==
               {Description.new(EX.S2, init: [{EX.P2, EX.O2}]),
                mixed_asserted_flat_compound([{EX.S1, EX.P1, EX.O1}], [{EX.S2, EX.P3, EX.O3}])}

      assert Compound.pop(mixed_asserted_flat_compound, EX.S2, assertion_mode: :unasserted) ==
               {Description.new(EX.S2, init: [{EX.P3, EX.O3}]),
                mixed_asserted_flat_compound(triples(), [])}

      assert Compound.pop(unasserted_nested_compound(), EX.S4, assertion_mode: :unasserted) ==
               {Description.new(EX.S4, init: [{EX.P4, EX.O4}]),
                unasserted_nested_compound(
                  triples(),
                  unasserted_sub_compound({EX.S3, EX.P3, EX.O3})
                )}
    end
  end

  describe "pop/1" do
    test "pops elements until empty" do
      [triple1, triple2, triple3, triple4] = Compound.triples(nested_compound())
      assert {^triple1, compound} = Compound.pop(nested_compound())
      assert {^triple2, compound} = Compound.pop(compound)
      assert {^triple3, compound} = Compound.pop(compound)
      assert {^triple4, compound} = Compound.pop(compound)
      assert Compound.empty?(compound)
      assert {nil, compound} = Compound.pop(compound)
      assert Compound.empty?(compound)
    end

    test "asserted and unasserted statements" do
      [triple1, triple2] = Compound.triples(mixed_asserted_flat_compound())
      assert {^triple1, compound} = Compound.pop(mixed_asserted_flat_compound())
      assert {^triple2, compound} = Compound.pop(compound)
      assert Compound.empty?(compound)
    end
  end

  describe "sub_compounds/1" do
    test "without any sub-compounds present" do
      assert Compound.sub_compounds(empty_compound()) == []
      assert Compound.sub_compounds(flat_compound()) == []
    end

    test "returns the sub-compounds with super-compound supplemented" do
      assert Compound.sub_compounds(nested_compound()) ==
               [Compound.put_super_compound(sub_compound(), nested_compound())]
    end
  end

  describe "sub_compound/2" do
    test "when the given sub-compound does not exist" do
      assert Compound.sub_compound(flat_compound(), EX.Unknown) == nil
    end

    test "when the given sub-compound exists directly as a sub-compound in the given compound" do
      assert Compound.sub_compound(nested_compound(), EX.SubCompound) ==
               Compound.put_super_compound(sub_compound(), nested_compound())
    end

    test "when the given sub-compound exists deeply as a sub-compound in the given compound" do
      assert Compound.sub_compound(deeply_nested_compound(), EX.DeepCompound) ==
               Compound.new(more_triples(), EX.DeepCompound, super_compounds: sub_compound())
    end
  end

  describe "put_sub_compound/2" do
    test "with a compound" do
      assert Compound.put_sub_compound(flat_compound(), sub_compound()) == nested_compound()
    end

    test "with triples, a compound is created implicitly" do
      assert %Compound{} = compound = Compound.put_sub_compound(flat_compound(), other_triples())
      assert [%Compound{} = sub_compound] = Map.values(compound.sub_compounds)
      assert %BlankNode{} = id = Compound.id(sub_compound)
      assert sub_compound == Compound.new(other_triples(), id)
    end

    test "removes the now implicit super-compound" do
      assert Compound.put_sub_compound(
               nested_compound(),
               Compound.put_super_compound(sub_compound(), nested_compound())
             ) ==
               nested_compound()
    end

    test "an already included compound is overwritten" do
      original_sub_compound = Compound.new([{EX.S5, EX.P5, EX.O5}], EX.SubCompound)
      original_nested_compound = Compound.put_sub_compound(flat_compound(), original_sub_compound)

      assert Compound.put_sub_compound(original_nested_compound, sub_compound()) ==
               nested_compound()
    end
  end

  describe "delete_sub_compound/2" do
    test "with a compound" do
      assert Compound.delete_sub_compound(nested_compound(), sub_compound()) == flat_compound()
    end

    test "with a compound id" do
      assert Compound.delete_sub_compound(nested_compound(), EX.SubCompound) == flat_compound()
    end

    test "when the given compound is not a sub-compound" do
      assert Compound.delete_sub_compound(flat_compound(), sub_compound()) == flat_compound()
    end
  end

  describe "put_super_compound/2" do
    test "with a compound id" do
      assert Compound.put_super_compound(flat_compound(), EX.SuperCompound)
             |> Compound.super_compounds() == [RDF.iri(EX.SuperCompound)]
    end

    test "with a compound" do
      assert Compound.put_super_compound(
               flat_compound(),
               Compound.new([], EX.SuperCompound,
                 annotations: {EX.inherited_p(), EX.inherited_o()}
               )
             ) == compound_with_super_compound()
    end

    test "with a description" do
      assert Compound.put_super_compound(
               flat_compound(),
               RDF.description(EX.SuperCompound, init: {EX.inherited_p(), EX.inherited_o()})
             ) == compound_with_super_compound()
    end

    test "an already included super-compound is overwritten" do
      assert Compound.new(triples(), EX.Compound,
               super_compounds:
                 RDF.description(EX.SuperCompound, init: {EX.original_p(), EX.original_o()})
             )
             |> Compound.put_super_compound(
               RDF.description(EX.SuperCompound, init: {EX.inherited_p(), EX.inherited_o()})
             ) ==
               compound_with_super_compound()
    end
  end

  describe "delete_super_compound/2" do
    test "with a compound id" do
      assert Compound.delete_super_compound(compound_with_super_compound(), EX.SuperCompound) ==
               flat_compound()
    end

    test "with a compound" do
      assert Compound.delete_super_compound(
               compound_with_super_compound(),
               Compound.new([], EX.SuperCompound)
             ) ==
               flat_compound()
    end

    test "when the given compound is not a super-compound" do
      assert Compound.delete_super_compound(flat_compound(), EX.SuperCompound) ==
               flat_compound()
    end
  end

  describe "annotations/1" do
    test "without enabling inherited annotations" do
      annotations = RDF.description(EX.Compound, init: {EX.P, EX.O})

      assert Compound.new([], EX.Compound, annotations: annotations) |> Compound.annotations() ==
               annotations
    end

    test "with inherited: true" do
      annotations = RDF.description(EX.Compound, init: {EX.P, EX.O})
      inherited = RDF.description(EX.SuperCompound, init: {EX.inherited_p(), EX.inherited_o()})

      assert Compound.new([], EX.Compound, annotations: annotations, super_compounds: inherited)
             |> Compound.annotations(inherited: true) ==
               RDF.description(EX.Compound, init: [annotations, inherited])
    end
  end

  test "inherited_annotations/1" do
    assert Compound.inherited_annotations(compound_with_super_compound()) ==
             RDF.description(Compound.id(compound_with_super_compound()),
               init: {EX.inherited_p(), EX.inherited_o()}
             )

    assert compound_with_super_compound()
           |> Compound.put_super_compound(
             RDF.description(EX.Other,
               init: [
                 {EX.inherited_p(), EX.inherited_o2()},
                 {EX.other_p(), EX.other_o()}
               ]
             )
           )
           |> Compound.inherited_annotations() ==
             RDF.description(Compound.id(compound_with_super_compound()),
               init: [
                 {EX.inherited_p(), [EX.inherited_o(), EX.inherited_o2()]},
                 {EX.other_p(), EX.other_o()}
               ]
             )
  end

  test "inherited_annotations/2" do
    assert Compound.inherited_annotations(compound_with_super_compound(), EX.SuperCompound) ==
             RDF.description(EX.SuperCompound, init: {EX.inherited_p(), EX.inherited_o()})
  end

  test "add_annotations/2" do
    assert flat_compound()
           |> Compound.add_annotations({EX.Foo, EX.Bar})
           |> Compound.add_annotations({EX.Foo, EX.Baz}) ==
             %Compound{
               flat_compound()
               | annotations: RDF.description(EX.Compound, init: {EX.Foo, [EX.Bar, EX.Baz]})
             }

    assert flat_compound()
           |> Compound.add_annotations({EX.Foo, EX.Bar})
           |> Compound.add_annotations({EX.Foo, EX.Baz}) ==
             %Compound{
               flat_compound()
               | annotations: RDF.description(EX.Compound, init: {EX.Foo, [EX.Bar, EX.Baz]})
             }
  end

  test "delete_annotations/2" do
    assert Compound.new([], EX.Compound, annotations: {EX.Foo, EX.Bar})
           |> Compound.delete_annotations({EX.Foo, EX.Bar}) ==
             empty_compound()
  end

  describe "Enumerable protocol" do
    test "Enum.count" do
      assert Enum.count(nested_compound()) == 4
    end

    test "Enum.member?" do
      Enum.each(triples(), fn triple ->
        assert Enum.member?(flat_compound(), triple) == true
      end)

      assert Enum.member?(flat_compound(), {EX.S2, EX.P2, EX.O3}) == false
    end

    test "Enum.reduce" do
      assert Enum.reduce(flat_compound(), [], fn triple, acc -> [triple | acc] end) ==
               triples() |> Enum.map(&RDF.triple/1) |> Enum.reverse()

      assert Enum.reduce(nested_compound(), [], fn triple, acc -> [triple | acc] end) ==
               (triples() ++ other_triples()) |> Enum.map(&RDF.triple/1) |> Enum.reverse()
    end

    test "Enum.at (for Enumerable.slice/1)" do
      assert Enum.at(flat_compound(), 0) == {RDF.iri(EX.S1), RDF.iri(EX.P1), RDF.iri(EX.O1)}
      assert Enum.at(flat_compound(), 1) == {RDF.iri(EX.S2), RDF.iri(EX.P2), RDF.iri(EX.O2)}
      assert Enum.at(flat_compound(), 2) == nil
      assert Enum.at(nested_compound(), 0) == {RDF.iri(EX.S1), RDF.iri(EX.P1), RDF.iri(EX.O1)}
      assert Enum.at(nested_compound(), 1) == {RDF.iri(EX.S2), RDF.iri(EX.P2), RDF.iri(EX.O2)}
      assert Enum.at(nested_compound(), 2) == {RDF.iri(EX.S3), RDF.iri(EX.P3), RDF.iri(EX.O3)}
      assert Enum.at(nested_compound(), 3) == {RDF.iri(EX.S4), RDF.iri(EX.P4), RDF.iri(EX.O4)}
      assert Enum.at(nested_compound(), 4) == nil
    end
  end

  describe "RDF.Data protocol" do
    test "merge/2" do
      assert RDF.Data.merge(flat_compound(), {EX.S1, EX.P1, EX.O3}) ==
               flat_compound() |> Compound.graph(name: nil) |> Graph.add({EX.S1, EX.P1, EX.O3})

      assert RDF.Data.merge(flat_compound(), {EX.S1, EX.P1, EX.O3, EX.NamedGraph}) ==
               flat_compound()
               |> Compound.graph(name: EX.NamedGraph)
               |> Graph.add({EX.S1, EX.P1, EX.O3})

      assert RDF.Data.merge(flat_compound(), Description.new(EX.S1, init: {EX.P1, EX.O3})) ==
               flat_compound() |> Compound.graph(name: nil) |> Graph.add({EX.S1, EX.P1, EX.O3})

      assert RDF.Data.merge(flat_compound(), Graph.new({EX.S1, EX.P1, EX.O3})) ==
               flat_compound() |> Compound.graph(name: nil) |> Graph.add({EX.S1, EX.P1, EX.O3})

      assert RDF.Data.merge(
               flat_compound(),
               Graph.new({EX.S1, EX.P1, EX.O3}, name: EX.NamedGraph)
             ) ==
               flat_compound()
               |> Compound.graph(name: EX.NamedGraph)
               |> Graph.add({EX.S1, EX.P1, EX.O3})

      assert RDF.Data.merge(
               flat_compound(),
               Dataset.new({EX.S1, EX.P1, EX.O3})
             ) ==
               flat_compound()
               |> Compound.graph()
               |> Dataset.new()
               |> Dataset.add({EX.S1, EX.P1, EX.O3})
    end

    test "merge/2 of two compounds" do
      assert RDF.Data.merge(flat_compound(), Compound.new([{EX.S1, EX.P1, EX.O3}], EX.Compound2)) ==
               flat_compound()
               |> Compound.graph(name: nil)
               |> Graph.add({EX.S1, EX.P1, EX.O3})
    end

    test "merge/2 with compound as secondary argument" do
      assert RDF.Data.merge(Description.new(EX.S1, init: {EX.P1, EX.O3}), flat_compound()) ==
               flat_compound() |> Compound.graph(name: nil) |> Graph.add({EX.S1, EX.P1, EX.O3})

      assert RDF.Data.merge(Graph.new({EX.S1, EX.P1, EX.O3}), flat_compound()) ==
               flat_compound() |> Compound.graph(name: nil) |> Graph.add({EX.S1, EX.P1, EX.O3})

      assert RDF.Data.merge(
               Graph.new({EX.S1, EX.P1, EX.O3}, name: EX.NamedGraph),
               flat_compound()
             ) ==
               flat_compound()
               |> Compound.graph(name: EX.NamedGraph)
               |> Graph.add({EX.S1, EX.P1, EX.O3})

      assert RDF.Data.merge(
               Dataset.new({EX.S1, EX.P1, EX.O3}),
               flat_compound()
             ) ==
               flat_compound()
               |> Compound.graph()
               |> Dataset.new()
               |> Dataset.add({EX.S1, EX.P1, EX.O3})
    end

    test "delete/2" do
      assert RDF.Data.delete(flat_compound(), {EX.S1, EX.P1, EX.O1}) ==
               Compound.delete(flat_compound(), {EX.S1, EX.P1, EX.O1})

      assert RDF.Data.delete(flat_compound(), {EX.Other, EX.p1(), EX.O2}) == flat_compound()
    end

    test "pop" do
      assert RDF.Data.pop(flat_compound()) == RTC.Compound.pop(flat_compound())
    end

    test "empty?/1" do
      assert RDF.Data.empty?(flat_compound()) == false
      assert RDF.Data.empty?(empty_compound()) == true
    end

    test "include?/2" do
      assert RDF.Data.include?(flat_compound(), {EX.S1, EX.P1, EX.O1})
      assert RDF.Data.include?(flat_compound(), {EX.S2, EX.P2, EX.O2})
      refute RDF.Data.include?(flat_compound(), {EX.Other, EX.p1(), EX.O2})
    end

    test "describes?/2" do
      assert RDF.Data.describes?(flat_compound(), EX.S1)
      assert RDF.Data.describes?(flat_compound(), EX.S2)
      refute RDF.Data.describes?(flat_compound(), EX.Other)
    end

    test "description/2 " do
      assert RDF.Data.description(flat_compound(), EX.S1) ==
               RDF.description(EX.S1, init: {EX.P1, EX.O1})

      assert RDF.Data.description(flat_compound(), EX.Other) ==
               Description.new(EX.Other)
    end

    test "descriptions/1" do
      assert RDF.Data.descriptions(flat_compound()) ==
               triples() |> RDF.graph() |> Graph.descriptions()
    end

    test "statements/1" do
      assert RDF.Data.statements(flat_compound()) == Enum.map(triples(), &Triple.new/1)
    end

    test "subjects/1" do
      assert RDF.Data.subjects(flat_compound()) == MapSet.new([RDF.iri(EX.S1), RDF.iri(EX.S2)])

      assert RDF.Data.subjects(nested_compound()) ==
               MapSet.new([RDF.iri(EX.S1), RDF.iri(EX.S2), RDF.iri(EX.S3), RDF.iri(EX.S4)])
    end

    test "predicates/1" do
      assert RDF.Data.predicates(flat_compound()) == MapSet.new([RDF.iri(EX.P1), RDF.iri(EX.P2)])
    end

    test "objects/1" do
      assert RDF.Data.objects(flat_compound()) ==
               MapSet.new([RDF.iri(EX.O1), RDF.iri(EX.O2)])
    end

    test "resources/1" do
      assert RDF.Data.resources(flat_compound()) ==
               MapSet.new([
                 RDF.iri(EX.S1),
                 RDF.iri(EX.S2),
                 RDF.iri(EX.P1),
                 RDF.iri(EX.P2),
                 RDF.iri(EX.O1),
                 RDF.iri(EX.O2)
               ])
    end

    test "subject_count/1" do
      assert RDF.Data.subject_count(flat_compound()) == 2
      assert RDF.Data.subject_count(nested_compound()) == 4
    end

    test "statement_count/1" do
      assert RDF.Data.statement_count(flat_compound()) == 2
    end

    test "values/1" do
      assert RDF.Data.values(flat_compound()) ==
               flat_compound() |> Compound.graph() |> RDF.Data.values()
    end

    test "equal/2" do
      assert RDF.Data.equal?(flat_compound(), flat_compound())

      assert RDF.Data.equal?(
               flat_compound(),
               Compound.put_sub_compound(flat_compound(), Compound.new([]))
             )

      assert RDF.Data.equal?(flat_compound(), Compound.graph(flat_compound()))

      assert RDF.Data.equal?(
               flat_compound(),
               Compound.graph(flat_compound()) |> Graph.change_name(EX.Graph)
             )

      refute RDF.Data.equal?(
               flat_compound(),
               flat_compound() |> Compound.delete_descriptions(EX.S2)
             )

      refute RDF.Data.equal?(
               flat_compound() |> Compound.delete_descriptions(EX.S2),
               flat_compound()
             )
    end
  end

  describe "Inspect protocol" do
    test "it includes a header with the compound id" do
      {header, _} = inspect_parts(flat_compound())
      assert header == "#RTC.Compound<id: #{inspect(Compound.id(flat_compound()))}"
    end

    test "it includes graph_name when a different graph name than the compound id is set" do
      graph_name = RDF.iri(EX.Graph)
      compound = Compound.new(triples(), EX.Compound, name: graph_name)

      {header, _} = inspect_parts(compound)

      assert header ==
               "#RTC.Compound<id: #{inspect(Compound.id(compound))}, graph_name: #{inspect(graph_name)}"
    end

    test "it encodes the graph in Turtle" do
      {_, body} = inspect_parts(flat_compound())
      graph = Compound.to_rdf(flat_compound(), element_style: :elements)

      assert body ==
               "  " <>
                 (RDF.Turtle.write_string!(graph, indent: 2) |> String.trim()) <> "\n>"
    end

    test "the graph includes descriptions of super-compounds" do
      {_, body} = inspect_parts(compound_with_super_compound())

      graph =
        compound_with_super_compound()
        |> Compound.to_rdf(element_style: :elements)
        |> Graph.add(
          RDF.description(EX.SuperCompound, init: {EX.inherited_p(), EX.inherited_o()})
        )

      assert body ==
               "  " <>
                 (RDF.Turtle.write_string!(graph, indent: 2) |> String.trim()) <> "\n>"
    end

    def inspect_parts(compound, opts \\ []) do
      inspect_form = inspect(compound, opts)
      [header, body] = String.split(inspect_form, "\n", parts: 2)
      {header, body}
    end
  end
end
