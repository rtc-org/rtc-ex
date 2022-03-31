defmodule RTC.CompoundTest do
  use RTC.Case

  doctest RTC.Compound

  describe "new with an explicitly given id" do
    test "constructs a compound from a list of triples" do
      assert Compound.new(triples(), EX.Compound) == flat_compound()
    end

    test "constructs a compound from a description" do
      description = RDF.description(EX.S, init: [{EX.P1, EX.O1}, {EX.P2, EX.O2}])

      assert Compound.new(description, EX.Compound) ==
               %Compound{
                 graph:
                   graph([
                     RDF.triple({EX.S, EX.P1, EX.O1}),
                     RDF.triple({EX.S, EX.P2, EX.O2})
                   ]),
                 annotations: RDF.description(EX.Compound)
               }
    end

    test "constructs a compound from a graph" do
      assert triples() |> RDF.graph() |> Compound.new(EX.Compound) == flat_compound()
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

    test "annotations are not added to sub-compounds" do
      nested_compound =
        Compound.new([{EX.S3, EX.P3, EX.O3}, {EX.S4, EX.P4, EX.O4}], EX.SubCompound)

      annotations = %{EX.ap1() => EX.AO1, EX.ap2() => EX.AO2}

      assert Compound.new(triples(), EX.Compound,
               sub_compounds: nested_compound,
               annotations: annotations
             ) ==
               %Compound{
                 graph: graph(triples()),
                 sub_compounds: %{Compound.id(nested_compound) => nested_compound},
                 annotations: RDF.description(EX.Compound, init: annotations)
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
                 graph:
                   graph(
                     id,
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
      assert [%Compound{} = sub_compound] = Compound.sub_compounds(compound)
      assert %BlankNode{} = id = Compound.id(sub_compound)
      assert sub_compound == Compound.new(nested_triples, id)

      assert %Compound{} = compound = Compound.new(triples)
      assert %BlankNode{} = Compound.id(compound)
      assert [%Compound{} = sub_compound] = Compound.sub_compounds(compound)
      assert %BlankNode{} = id = Compound.id(sub_compound)
      assert sub_compound == Compound.new(nested_triples, id)
    end

    test "super_compounds opt" do
      assert Compound.new(triples(), EX.Compound, super_compounds: EX.SuperCompound) ==
               Compound.put_super_compound(flat_compound(), Description.new(EX.SuperCompound))

      assert Compound.new(triples(), EX.Compound,
               super_compounds: [EX.SuperCompound1, EX.SuperCompound2]
             ) ==
               Compound.put_super_compound(flat_compound(), [
                 Description.new(EX.SuperCompound1),
                 Description.new(EX.SuperCompound2)
               ])

      assert Compound.new(triples(), EX.Compound,
               super_compounds:
                 RDF.description(EX.SuperCompound, init: {EX.inherited_p(), EX.inherited_o()})
             ) ==
               compound_with_super_compound()

      assert Compound.new(triples(), EX.Compound,
               super_compounds:
                 Compound.new([], EX.SuperCompound,
                   annotations: {EX.inherited_p(), EX.inherited_o()}
                 )
             ) ==
               compound_with_super_compound()
    end
  end

  test "reset_id/2" do
    assert flat_compound()
           |> Compound.reset_id(EX.new_id())
           |> Compound.id() == EX.new_id()
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
  end

  describe "graph/1" do
    test "the empty compound" do
      assert Compound.graph(empty_compound()) == graph()
    end

    test "a flat compound" do
      assert Compound.graph(flat_compound()) == graph(triples())
    end

    test "a nested compound" do
      assert Compound.graph(nested_compound()) ==
               graph(triples() ++ other_triples())
    end

    test "compound with blank node id" do
      bnode = RDF.bnode("compound")
      assert Compound.new([], bnode) |> Compound.graph() == graph(bnode)
    end
  end

  describe "triples/1" do
    test "returns the triple elements as a list" do
      assert Compound.triples(flat_compound()) == Enum.map(triples(), &RDF.triple/1)
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

  describe "include?/2" do
    test "returns whether the triple is an element of the compound" do
      Enum.each(triples(), fn triple ->
        assert Compound.include?(flat_compound(), triple) == true
      end)

      assert Compound.include?(flat_compound(), {EX.S2, EX.P2, EX.O3}) == false
    end

    test "takes nested compound into account" do
      Enum.each(triples() ++ other_triples(), fn triple ->
        assert Compound.include?(nested_compound(), triple) == true
      end)

      assert Compound.include?(nested_compound(), {EX.S2, EX.P2, EX.O3}) == false
    end
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
  end

  describe "add/2" do
    test "a single triple" do
      assert empty_compound()
             |> Compound.add({EX.S1, EX.P1, EX.O1})
             |> Compound.add({EX.S2, EX.P2, EX.O2}) == flat_compound()
    end

    test "with RDF.ex data structures" do
      [
        EX.Foo |> EX.bar(42) |> EX.baz(EX.O),
        RDF.graph(EX.Foo |> EX.bar(42) |> EX.baz(EX.O))
      ]
      |> Enum.each(fn data ->
        assert Compound.add(flat_compound(), data) ==
                 Compound.add(flat_compound(), RDF.Data.statements(data))
      end)
    end

    test "a list of triples" do
      assert Compound.add(empty_compound(), [{EX.S1, EX.P1, EX.O1}, {EX.S2, EX.P2, EX.O2}]) ==
               flat_compound()
    end

    test "a list of RDF.Descriptions and RDF.Graphs" do
      assert Compound.add(empty_compound(), [
               RDF.description(EX.S1, init: {EX.P1, EX.O1}),
               RDF.graph({EX.S2, EX.P2, EX.O2})
             ]) == flat_compound()
    end

    test "an already included triple" do
      assert Compound.add(flat_compound(), [{EX.S2, EX.P2, EX.O2}]) == flat_compound()
    end

    test "a triple that is an element of a sub-compound" do
      assert Compound.add(nested_compound(), [{EX.S3, EX.P3, EX.O3}]) ==
               compound_with_duplicate_triple_in_sub_compound()
    end
  end

  describe "delete/2" do
    test "a single triple" do
      assert flat_compound()
             |> Compound.delete({EX.S1, EX.P1, EX.O1})
             |> Compound.delete({EX.S2, EX.P2, EX.O2}) == empty_compound()
    end

    test "a list of triples" do
      assert Compound.delete(flat_compound(), [{EX.S1, EX.P1, EX.O1}, {EX.S2, EX.P2, EX.O2}]) ==
               empty_compound()
    end

    test "with RDF.ex data structures" do
      [
        EX.Foo |> EX.bar(42) |> EX.baz(EX.O),
        RDF.graph(EX.Foo |> EX.bar(42) |> EX.baz(EX.O))
      ]
      |> Enum.each(fn data ->
        assert flat_compound()
               |> Compound.add(data)
               |> Compound.delete(data) ==
                 flat_compound()
      end)
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
    end
  end

  describe "delete_descriptions/2" do
    test "a single subject" do
      assert flat_compound()
             |> Compound.delete_descriptions(EX.S1)
             |> Compound.delete_descriptions(EX.S2) == empty_compound()
    end

    test "a list of triples" do
      assert Compound.delete_descriptions(flat_compound(), [EX.S1, EX.S2]) ==
               empty_compound()
    end

    test "a triple that is not an element" do
      assert Compound.delete_descriptions(empty_compound(), EX.S2) == empty_compound()
    end

    test "a triple that is an element of a sub-compound" do
      assert Compound.new([], EX.Compound, sub_compounds: Compound.new(triples(), EX.Sub))
             |> Compound.delete_descriptions([EX.S1, EX.S2]) ==
               Compound.new([], EX.Compound, sub_compounds: Compound.new([], EX.Sub))

      assert Compound.new(triples(), EX.Compound, sub_compounds: Compound.new(triples(), EX.Sub))
             |> Compound.delete_descriptions([EX.S1, EX.S2]) ==
               Compound.new([], EX.Compound, sub_compounds: Compound.new([], EX.Sub))
    end
  end

  describe "put_sub_compound/2" do
    test "with a compound" do
      assert Compound.put_sub_compound(flat_compound(), sub_compound()) == nested_compound()
    end

    test "with triples, a compound is created implicitly" do
      assert %Compound{} = compound = Compound.put_sub_compound(flat_compound(), other_triples())
      assert [%Compound{} = sub_compound] = Compound.sub_compounds(compound)
      assert %BlankNode{} = id = Compound.id(sub_compound)
      assert sub_compound == Compound.new(other_triples(), id)
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

  test "add_annotations/2" do
    assert flat_compound()
           |> Compound.add_annotations({EX.Foo, EX.Bar})
           |> Compound.add_annotations({EX.Foo, EX.Baz}) ==
             %Compound{
               flat_compound()
               | annotations: RDF.description(EX.Compound, init: {EX.Foo, [EX.Bar, EX.Baz]})
             }
  end

  test "put_annotations/2" do
    assert flat_compound()
           |> Compound.put_annotations({EX.Foo, EX.Bar})
           |> Compound.put_annotations({EX.Foo, EX.Baz}) ==
             %Compound{
               flat_compound()
               | annotations: RDF.description(EX.Compound, init: {EX.Foo, EX.Baz})
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
end
