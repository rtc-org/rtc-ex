defmodule RTC.SPARQLTest do
  @moduledoc """
  These tests require a Ontotext GraphDB to be running on port 7200 and are
  therefor ignored by default. They can be run via

      $ mix test.sparql_client

  """

  use RTC.Case, async: false

  @moduletag :sparql_client

  alias RTC.Compound

  import RTC.Test.SparqlHelper

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
      |> insert()

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
      |> insert()

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
      |> insert()

      assert from_sparql!(EX.Compound) == nested_compound()
    end

    test "retrieves a nested compound (via rtc:elementOf)" do
      RDF.graph(name: RDF.iri(EX.Compound))
      |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
      |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
      |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
      |> insert()

      assert from_sparql!(EX.Compound) == nested_compound()
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
      |> insert()

      assert from_sparql!(EX.Compound) == deeply_nested_compound()
    end

    test "retrieves a deeply nested compound (via rtc:elementOf)" do
      RDF.graph(name: RDF.iri(EX.Compound))
      |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
      |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
      |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
      |> Graph.add(more_triples(), add_annotations: {RTC.elementOf(), EX.DeepCompound})
      |> Graph.add({EX.DeepCompound, RTC.subCompoundOf(), EX.SubCompound})
      |> insert()

      assert from_sparql!(EX.Compound) == deeply_nested_compound()
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
      |> insert()

      assert from_sparql!(EX.Compound) ==
               Compound.new(triples(), EX.Compound, annotations: {EX.foo(), EX.Bar})
    end

    test "retrieves annotations (sub-compounds)" do
      RDF.graph(name: RDF.iri(EX.Compound))
      |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
      |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
      |> Graph.add(EX.Compound |> EX.foo1(EX.Bar1))
      |> Graph.add(EX.SubCompound |> RTC.subCompoundOf(EX.Compound) |> EX.foo2(EX.Bar2))
      |> insert()

      assert from_sparql!(EX.Compound) ==
               Compound.new(triples(), EX.Compound,
                 sub_compounds: Compound.add_annotations(sub_compound(), {EX.foo2(), EX.Bar2}),
                 annotations: {EX.foo1(), EX.Bar1}
               )
    end

    test "annotations from super-compounds (rtc:elementOf)" do
      RDF.graph(name: RDF.iri(EX.Compound))
      |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
      |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
      |> Graph.add(EX.Compound |> EX.foo1(EX.Bar1))
      |> Graph.add(EX.SubCompound |> RTC.subCompoundOf(EX.Compound) |> EX.foo2(EX.Bar2))
      |> insert()

      assert from_sparql!(EX.SubCompound) ==
               Compound.new(other_triples(), EX.SubCompound,
                 annotations: {EX.foo2(), EX.Bar2},
                 super_compounds: Description.new(EX.Compound, init: {EX.foo1(), EX.Bar1})
               )
    end

    test "annotations from super-compounds (rtc:elements)" do
      RDF.graph(name: RDF.iri(EX.Compound))
      |> Graph.add(EX.Compound |> RTC.elements(triples()) |> EX.foo1(EX.Bar1))
      |> Graph.add(
        EX.SubCompound
        |> RTC.subCompoundOf(EX.Compound)
        |> RTC.elements(other_triples())
        |> EX.foo2(EX.Bar2)
      )
      |> insert()

      assert from_sparql!(EX.SubCompound) ==
               Compound.new(other_triples(), EX.SubCompound,
                 annotations: {EX.foo2(), EX.Bar2},
                 super_compounds: Description.new(EX.Compound, init: {EX.foo1(), EX.Bar1})
               )
    end

    test "annotations from different super-compounds" do
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
      |> insert()

      assert from_sparql!(EX.SubCompound) ==
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
      |> insert()

      assert from_sparql!(EX.Compound) ==
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
      RDF.graph(name: RDF.iri(EX.Compound))
      |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
      |> Graph.add({EX.Compound, RTC.subCompoundOf(), EX.Compound})
      |> insert()

      assert_raise RuntimeError, "circle in sub-compound #{RDF.iri(EX.Compound)}", fn ->
        from_sparql!(EX.Compound)
      end

      RDF.graph(name: RDF.iri(EX.Compound))
      |> Graph.add(triples(), add_annotations: {RTC.elementOf(), EX.Compound})
      |> Graph.add(other_triples(), add_annotations: {RTC.elementOf(), EX.SubCompound})
      |> Graph.add({EX.SubCompound, RTC.subCompoundOf(), EX.Compound})
      |> Graph.add({EX.Compound, RTC.subCompoundOf(), EX.SubCompound})
      |> insert()

      assert_raise RuntimeError, "circle in sub-compound #{RDF.iri(EX.Compound)}", fn ->
        from_sparql!(EX.Compound)
      end
    end
  end
end
