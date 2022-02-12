defmodule RTC.Compound do
  alias RDF.{Statement, Triple, IRI, BlankNode, Description, Graph}

  @enforce_keys [:triples, :annotations]
  defstruct [
    # we have no explicit id field, since we're using the subject of the description for this
    triples: MapSet.new(),
    sub_compounds: %{},
    annotations: nil
  ]

  @type t :: %__MODULE__{
          triples: MapSet.t(),
          sub_compounds: %{(IRI.t() | BlankNode.t()) => t},
          annotations: Description.t()
        }

  @type id :: Statement.subject()
  @type coercible_id :: Statement.coercible_subject()

  @type triple :: Triple.t()

  @type coercible_triple ::
          {
            Statement.coercible_subject(),
            Statement.coercible_predicate(),
            Statement.coercible_object()
          }

  @spec id(t) :: id()
  def id(%__MODULE__{} = compound), do: compound.annotations.subject

  @spec new([coercible_triple()], coercible_id(), keyword) :: t
  def new(triples, compound_id, opts \\ []) do
    triples = for triple <- triples, into: MapSet.new(), do: RDF.triple(triple)
    sub_compounds = opts |> Keyword.get(:sub_compound) |> List.wrap() |> MapSet.new()

    %__MODULE__{
      triples: triples,
      sub_compounds: Map.new(sub_compounds, &{id(&1), &1}),
      annotations: new_annotation(compound_id, Keyword.get(opts, :annotations))
    }
  end

  defp new_annotation(compound_id, nil), do: RDF.description(compound_id)

  defp new_annotation(compound_id, description),
    do: RDF.description(compound_id, init: description)

  @spec from_rdf(Graph.t(), coercible_id()) :: t
  def from_rdf(%Graph{} = graph, compound_id), do: do_from_rdf(graph, compound_id, [])

  defp do_from_rdf(graph, compound_id, parent_compound_ids) do
    if compound_id in parent_compound_ids do
      raise("circle in sub-compound #{compound_id}")
    end

    {elements, annotations} =
      if description = graph[compound_id] do
        parent_compound_ids
        |> Enum.reduce(description, fn parent_compound_id, description ->
          Description.delete(description, {RTC.subCompoundOf(), parent_compound_id})
        end)
        |> Description.pop(RTC.elements())
      else
        {[], []}
      end

    element_ofs =
      graph
      |> Graph.query({:triple?, RTC.elementOf(), compound_id})
      |> Enum.map(&Map.get(&1, :triple))

    sub_compounds =
      graph
      |> Graph.query({:sub_compound?, RTC.subCompoundOf(), compound_id})
      |> Enum.map(
        &do_from_rdf(graph, Map.get(&1, :sub_compound), [compound_id | parent_compound_ids])
      )

    new(
      List.wrap(elements) ++ element_ofs,
      compound_id,
      sub_compound: sub_compounds,
      annotations: annotations
    )
  end

  @spec to_rdf(t) :: Graph.t()
  def to_rdf(%__MODULE__{} = compound) do
    compound_id = id(compound)

    graph =
      Enum.reduce(
        compound.triples,
        RDF.graph(name: compound_id, init: compound.annotations),
        &Graph.add(&2, &1, add_annotations: {RTC.elementOf(), compound_id})
      )

    Enum.reduce(compound.sub_compounds, graph, fn {sub_compound_id, sub_compound}, graph ->
      graph
      |> Graph.add({sub_compound_id, RTC.subCompoundOf(), compound_id})
      |> Graph.add(to_rdf(sub_compound))
    end)
  end

  @spec graph(t) :: Graph.t()
  def graph(%__MODULE__{} = compound) do
    RDF.graph(name: id(compound), init: triples(compound))
  end

  @spec triples(t) :: [triple]
  def triples(%__MODULE__{} = compound) do
    compound
    |> triple_set()
    |> MapSet.to_list()
  end

  @spec triple_set(t) :: MapSet.t()
  def triple_set(%__MODULE__{} = compound) do
    Enum.reduce(compound.sub_compounds, compound.triples, fn {_, sub_compound}, triple_set ->
      MapSet.union(triple_set, triple_set(sub_compound))
    end)
  end

  @spec element?(t, coercible_triple) :: boolean
  def element?(%__MODULE__{} = compound, triple) do
    triple = RDF.triple(triple)

    triple in compound.triples or
      Enum.any?(compound.sub_compounds, fn {_, sub_compound} ->
        element?(sub_compound, triple)
      end)
  end

  @spec size(t) :: non_neg_integer
  def size(%__MODULE__{} = compound) do
    compound
    |> triple_set()
    |> MapSet.size()
  end

  @spec add(t, coercible_triple | [coercible_triple]) :: t
  def add(compound, triples)

  def add(%__MODULE__{} = compound, triples) when is_list(triples) do
    Enum.reduce(triples, compound, &add(&2, &1))
  end

  def add(%__MODULE__{} = compound, %Description{} = description) do
    add(compound, Description.triples(description))
  end

  def add(%__MODULE__{} = compound, %Graph{} = graph) do
    add(compound, Graph.triples(graph))
  end

  def add(%__MODULE__{} = compound, triple) do
    %__MODULE__{compound | triples: MapSet.put(compound.triples, RDF.triple(triple))}
  end

  @spec delete(t, coercible_triple | [coercible_triple]) :: t
  def delete(compound, triples)

  def delete(%__MODULE__{} = compound, triples) when is_list(triples) do
    Enum.reduce(triples, compound, &delete(&2, &1))
  end

  def delete(%__MODULE__{} = compound, %Description{} = description) do
    delete(compound, Description.triples(description))
  end

  def delete(%__MODULE__{} = compound, %Graph{} = graph) do
    delete(compound, Graph.triples(graph))
  end

  def delete(%__MODULE__{} = compound, triple) do
    triple = RDF.triple(triple)

    %__MODULE__{
      compound
      | triples: MapSet.delete(compound.triples, triple),
        sub_compounds:
          Map.new(compound.sub_compounds, fn {id, sub_compound} ->
            {id, delete(sub_compound, triple)}
          end)
    }
  end

  @spec put_sub_compound(t, t | [t]) :: t
  def put_sub_compound(compound, sub_compounds)

  def put_sub_compound(%__MODULE__{} = compound, sub_compounds) when is_list(sub_compounds) do
    Enum.reduce(sub_compounds, compound, &put_sub_compound(&2, &1))
  end

  def put_sub_compound(%__MODULE__{} = compound, %__MODULE__{} = sub_compound) do
    %__MODULE__{
      compound
      | sub_compounds: Map.put(compound.sub_compounds, id(sub_compound), sub_compound)
    }
  end

  @spec delete_sub_compound(t, t | id | [t | id]) :: t
  def delete_sub_compound(compound, sub_compounds)

  def delete_sub_compound(%__MODULE__{} = compound, sub_compounds) when is_list(sub_compounds) do
    Enum.reduce(sub_compounds, compound, &delete_sub_compound(&2, &1))
  end

  def delete_sub_compound(%__MODULE__{} = compound, %__MODULE__{} = sub_compound) do
    delete_sub_compound(compound, id(sub_compound))
  end

  def delete_sub_compound(%__MODULE__{} = compound, sub_compound_id) do
    %__MODULE__{
      compound
      | sub_compounds:
          Map.delete(compound.sub_compounds, Statement.coerce_subject(sub_compound_id))
    }
  end

  @spec add_annotations(t, Description.input()) :: t
  def add_annotations(%__MODULE__{} = compound, annotations) do
    %__MODULE__{compound | annotations: Description.add(compound.annotations, annotations)}
  end

  @spec put_annotations(t, Description.input()) :: t
  def put_annotations(%__MODULE__{} = compound, annotations) do
    %__MODULE__{compound | annotations: Description.put(compound.annotations, annotations)}
  end

  @spec delete_annotations(t, Description.input()) :: t
  def delete_annotations(%__MODULE__{} = compound, annotations) do
    %__MODULE__{compound | annotations: Description.delete(compound.annotations, annotations)}
  end

  defimpl Enumerable do
    alias RTC.Compound

    def count(%Compound{} = compound), do: {:ok, Compound.size(compound)}

    def member?(%Compound{} = compound, triple), do: {:ok, Compound.element?(compound, triple)}

    def slice(%Compound{} = compound),
      do: compound |> Compound.triple_set() |> Enumerable.slice()

    def reduce(%Compound{} = compound, acc, fun),
      do: compound |> Compound.triple_set() |> Enumerable.reduce(acc, fun)
  end
end
