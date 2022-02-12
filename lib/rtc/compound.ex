defmodule RTC.Compound do
  alias RDF.{Statement, Triple, IRI, BlankNode, Description}

  @enforce_keys [:elements, :annotations]
  defstruct [
    # we have no explicit id field, since we're using the subject of the description for this
    elements: MapSet.new(),
    sub_compounds: %{},
    annotations: nil
  ]

  @type t :: %__MODULE__{
          elements: MapSet.t(),
          sub_compounds: %{(IRI.t() | BlankNode.t()) => t},
          annotations: Description.t()
        }

  @type id :: Statement.subject()
  @type coercible_id :: Statement.coercible_subject()

  @type element :: Triple.t()

  @type coercible_element ::
          {
            Statement.coercible_subject(),
            Statement.coercible_predicate(),
            Statement.coercible_object()
          }

  @spec id(t) :: id()
  def id(%__MODULE__{} = compound), do: compound.annotations.subject

  @spec new([coercible_element()], coercible_id(), keyword) :: t
  def new(elements, compound_id, opts \\ []) do
    elements = for element <- elements, into: MapSet.new(), do: RDF.triple(element)
    sub_compounds = opts |> Keyword.get(:sub_compound) |> List.wrap() |> MapSet.new()

    %__MODULE__{
      elements: elements,
      sub_compounds: Map.new(sub_compounds, &{id(&1), &1}),
      annotations: new_annotation(compound_id, Keyword.get(opts, :annotations))
    }
  end

  defp new_annotation(compound_id, nil), do: RDF.description(compound_id)

  defp new_annotation(compound_id, description),
    do: RDF.description(compound_id, init: description)

  @spec from_rdf(RDF.Graph.t(), coercible_id()) :: t
  def from_rdf(%RDF.Graph{} = graph, compound_id), do: do_from_rdf(graph, compound_id, [])

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
      |> RDF.Graph.query({:element?, RTC.elementOf(), compound_id})
      |> Enum.map(&Map.get(&1, :element))

    sub_compounds =
      graph
      |> RDF.Graph.query({:sub_compound?, RTC.subCompoundOf(), compound_id})
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

  @spec to_rdf(t) :: RDF.Graph.t()
  def to_rdf(%__MODULE__{} = compound) do
    compound_id = id(compound)

    graph =
      Enum.reduce(
        compound.elements,
        RDF.graph(name: compound_id, init: compound.annotations),
        &RDF.Graph.add(&2, &1, add_annotations: {RTC.elementOf(), compound_id})
      )

    Enum.reduce(compound.sub_compounds, graph, fn {sub_compound_id, sub_compound}, graph ->
      graph
      |> RDF.Graph.add({sub_compound_id, RTC.subCompoundOf(), compound_id})
      |> RDF.Graph.add(to_rdf(sub_compound))
    end)
  end

  @spec graph(t) :: RDF.Graph.t()
  def graph(%__MODULE__{} = compound) do
    RDF.graph(name: id(compound), init: elements(compound))
  end

  @spec elements(t) :: [element]
  def elements(%__MODULE__{} = compound) do
    compound
    |> element_set()
    |> MapSet.to_list()
  end

  @spec element_set(t) :: MapSet.t()
  def element_set(%__MODULE__{} = compound) do
    Enum.reduce(compound.sub_compounds, compound.elements, fn {_, sub_compound}, element_set ->
      MapSet.union(element_set, element_set(sub_compound))
    end)
  end

  @spec element?(t, coercible_element) :: boolean
  def element?(%__MODULE__{} = compound, element) do
    element = RDF.triple(element)

    element in compound.elements or
      Enum.any?(compound.sub_compounds, fn {_, sub_compound} ->
        element?(sub_compound, element)
      end)
  end

  @spec size(t) :: non_neg_integer
  def size(%__MODULE__{} = compound) do
    compound
    |> element_set()
    |> MapSet.size()
  end

  @spec add(t, coercible_element | [coercible_element]) :: t
  def add(compound, elements)

  def add(%__MODULE__{} = compound, elements) when is_list(elements) do
    Enum.reduce(elements, compound, &add(&2, &1))
  end

  def add(%__MODULE__{} = compound, %Description{} = description) do
    add(compound, Description.triples(description))
  end

  def add(%__MODULE__{} = compound, %RDF.Graph{} = graph) do
    add(compound, RDF.Graph.triples(graph))
  end

  def add(%__MODULE__{} = compound, element) do
    %__MODULE__{compound | elements: MapSet.put(compound.elements, RDF.triple(element))}
  end

  @spec delete(t, coercible_element | [coercible_element]) :: t
  def delete(compound, elements)

  def delete(%__MODULE__{} = compound, elements) when is_list(elements) do
    Enum.reduce(elements, compound, &delete(&2, &1))
  end

  def delete(%__MODULE__{} = compound, %Description{} = description) do
    delete(compound, Description.triples(description))
  end

  def delete(%__MODULE__{} = compound, %RDF.Graph{} = graph) do
    delete(compound, RDF.Graph.triples(graph))
  end

  def delete(%__MODULE__{} = compound, element) do
    element = RDF.triple(element)

    %__MODULE__{
      compound
      | elements: MapSet.delete(compound.elements, element),
        sub_compounds:
          Map.new(compound.sub_compounds, fn {id, sub_compound} ->
            {id, delete(sub_compound, element)}
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

    def member?(%Compound{} = compound, element), do: {:ok, Compound.element?(compound, element)}

    def slice(%Compound{} = compound),
      do: compound |> Compound.element_set() |> Enumerable.slice()

    def reduce(%Compound{} = compound, acc, fun),
      do: compound |> Compound.element_set() |> Enumerable.reduce(acc, fun)
  end
end
