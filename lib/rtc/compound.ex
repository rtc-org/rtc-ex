defmodule RTC.Compound do
  import RDF.Guards

  defstruct [
    # we have no explicit id field, since we're using the subject of the description for this
    :elements,
    :annotations
  ]

  @type t :: %__MODULE__{
          elements: any,
          annotations: RDF.Description.t()
        }

  @type element :: RDF.Triple.t()

  def id(%__MODULE__{} = compound), do: compound.annotations.subject

  def new(triples, compound_id, opts \\ [])

  def new(triples, compound_id, opts) do
    %__MODULE__{
      elements: new_elements(triples),
      annotations: new_annotation(compound_id, Keyword.get(opts, :annotations))
    }
  end

  def new_elements(elements) do
    elements
    |> normalize_elements()
    |> Map.new()
  end

  defp normalize_elements(elements) do
    Enum.map(elements, &normalize_element/1)
  end

  defp normalize_element(%__MODULE__{} = nested_compound),
    do: {id(nested_compound), nested_compound}

  defp normalize_element(triple) when is_triple(triple), do: {RDF.triple(triple), nil}

  defp new_annotation(compound_id, nil), do: RDF.description(compound_id)

  defp new_annotation(compound_id, description),
    do: RDF.description(compound_id, init: description)

  def from_rdf(%RDF.Graph{} = graph, compound_id), do: do_from_rdf(graph, compound_id, nil)

  defp do_from_rdf(graph, compound_id, parent_compound_id) do
    {elements, annotations} =
      if description = graph[compound_id] do
        if parent_compound_id do
          RDF.Description.delete(description, {RTC.elementOf(), parent_compound_id})
        else
          description
        end
        |> RDF.Description.pop(RTC.elements())
      else
        {[], nil}
      end

    element_ofs =
      graph
      |> RDF.Graph.query({:element?, RTC.elementOf(), compound_id})
      |> Enum.map(&Map.get(&1, :element))

    unless is_nil(annotations) and Enum.empty?(element_ofs) do
      (List.wrap(elements) ++ element_ofs)
      |> MapSet.new()
      |> Enum.map(fn
        triple when is_triple(triple) ->
          triple

        nested_compound ->
          do_from_rdf(graph, nested_compound, compound_id)
      end)
      |> new(compound_id, annotations: annotations)
    else
      new([], compound_id)
    end
  end

  def to_rdf(%__MODULE__{} = compound) do
    compound_id = id(compound)

    base_graph =
      RDF.graph(name: compound_id)
      |> RDF.Graph.add(compound.annotations)

    Enum.reduce(compound.elements, base_graph, fn
      {triple, nil}, graph when is_triple(triple) ->
        RDF.Graph.add(graph, triple, add_annotations: {RTC.elementOf(), compound_id})

      {sub_compound_id, sub_compound}, graph ->
        graph
        |> RDF.Graph.add({sub_compound_id, RTC.elementOf(), compound_id})
        |> RDF.Graph.add(to_rdf(sub_compound))
    end)
  end

  defdelegate graph(compound), to: __MODULE__, as: :to_rdf
end
