defmodule RTC.Compound do
  defstruct [
    # we have no explicit id field, since we're using the subject of the description for this
    :elements,
    :sub_compounds,
    :annotations
  ]

  @type t :: %__MODULE__{
          elements: any,
          sub_compounds: [t],
          annotations: RDF.Description.t()
        }

  @type element :: RDF.Triple.t()

  def id(%__MODULE__{} = compound), do: compound.annotations.subject

  def new(elements, compound_id, opts \\ []) do
    elements = for element <- elements, into: MapSet.new(), do: RDF.triple(element)
    sub_compounds = opts |> Keyword.get(:sub_compound) |> List.wrap() |> MapSet.new()

    %__MODULE__{
      elements: elements,
      sub_compounds: sub_compounds,
      annotations: new_annotation(compound_id, Keyword.get(opts, :annotations))
    }
  end

  defp new_annotation(compound_id, nil), do: RDF.description(compound_id)

  defp new_annotation(compound_id, description),
    do: RDF.description(compound_id, init: description)

  def from_rdf(%RDF.Graph{} = graph, compound_id), do: do_from_rdf(graph, compound_id, nil)

  defp do_from_rdf(graph, compound_id, parent_compound_id) do
    {elements, annotations} =
      if description = graph[compound_id] do
        if parent_compound_id do
          RDF.Description.delete(description, {RTC.subCompoundOf(), parent_compound_id})
        else
          description
        end
        |> RDF.Description.pop(RTC.elements())
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
      |> Enum.map(&do_from_rdf(graph, Map.get(&1, :sub_compound), compound_id))

    new(
      List.wrap(elements) ++ element_ofs,
      compound_id,
      sub_compound: sub_compounds,
      annotations: annotations
    )
  end

  def to_rdf(%__MODULE__{} = compound) do
    compound_id = id(compound)

    graph =
      Enum.reduce(
        compound.elements,
        RDF.graph(name: compound_id, init: compound.annotations),
        &RDF.Graph.add(&2, &1, add_annotations: {RTC.elementOf(), compound_id})
      )

    Enum.reduce(compound.sub_compounds, graph, fn sub_compound, graph ->
      graph
      |> RDF.Graph.add({id(sub_compound), RTC.subCompoundOf(), compound_id})
      |> RDF.Graph.add(to_rdf(sub_compound))
    end)
  end

  defdelegate graph(compound), to: __MODULE__, as: :to_rdf
end
