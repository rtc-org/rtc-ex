defimpl RDF.Data.Source, for: RTC.Compound do
  alias RTC.Compound

  def structure_type(_), do: :graph

  def derive(%Compound{}, :description, opts) do
    case Keyword.fetch(opts, :subject) do
      {:ok, subject} -> {:ok, RDF.Description.new(subject)}
      :error -> {:error, :no_subject}
    end
  end

  def derive(
        %Compound{asserted: %{prefixes: prefixes, base_iri: base_iri}} = compound,
        :graph,
        opts
      ) do
    if Keyword.get(opts, :preserve_metadata, true) do
      name = Keyword.get(opts, :name, Compound.id(compound))
      {:ok, RDF.Graph.new(name: name, prefixes: prefixes, base_iri: base_iri)}
    else
      name = Keyword.get(opts, :name)
      {:ok, RDF.Graph.new(name: name)}
    end
  end

  def derive(%Compound{}, :dataset, _opts) do
    {:ok, RDF.Dataset.new()}
  end

  def reduce(%Compound{} = compound, acc, fun) do
    compound
    |> Compound.graph(assertion_mode: :all)
    |> RDF.Data.Source.reduce(acc, fun)
  end

  def description(%Compound{} = compound, subject) do
    case Compound.get(compound, subject) do
      nil -> :error
      description -> {:ok, description}
    end
  end

  def graph(%Compound{} = compound, graph_name) do
    if Compound.id(compound) == RDF.Statement.coerce_graph_name(graph_name) do
      {:ok, Compound.graph(compound)}
    else
      :error
    end
  end

  def subject(%Compound{}), do: nil

  def subjects(%Compound{} = compound) do
    {:ok, compound |> Compound.subjects() |> MapSet.to_list()}
  end

  def graph_name(%Compound{} = compound), do: Compound.id(compound)

  def graph_names(%Compound{} = compound), do: {:ok, [graph_name(compound)]}

  def statement_count(%Compound{} = compound), do: {:ok, Compound.triple_count(compound)}

  def description_count(%Compound{} = compound) do
    {:ok, compound |> Compound.subjects() |> MapSet.size()}
  end

  def graph_count(%Compound{}), do: {:ok, 1}

  def add(%Compound{} = compound, statements) do
    {:ok, Compound.add(compound, statements)}
  end

  def delete(%Compound{} = compound, statements) do
    {:ok, Compound.delete(compound, statements)}
  end
end
