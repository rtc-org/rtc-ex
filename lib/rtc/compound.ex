defmodule RTC.Compound do
  @moduledoc """
  A struct representing an RDF Triple Compound.

  An RDF Triple Compound is a set of triples embedded in an RDF graph.

  You can create a such a set of triples either from scratch with the `new/1`
  function or you can load an already existing compound from an RDF graph with
  the `from_rdf/2` function.

  You can then use the various functions on this module to get its triples,
  sub-compounds and annotations or edit them.
  Whenever a function accepts triples they can be given as

  - coercible triples, i.e. 3-element tuples of elements which can coerced
    to RDF terms,
  - an `RDF.Description` or
  - an `RDF.Graph`

  Finally, you can get back the RDF form of the compound with `to_rdf/2`.
  If you only want a RDF graph of the contained triples (without the annotations),
  you can use the `graph/1` function.


  ## Auto-generated ids

  Various functions can be used in such a way, that the they will create compounds with
  a proper resource identifier implicitly. By default, they will create a random blank
  node, but the identifier creation behavior can be configured and customized via
  `RDF.Resource.Generator`s.
  For example, to create UUIDv4 URIs instead, you could use this configuration in your
  `config.exs`:

      config :rtc, :id,
        generator: RDF.IRI.UUID.Generator,
        uuid_version: 4,
        prefix: "http://example.com/ns/"

  See [the guide on resource generators](https://rdf-elixir.dev/rdf-ex/resource-generators.html)
  for more information and available generators.
  """

  alias RDF.{Statement, Triple, Description, Graph}

  @enforce_keys [:triples, :annotations]
  defstruct [
    # we have no explicit id field, since we're using the subject of the description for this
    triples: MapSet.new(),
    sub_compounds: %{},
    annotations: nil
  ]

  @type id :: RDF.Resource.t()
  @type coercible_id :: Statement.coercible_subject()

  @type t :: %__MODULE__{
          triples: MapSet.t(),
          sub_compounds: %{id => t()},
          annotations: Description.t()
        }

  @type triple :: Triple.t()
  @type coercible_triple :: Triple.coercible()
  @type coercible_triples ::
          [Triple.coercible()]
          | Graph.t()
          | Description.t()
          | Graph.t()
          | Description.t()
          | [coercible_triples()]

  @element_style Application.get_env(:rtc, :element_style, :element_of)

  @doc """
  Creates a new compound with the given set of triples.

  An id for the compound is automatically generated.
  If you want to define the id yourself, use `new/2` or `new/3`.

  When a list of triples is given which contains nested lists of triples,
  sub-compounds with auto-generated ids are generated and added for each
  of the nested lists.

  See the module documentation for information on auto-generated ids and the
  various ways to specify triples.
  """
  @spec new(coercible_triples()) :: t
  def new(triples), do: new(triples, [])

  @doc """
  Creates a new compound with the given set of triples and the given id.

  When a list of triples is given which contains nested lists of triples,
  sub-compounds with auto-generated ids are generated and added for each
  of the nested lists.

  If a keyword list is given as the second argument, an id is generated and
  delegated to `new/3`.

  See the module documentation for information on auto-generated ids and the
  various ways to specify triples.
  """
  @spec new(coercible_triples(), coercible_id() | keyword) :: t
  def new(triples, opts) when is_list(opts), do: new(triples, RTC.id(), opts)
  def new(triples, compound_id), do: new(triples, compound_id, [])

  @doc """
  Creates a new compound with the given set of triples and the given id.

  When a list of triples is given which contains nested lists of triples,
  sub-compounds with auto-generated ids are generated and added for each
  of the nested lists.
  Alternatively, the `sub_compound` keyword can be used to provide one
  or a list of sub-compounds to be added.

  See the module documentation for information on the various ways to specify triples.
  """
  @spec new(coercible_triples(), coercible_id(), keyword) :: t
  def new(triples, compound_id, opts) do
    {triples, sub_compounds} =
      Enum.reduce(triples, {MapSet.new(), MapSet.new()}, fn
        nested_triples, {triples, sub_compounds} when is_list(nested_triples) ->
          {triples, MapSet.put(sub_compounds, new(nested_triples))}

        triple, {triples, sub_compounds} ->
          {MapSet.put(triples, RDF.triple(triple)), sub_compounds}
      end)

    sub_compounds =
      opts
      |> Keyword.get(:sub_compound)
      |> List.wrap()
      |> ensure_all_compounds!()
      |> MapSet.new()
      |> MapSet.union(sub_compounds)

    %__MODULE__{
      triples: triples,
      sub_compounds: Map.new(sub_compounds, &{id(&1), &1}),
      annotations: new_annotation(compound_id, Keyword.get(opts, :annotations))
    }
  end

  defp ensure_all_compounds!(compounds, acc \\ [])

  defp ensure_all_compounds!([], acc), do: acc

  defp ensure_all_compounds!([%__MODULE__{} = compound | rest], acc),
    do: ensure_all_compounds!(rest, [compound | acc])

  defp ensure_all_compounds!([non_compound | _], _),
    do: raise(ArgumentError, "#{inspect(non_compound)} is not a compound")

  defp new_annotation(compound_id, nil), do: RDF.description(compound_id)

  defp new_annotation(compound_id, description),
    do: RDF.description(compound_id, init: description)

  @doc """
  Retrieves the compound with the given `compound_id` from a `RDF.Graph`.

  When no compound with the given `compound_id` can be found in the given
  `graph`, an empty compound is returned.
  """
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

  @doc """
  Creates an RDF-star graph of the given compound with all RTC annotations.

  The style for how the assignments of the triples are encoded can be specified
  with the `:element_style` keyword and one the following values:

  - `:element_of`: Uses the `rtc:elementOf` property to assign each of the triples
    individually to the compound. This has the benefit that in the Turtle-star
    serializations the [annotation syntax](https://w3c.github.io/rdf-star/cg-spec/2021-12-17.html#annotation-syntax)
    can be used, which doesn't require repeating the assertion and is therefore
    more compact.
  - `:elements`: Assigns the triples to the compound, by listing them as objects
    of the inverse `rtc:elements` property. This has the benefit that the actual
    assertion are not pervaded by annotations and the compound resource can be
    serialized in an isolated and self-contained manner.

  When no `:element_style` is specified, the `:element_of` style is used by default.
  You can however configure a different default style in your application with
  the following configuration on your `config.exs` files:

      config :rtc, :element_style, :element_of

  All remaining opts are passed-through to `RDF.Graph.new/1`, which means in particular:

  - you can set the graph name with the `:name` option
    (by default, the compound id is used as the graph name)
  - you can set the prefixes with the `:prefixes` options
    (by default, the `RDF.default_prefixes/0` together with the `rtc` prefix is used)

  """
  @spec to_rdf(t, keyword) :: Graph.t()
  def to_rdf(%__MODULE__{} = compound, opts \\ []) do
    element_style = Keyword.get(opts, :element_style, @element_style)

    graph_new_opts =
      opts
      |> Keyword.put_new(:name, id(compound))
      |> Keyword.put_new_lazy(:prefixes, fn -> RDF.default_prefixes(rtc: RTC.NS.RTC) end)

    graph =
      RDF.graph(graph_new_opts)
      |> elements_to_rdf(compound, element_style)

    Enum.reduce(compound.sub_compounds, graph, fn {sub_compound_id, sub_compound}, graph ->
      graph
      |> Graph.add({sub_compound_id, RTC.subCompoundOf(), id(compound)})
      |> Graph.add(to_rdf(sub_compound, opts))
    end)
  end

  defp elements_to_rdf(graph, compound, :element_of) do
    compound_id = id(compound)

    Enum.reduce(
      compound.triples,
      Graph.add(graph, compound.annotations),
      &Graph.add(&2, &1, add_annotations: {RTC.elementOf(), compound_id})
    )
  end

  defp elements_to_rdf(graph, compound, :elements) do
    triples = MapSet.to_list(compound.triples)

    graph
    |> Graph.add(triples)
    |> Graph.add(compound.annotations |> RTC.elements(triples))
  end

  @doc """
  Creates an RDF graph of the triples in the compound (incl. its sub-compounds) without the annotations.
  """
  @spec graph(t) :: Graph.t()
  def graph(%__MODULE__{} = compound) do
    RDF.graph(name: id(compound), init: triples(compound))
  end

  @doc """
  Returns the id of the given `compound`.
  """
  @spec id(t) :: id()
  def id(%__MODULE__{} = compound), do: compound.annotations.subject

  @doc """
  Sets a new id on the given `compound`.
  """
  @spec reset_id(t, id()) :: t()
  def reset_id(%__MODULE__{} = compound, id) do
    %{compound | annotations: Description.change_subject(compound.annotations, id)}
  end

  @doc """
  Returns a list of the triples in the given `compound`.
  """
  @spec triples(t) :: [triple]
  def triples(%__MODULE__{} = compound) do
    compound
    |> triple_set()
    |> MapSet.to_list()
  end

  @doc """
  Returns the set of triples in the given `compound`.
  """
  @spec triple_set(t) :: MapSet.t()
  def triple_set(%__MODULE__{} = compound) do
    Enum.reduce(compound.sub_compounds, compound.triples, fn {_, sub_compound}, triple_set ->
      MapSet.union(triple_set, triple_set(sub_compound))
    end)
  end

  @doc """
  Returns whether the given `triple` is an element of the given `compound`.
  """
  @spec element?(t, coercible_triple) :: boolean
  def element?(%__MODULE__{} = compound, triple) do
    triple = RDF.triple(triple)

    triple in compound.triples or
      Enum.any?(compound.sub_compounds, fn {_, sub_compound} ->
        element?(sub_compound, triple)
      end)
  end

  @doc """
  Returns the number of triple in the given `compound`.
  """
  @spec size(t) :: non_neg_integer
  def size(%__MODULE__{} = compound) do
    compound
    |> triple_set()
    |> MapSet.size()
  end

  @doc """
  Adds triples to the given `compound`.

  See the module documentation for the different ways to specify triples.
  """
  @spec add(t, coercible_triple() | coercible_triples) :: t
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

  @doc """
  Deletes triples from the given `compound`.

  See the module documentation for the different ways to specify triples.

  If a triple occurs in multiple sub-compounds, it gets deleted from all of them.
  """
  @spec delete(t, coercible_triple | coercible_triples) :: t
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

  @doc """
  Returns a list of the sub-compounds of the given `compound`.
  """
  @spec sub_compounds(t) :: [t]
  def sub_compounds(%__MODULE__{} = compound), do: Map.values(compound.sub_compounds)

  @doc """
  Adds a sub-compound to the given `compound`.

  If a sub-compound with the same id already exists, it gets overwritten.

  When just triples are passed instead of compound, a compound with an
  auto-generated id is created implicitly.
  """
  @spec put_sub_compound(t, t | coercible_triples) :: t
  def put_sub_compound(compound, sub_compounds)

  def put_sub_compound(%__MODULE__{} = compound, %__MODULE__{} = sub_compound) do
    %__MODULE__{
      compound
      | sub_compounds: Map.put(compound.sub_compounds, id(sub_compound), sub_compound)
    }
  end

  def put_sub_compound(compound, triples), do: put_sub_compound(compound, new(triples))

  @doc """
  Deletes a sub-compound to the given `compound`.

  The `sub_compound` to be deleted can be specified by id or given directly.
  Note however, that the elements of the sub-compound to be deleted are not
  taken into consideration, just its id is used to address the sub-compound
  to be deleted.
  """
  @spec delete_sub_compound(t, t | id) :: t
  def delete_sub_compound(compound, sub_compound)

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

  @doc """
  Returns the description of the given `compound`.
  """
  @spec annotations(t) :: Description.t()
  def annotations(%__MODULE__{} = compound), do: compound.annotations

  @doc """
  Adds statements to the description of the given `compound`.
  """
  @spec add_annotations(t, Description.input()) :: t
  def add_annotations(%__MODULE__{} = compound, annotations) do
    %__MODULE__{compound | annotations: Description.add(compound.annotations, annotations)}
  end

  @doc """
  Adds statements to the description of the given `compound`,
  overwriting any previous statements with the given properties.
  """
  @spec put_annotations(t, Description.input()) :: t
  def put_annotations(%__MODULE__{} = compound, annotations) do
    %__MODULE__{compound | annotations: Description.put(compound.annotations, annotations)}
  end

  @doc """
  Deletes statements from the description of the given `compound`.

  Statements not part of the description are simply ignored.
  """
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
