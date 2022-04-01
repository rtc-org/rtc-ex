defmodule RTC.Compound do
  @moduledoc """
  A struct representing an RDF Triple Compound.

  An RDF Triple Compound is a set of triples embedded in an RDF graph.

  You can create a such a set of triples either from scratch with the `new/1`
  function or you can load an already existing compound from an RDF graph with
  the `from_rdf/2` function.

  You can then use the various functions on this module to get its triples,
  sub-compounds, super-compounds and annotations or edit them.

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

  import RDF.Guards

  # we have no explicit id field, since we're using the subject of the annotations for this
  @enforce_keys [:graph, :annotations]
  defstruct graph: nil,
            sub_compounds: %{},
            super_compounds: %{},
            annotations: nil

  @type id :: RDF.Resource.t()
  @type coercible_id :: Statement.coercible_subject()

  @type t :: %__MODULE__{
          graph: Graph.t(),
          sub_compounds: %{id => t()},
          super_compounds: %{id => Description.t()},
          annotations: Description.t()
        }

  @element_style Application.get_env(:rtc, :element_style, :element_of)

  @doc """
  Creates a new compound.

  An id for the compound is automatically generated.
  See the module documentation for information on auto-generated ids.
  """
  @spec new :: t
  def new, do: new([])

  @doc """
  Creates a new compound with the given set of triples.

  An id for the compound is automatically generated.
  If you want to define the id yourself, use `new/2` or `new/3`.

  Triples can be provided in any form accepted by `RDF.Graph.new/2`.
  When a list of triples is given which contains nested lists of triples,
  sub-compounds with auto-generated ids are generated and added for each
  of the nested lists.

  See the module documentation for information on auto-generated ids.
  """
  @spec new(Graph.input()) :: t
  def new(triples), do: new(triples, [])

  @doc """
  Creates a new compound with the given set of triples and the given id.

  Triples can be provided in any form accepted by `RDF.Graph.new/2`.
  When a list of triples is given which contains nested lists of triples,
  sub-compounds with auto-generated ids are generated and added for each
  of the nested lists.

  If a keyword list is given as the second argument, an id is generated and
  delegated to `new/3`.
  See the module documentation for information on auto-generated ids.
  """
  @spec new(Graph.input(), coercible_id() | keyword) :: t
  def new(triples, opts) when is_list(opts), do: new(triples, RTC.id(), opts)
  def new(triples, compound_id), do: new(triples, compound_id, [])

  @doc """
  Creates a new compound with the given set of triples and the given id.

  Triples can be provided in any form accepted by `RDF.Graph.new/2`.
  When a list of triples is given which contains nested lists of triples,
  sub-compounds with auto-generated ids are generated and added for each
  of the nested lists.
  Alternatively, the `sub_compound` keyword can be used to provide one
  or a list of sub-compounds to be added.
  """
  @spec new(Graph.input(), coercible_id(), keyword) :: t
  def new(triples, compound_id, opts) when is_list(triples) do
    {graph, sub_compounds} =
      Enum.reduce(triples, {Graph.new(), []}, fn
        nested_triples, {graph, sub_compounds} when is_list(nested_triples) ->
          {graph, [new(nested_triples) | sub_compounds]}

        triple, {graph, sub_compounds} ->
          {Graph.add(graph, triple), sub_compounds}
      end)

    opts = Keyword.update(opts, :sub_compounds, sub_compounds, &(sub_compounds ++ List.wrap(&1)))

    new(graph, compound_id, opts)
  end

  def new(triples, compound_id, opts) do
    sub_compounds =
      opts
      |> Keyword.get(:sub_compounds)
      |> List.wrap()
      |> ensure_all_compounds!()

    %__MODULE__{
      graph:
        Graph.new(triples, name: compound_id, prefixes: RDF.default_prefixes(rtc: RTC.NS.RTC)),
      sub_compounds: Map.new(sub_compounds, &{id(&1), &1}),
      annotations: new_annotation(compound_id, Keyword.get(opts, :annotations))
    }
    |> put_super_compound(Keyword.get(opts, :super_compounds, []))
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
  Returns the id of the given `compound`.
  """
  @spec id(t) :: id()
  def id(%__MODULE__{} = compound), do: compound.annotations.subject

  defdelegate name(compound), to: __MODULE__, as: :id

  @doc """
  Sets a new id on the given `compound`.
  """
  @spec reset_id(t, id()) :: t()
  def reset_id(%__MODULE__{} = compound, id) do
    %{
      compound
      | graph: Graph.change_name(compound.graph, id),
        annotations: Description.change_subject(compound.annotations, id)
    }
  end

  defdelegate change_name(compound, name), to: __MODULE__, as: :reset_id

  @doc """
  Retrieves the compound with the given `compound_id` from a `RDF.Graph`.

  When no compound with the given `compound_id` can be found in the given
  `graph`, an empty compound is returned.
  """
  @spec from_rdf(Graph.t(), coercible_id()) :: t
  def from_rdf(graph, compound_id) when maybe_ns_term(compound_id) or is_binary(compound_id),
    do: from_rdf(graph, RDF.iri(compound_id))

  def from_rdf(%Graph{} = graph, compound_id) do
    do_from_rdf(graph, compound_id, [])
  end

  defp do_from_rdf(graph, compound_id, super_compound_ids) do
    if compound_id in super_compound_ids do
      raise("circle in sub-compound #{compound_id}")
    end

    {elements, annotations} =
      graph
      |> Graph.get(compound_id, Description.new(compound_id))
      |> Description.pop(RTC.elements())

    element_ofs =
      graph
      |> Graph.query({:triple?, RTC.elementOf(), compound_id})
      |> Enum.map(&Map.get(&1, :triple))

    {super_compounds, annotations} = Description.pop(annotations, RTC.subCompoundOf())

    super_compounds =
      super_compounds_from_graph(graph, List.wrap(super_compounds) -- super_compound_ids)

    sub_compounds =
      graph
      |> Graph.query({:sub_compound?, RTC.subCompoundOf(), compound_id})
      |> Enum.map(
        &do_from_rdf(graph, Map.get(&1, :sub_compound), [compound_id | super_compound_ids])
      )

    new(
      List.wrap(elements) ++ element_ofs,
      compound_id,
      sub_compounds: sub_compounds,
      super_compounds: super_compounds,
      annotations: annotations
    )
  end

  defp super_compounds_from_graph(graph, super_compound_ids) when is_list(super_compound_ids) do
    Enum.map(super_compound_ids, &super_compounds_from_graph(graph, &1))
  end

  defp super_compounds_from_graph(graph, super_compound_id) do
    do_super_compounds_from_graph(
      graph,
      super_compound_id,
      Description.new(super_compound_id),
      []
    )
  end

  defp do_super_compounds_from_graph(graph, super_compound_id, super_compound, super_compound_ids) do
    if super_compound_id in super_compound_ids do
      raise("circle in sub-compound #{super_compound_id}")
    end

    super_compound_ids = [super_compound_id | super_compound_ids]

    {new_super_compound_ids, annotations} =
      if description = graph[super_compound_id] do
        description
        |> Description.delete_predicates(RTC.elements())
        |> Description.pop(RTC.subCompoundOf())
      else
        {[], []}
      end

    annotations = Description.add(super_compound, annotations)

    if new_super_compound_ids do
      Enum.reduce(
        new_super_compound_ids,
        annotations,
        &do_super_compounds_from_graph(graph, &1, &2, super_compound_ids)
      )
    else
      annotations
    end
  end

  if Code.ensure_loaded?(SPARQL.Client) do
    @doc """
    Retrieves the compound with the given `compound_id` from a SPARQL endpoint.

    This function is only available when the `sparql_client` dependency is
    added in your `Mixfile`.

    When no compound with the given `compound_id` can be found in the given
    `graph`, an empty compound is returned.
    """
    @spec from_sparql(String.t(), id()) :: {:ok, t()}
    defdelegate from_sparql(endpoint, compound_id), to: RTC.SPARQL, as: :from_endpoint

    @doc """
    Retrieves the compound with the given `compound_id` from a SPARQL endpoint.

    This function is only available when the `sparql_client` dependency is
    added in your `Mixfile`.

    The `opts` are used as the options for the `CONSTRUCT` query call by the
    `SPARQL.Client` against the endpoint. See `SPARQL.Client.construct/3` for
    available options.

    When no compound with the given `compound_id` can be found in the given
    `graph`, an empty compound is returned.
    """
    @spec from_sparql(String.t(), id(), keyword) :: {:ok, t()} | {:error, any}
    defdelegate from_sparql(endpoint, compound_id, opts), to: RTC.SPARQL, as: :from_endpoint

    @doc """
    Retrieves the compound with the given `compound_id` from a SPARQL endpoint.

    As opposed to `from_sparql/3` this function returns the result directly and
    raises an error when the SPARQL client call fails.
    """
    @spec from_sparql!(String.t(), id(), keyword) :: t()
    def from_sparql!(endpoint, compound_id, opts \\ []) do
      case from_sparql(endpoint, compound_id, opts) do
        {:ok, results} -> results
        {:error, error} -> raise error
      end
    end
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

  The following options can used to customize the returned graph:

  - `:name`: the name of the graph to be created
    (by default, the compound id is used as the graph name)
  - `:prefixes`: some prefix mappings which should be added the graph
    and will be used for example when serializing in a format with prefix support
    (the `RDF.default_prefixes/0` together with the `rtc` prefix are already added)
  - `:base_iri`: a base IRI which should be stored alongside the graph
    and will be used for example when serializing in a format with base IRI support

  """
  @spec to_rdf(t, keyword) :: Graph.t()
  def to_rdf(%__MODULE__{} = compound, opts \\ []) do
    annotated_graph =
      compound.graph
      |> Graph.change_name(Keyword.get(opts, :name, compound.graph.name))
      |> Graph.set_base_iri(Keyword.get(opts, :base_iri, compound.graph.base_iri))
      |> Graph.add_prefixes(Keyword.get(opts, :prefixes, []))
      |> annotate(compound, Keyword.get(opts, :element_style, @element_style))
      |> Graph.add({id(compound), RTC.subCompoundOf(), super_compounds(compound)})

    Enum.reduce(compound.sub_compounds, annotated_graph, fn
      {sub_compound_id, sub_compound}, annotated_graph ->
        annotated_graph
        |> Graph.add({sub_compound_id, RTC.subCompoundOf(), id(compound)})
        |> Graph.add(to_rdf(sub_compound, opts))
    end)
  end

  defp annotate(graph, compound, :element_of) do
    graph
    |> Graph.add(compound.annotations)
    |> Graph.add_annotations(compound.graph, {RTC.elementOf(), id(compound)})
  end

  defp annotate(graph, compound, :elements) do
    Graph.add(graph, compound.annotations |> RTC.elements(Graph.triples(compound.graph)))
  end

  @doc """
  Returns an RDF graph of the triples in the compound (incl. its sub-compounds) without the annotations.
  """
  @spec graph(t) :: Graph.t()
  def graph(%__MODULE__{} = compound) do
    Enum.reduce(compound.sub_compounds, compound.graph, fn
      {_, sub_compound}, graph -> Graph.add(graph, graph(sub_compound))
    end)
  end

  @doc """
  Returns a list of the triples in the given `compound`.
  """
  @spec triples(t) :: [Triple.t()]
  def triples(%__MODULE__{} = compound) do
    compound
    |> graph()
    |> Graph.triples()
  end

  defdelegate statements(compound), to: __MODULE__, as: :triples

  @doc """
  Returns the number of triples in the given `compound`.
  """
  @spec triple_count(t) :: non_neg_integer
  def triple_count(%__MODULE__{} = compound) do
    compound
    |> graph()
    |> Graph.triple_count()
  end

  defdelegate statement_count(compound), to: __MODULE__, as: :statement_count

  @doc """
  Returns a list of the `RDF.Description`s in the given `compound`.
  """
  @spec descriptions(t) :: [Description.t()]
  def descriptions(%__MODULE__{} = compound) do
    compound
    |> graph()
    |> Graph.descriptions()
  end

  @doc """
  Returns whether the given triples are an element of the given `compound` or any of its sub-compounds.
  """
  @spec include?(t, Graph.input()) :: boolean
  def include?(%__MODULE__{} = compound, input) do
    Graph.include?(compound.graph, input) or
      Enum.any?(compound.sub_compounds, fn {_, sub_compound} ->
        include?(sub_compound, input)
      end)
  end

  @doc """
  Returns whether the given `compound` or any of its sub-compounds contains triples about the given subject.

  ## Examples

        iex> RTC.Compound.new([{EX.S1, EX.p1, EX.O1}]) |> RTC.Compound.describes?(EX.S1)
        true
        iex> RTC.Compound.new([{EX.S1, EX.p1, EX.O1}]) |> RTC.Compound.describes?(EX.S2)
        false

  """
  @spec describes?(t, Statement.coercible_subject()) :: boolean
  def describes?(%__MODULE__{} = compound, subject) do
    Graph.describes?(compound.graph, subject) or
      Enum.any?(compound.sub_compounds, fn {_, sub_compound} ->
        describes?(sub_compound, subject)
      end)
  end

  @doc """
  Returns the description of the given subject.

  When the subject can not be found an empty description is returned.

  ## Examples

      iex> RTC.Compound.new([{EX.S1, EX.P1, EX.O1}, {EX.S2, EX.P2, EX.O2}])
      ...> |> RTC.Compound.description(EX.S1)
      RDF.Description.new(EX.S1, init: {EX.P1, EX.O1})

      iex> RTC.Compound.new([{EX.S, EX.P1, EX.O1}], sub_compounds: RTC.Compound.new([{EX.S, EX.P2, EX.O2}]))
      ...> |> RTC.Compound.description(EX.S)
      RDF.Description.new(EX.S, init: [{EX.P1, EX.O1}, {EX.P2, EX.O2}])

  """
  @spec description(t, Statement.coercible_subject()) :: Description.t() | nil
  def description(%__MODULE__{} = compound, subject) do
    Enum.reduce(
      compound.sub_compounds,
      Graph.get(compound.graph, subject, Description.new(subject)),
      fn {_, sub_compound}, description ->
        Description.add(description, description(sub_compound, subject))
      end
    )
  end

  @doc """
  Adds triples to the given `compound`.

  Triples can be provided in any form accepted by `RDF.Graph.add/2`.
  """
  @spec add(t, Graph.input()) :: t
  def add(compound, triples) do
    update_graph(compound, &Graph.add(&1, triples))
  end

  defp update_graph(compound, fun) do
    %__MODULE__{compound | graph: fun.(compound.graph)}
  end

  @doc """
  Deletes triples from the given `compound`.

  Triples can be provided in any form accepted by `RDF.Graph.delete/2`.

  If a triple occurs in one or more sub-compounds, it gets deleted from all of them.
  """
  @spec delete(t, Graph.input()) :: t
  def delete(%__MODULE__{} = compound, triples) do
    %__MODULE__{
      compound
      | graph: Graph.delete(compound.graph, triples),
        sub_compounds:
          Map.new(compound.sub_compounds, fn {id, sub_compound} ->
            {id, delete(sub_compound, triples)}
          end)
    }
  end

  @doc """
  Deletes all triples with the given `subjects` from the given `compound`.

  If a triple occurs in one or more sub-compounds, it gets deleted from all of them.
  """
  @spec delete_descriptions(t, Statement.coercible_subject() | [Statement.coercible_subject()]) ::
          t
  def delete_descriptions(%__MODULE__{} = compound, subjects) do
    %__MODULE__{
      compound
      | graph: Graph.delete_descriptions(compound.graph, subjects),
        sub_compounds:
          Map.new(compound.sub_compounds, fn {id, sub_compound} ->
            {id, delete_descriptions(sub_compound, subjects)}
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
  @spec put_sub_compound(t, t | Graph.input()) :: t
  def put_sub_compound(compound, sub_compounds)

  def put_sub_compound(%__MODULE__{} = compound, %__MODULE__{} = sub_compound) do
    %__MODULE__{
      compound
      | sub_compounds: Map.put(compound.sub_compounds, id(sub_compound), sub_compound)
    }
  end

  def put_sub_compound(compound, triples), do: put_sub_compound(compound, new(triples))

  @doc """
  Deletes a sub-compound from the given `compound`.

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
  Returns a list of the ids of the super-compounds of the given `compound`.

  ## Example

      iex> RTC.Compound.new({EX.S, EX.p, EX.O}, EX.Compound, super_compounds: EX.SuperCompound)
      ...> |> RTC.Compound.super_compounds()
      [RDF.iri(EX.SuperCompound)]

  """
  @spec super_compounds(t) :: [id]
  def super_compounds(%__MODULE__{} = compound), do: Map.keys(compound.super_compounds)

  @doc """
  Adds a super-compound to the given `compound`.

  The super-compound can be given as a compound identifier, a `RDF.Description` or a
  `RTC.Compound`. In case of a compound  only its id and annotations are relevant, which
  will be returned when inherited annotations are requested. A `RDF.Description` is
  interpreted as the annotations of the super-compound.

  > #### Warning {: .warning}
  >
  > The annotations are just used for the purpose of showing inherited annotations.
  > They won't be rendered in `to_rdf/2`. So, you can't use this function to change the
  > annotations of super-compounds.
  > You'll have to load a super-compound with `from_rdf/2` and change the annotations on
  > this compound.

  If a super-compound with the same id already exists, it gets overwritten.

  """
  @spec put_super_compound(t, id | t | Description.t() | [id | t | Description.t()]) :: t
  def put_super_compound(compound, sub_compounds)

  def put_super_compound(%__MODULE__{} = compound, %Description{} = description) do
    %__MODULE__{
      compound
      | super_compounds: Map.put(compound.super_compounds, description.subject, description)
    }
  end

  def put_super_compound(compound, super_compounds) when is_list(super_compounds),
    do: Enum.reduce(super_compounds, compound, &put_super_compound(&2, &1))

  def put_super_compound(compound, %__MODULE__{annotations: annotations}),
    do: put_super_compound(compound, annotations)

  def put_super_compound(compound, super_compound_id),
    do: put_super_compound(compound, Description.new(super_compound_id))

  @doc """
  Deletes a super-compound from the given `compound`.

  The `super_compound` to be deleted can be specified by id or given directly.
  Note however, that the elements of the super-compound to be deleted are not
  taken into consideration, just its id is used to address the super-compound
  to be deleted.
  """
  @spec delete_super_compound(t, t | id) :: t
  def delete_super_compound(compound, super_compound)

  def delete_super_compound(%__MODULE__{} = compound, %__MODULE__{} = super_compound) do
    delete_super_compound(compound, id(super_compound))
  end

  def delete_super_compound(%__MODULE__{} = compound, super_compound_id) do
    %__MODULE__{
      compound
      | super_compounds:
          Map.delete(compound.super_compounds, Statement.coerce_subject(super_compound_id))
    }
  end

  @doc """
  Returns the annotations of the given `compound`.

  By default only the direct annotations of the given is returned.
  With the keyword option `:inherited` set to the value `true`,
  the annotations of all super-compounds can be included.
  """
  @spec annotations(t) :: Description.t()
  def annotations(%__MODULE__{} = compound, opts \\ []) do
    if Keyword.get(opts, :inherited, false) do
      Enum.reduce(compound.super_compounds, compound.annotations, fn
        {_, inherited}, annotations -> Description.add(annotations, inherited)
      end)
    else
      compound.annotations
    end
  end

  @doc """
  Returns the merged annotations of all super-compound the given `compound`.
  """
  @spec inherited_annotations(t) :: Description.t()
  def inherited_annotations(%__MODULE__{} = compound) do
    Enum.reduce(compound.super_compounds, RDF.description(id(compound)), fn
      {_, inherited}, annotations -> Description.add(annotations, inherited)
    end)
  end

  @doc """
  Returns the annotations of the given super-compound of `compound`.
  """
  @spec inherited_annotations(t, coercible_id) :: Description.t()
  def inherited_annotations(compound, super_compound_id)

  def inherited_annotations(compound, super_compound_id)
      when maybe_ns_term(super_compound_id) or is_binary(super_compound_id),
      do: inherited_annotations(compound, RDF.iri(super_compound_id))

  def inherited_annotations(%__MODULE__{} = compound, super_compound_id),
    do: compound.super_compounds[super_compound_id]

  @doc """
  Adds statements to the annotations of the given `compound`.
  """
  @spec add_annotations(t, Description.input()) :: t
  def add_annotations(%__MODULE__{} = compound, annotations) do
    %__MODULE__{compound | annotations: Description.add(compound.annotations, annotations)}
  end

  @doc """
  Adds statements to the annotations of the given `compound`,
  overwriting any previous statements with the given properties.
  """
  @spec put_annotations(t, Description.input()) :: t
  def put_annotations(%__MODULE__{} = compound, annotations) do
    %__MODULE__{compound | annotations: Description.put(compound.annotations, annotations)}
  end

  @doc """
  Deletes statements from the annotations of the given `compound`.

  Statements not part of the annotations are simply ignored.
  """
  @spec delete_annotations(t, Description.input()) :: t
  def delete_annotations(%__MODULE__{} = compound, annotations) do
    %__MODULE__{compound | annotations: Description.delete(compound.annotations, annotations)}
  end

  defimpl Enumerable do
    alias RTC.Compound

    def count(%Compound{} = compound), do: {:ok, Compound.triple_count(compound)}

    def member?(%Compound{} = compound, triple), do: {:ok, Compound.include?(compound, triple)}

    def slice(%Compound{} = compound),
      do: compound |> Compound.graph() |> Enumerable.slice()

    def reduce(%Compound{} = compound, acc, fun),
      do: compound |> Compound.graph() |> Enumerable.reduce(acc, fun)
  end
end
