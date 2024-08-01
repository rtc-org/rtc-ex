defmodule RTC.Compound do
  @moduledoc """
  A struct representing an RDF Triple Compound.

  An RDF Triple Compound is a set of triples embedded in an RDF graph.

  You can create such a set of triples either from scratch with the `new/1`
  function, or you can load an already existing compound from an RDF graph with
  the `from_rdf/2` function.

  You can then use the various functions on this module to get its triples,
  sub-compounds, super-compounds and annotations or edit them.

  Finally, you can get back the RDF form of the compound with `to_rdf/2`.
  If you only want an RDF graph of the contained triples (without the annotations),
  you can use the `graph/1` function.


  ## Asserted and unasserted triples

  A compound can contain both asserted and unasserted triples. When creating a
  compound with initial triples with the `new/3` function or adding triples with
  `add/3` you can specify in which assertion mode the given triples should be
  interpreted with the `:assertion_mode` option and one of the values `:asserted`
  or `:unasserted`. By default, `:asserted` is assumed, but you can configure the
  default value in your application with the following configuration
  on your `config.exs` files:

      config :rtc, :assertion_mode, :unasserted

  All query functions operating over the set of triples and the `delete/3`
  and `delete_description/3` functions also support an `:assertion_mode` which
  defines which triples should be considered with the following supported
  values:

  - `:all` (default): consider both asserted and unasserted triples
  - `:asserted`: only consider asserted triples
  - `:unasserted`: only consider unasserted triples


  ## Auto-generated ids

  Various functions can be used in such a way, that they will create compounds with
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

  alias RDF.{Statement, Triple, Description, Graph, BlankNode, PrefixMap}

  # we have no explicit id field, since we're using the subject of the annotations for this
  @enforce_keys [:asserted, :annotations]
  defstruct annotations: nil,
            asserted: nil,
            unasserted: Graph.new(),
            super_compounds: %{},
            sub_compounds: %{}

  @type id :: RDF.Resource.t()
  @type coercible_id :: Statement.coercible_subject()

  @type t :: %__MODULE__{
          annotations: Description.t(),
          asserted: Graph.t(),
          unasserted: Graph.t(),
          super_compounds: %{id => Description.t()},
          sub_compounds: %{id => t()}
        }

  @doc """
  Returns the default value of the `:element_style` option of `to_rdf/2`.

  This value can be configured in your application with the following configuration
  on your `config.exs` files:

      config :rtc, :element_style, :elements

  When no `:element_style` is specified, the `:element_of` style is used by default.
  """
  def default_element_style, do: Application.get_env(:rtc, :element_style, :element_of)

  @doc """
  Returns the default value of the `:assertion_mode` option.

  This value can be configured in your application with the following configuration
  on your `config.exs` files:

      config :rtc, :assertion_mode, :unasserted

  When no `:assertion_mode` is specified, the `:asserted` style is used by default.
  """
  def default_assertion_mode, do: Application.get_env(:rtc, :assertion_mode, :asserted)

  @doc """
  Creates a empty compound with an automatically generated id.

  See the module documentation for information on auto-generated ids.
  """
  @spec new :: t
  def new, do: new([])

  @doc """
  Creates a compound with the given set of triples and an automatically generated id.

  If you want to define the id yourself, use `new/2` or `new/3`.
  See the module documentation for information on auto-generated ids.

  Triples can be provided in any form accepted by `RDF.Graph.new/2`.
  When a list of triples is given which contains nested lists of triples,
  sub-compounds with auto-generated ids are generated and added for each
  of the nested lists.
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
  delegated to `new/3`. See `new/3` for a description of the available options.
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
  Alternatively, the `sub_compounds` option can be used to provide one or
  a list of sub-compounds to be added.

  Available options:

  - `:assertion_mode`: the assertion mode to be used for the `triples` to be added
    (see module documentation section on "Asserted and unasserted triples")
  - `:name`: the name of the graph which gets returned by `graph/1` and
    `to_rdf/2` (by default, the compound id is used as the graph name or `nil`
    when the compound id is a blank node)
  - `:prefixes`: the prefix mappings which should used by default when an `RDF.Graph`
    is produced from this compound, e.g. with `to_rdf/2`, `graph/2` etc.
    Note, that a `rtc` prefix for the RTC vocabulary is added by `to_rdf/2` in
    any case, so it usually not required to be defined here, unless you want it
    to be added also to the graphs with the element triples like `graph/2`.
  - `:base_iri`: a base IRI which should be stored alongside the graph
    and will be used for example when serializing in a format with base IRI support
  - `:annotations`: allows to set the initial annotations of the compound
     as an `RDF.Description` or anything an `RDF.Description` can be built
     from. The subject in the input is ignored, so that the `RDF.Description`
     of any resource can be used as a blueprint.
  - `:sub_compounds`: a single sub-compound or multiple sub-compounds as a list
  - `:super_compounds`: a single compound id of a super-compound or multiple
    compound ids as a list. One or multiple compounds or `RDF.Description`s of the
    annotations can be provided also, in which case only the ids and the annotations
    are stored.

  """
  @spec new(Graph.input(), coercible_id(), keyword) :: t
  def new(triples, compound_id, opts) do
    assertion_mode = assertion_mode(opts)
    sub_compounds = normalize_sub_compounds(opts)
    {triples, sub_compounds} = decompose_nested_triples(triples, sub_compounds, assertion_mode)

    {asserted, unasserted} =
      case assertion_mode do
        :asserted -> {triples, []}
        :unasserted -> {[], triples}
      end

    %__MODULE__{
      asserted: init_graph(asserted, compound_id, opts),
      unasserted: Graph.new(unasserted),
      sub_compounds: Map.new(sub_compounds, &{id(&1), &1}),
      annotations: new_annotation(compound_id, Keyword.get(opts, :annotations))
    }
    |> put_super_compound(Keyword.get(opts, :super_compounds, []))
  end

  defp normalize_sub_compounds(opts) do
    opts
    |> Keyword.get(:sub_compounds)
    |> List.wrap()
    |> ensure_all_compounds!()
  end

  defp ensure_all_compounds!(compounds, acc \\ [])

  defp ensure_all_compounds!([], acc), do: acc

  defp ensure_all_compounds!([%__MODULE__{} = compound | rest], acc),
    do: ensure_all_compounds!(rest, [compound | acc])

  defp ensure_all_compounds!([non_compound | _], _),
    do: raise(ArgumentError, "#{inspect(non_compound)} is not a compound")

  defp decompose_nested_triples(triples, sub_compounds, assertion_mode) when is_list(triples) do
    Enum.reduce(triples, {Graph.new(), List.wrap(sub_compounds)}, fn
      nested_triples, {graph, sub_compounds} when is_list(nested_triples) ->
        {graph, [new(nested_triples, assertion_mode: assertion_mode) | sub_compounds]}

      triple, {graph, sub_compounds} ->
        {Graph.add(graph, triple), sub_compounds}
    end)
  end

  defp decompose_nested_triples(triples, sub_compounds, _),
    do: {Graph.new(triples), sub_compounds}

  defp init_graph(triples, compound_id, opts) do
    opts = Keyword.put_new(opts, :name, graph_name(compound_id))

    Graph.new(triples, opts)
  end

  defp graph_name(%BlankNode{}), do: nil
  defp graph_name(id), do: id

  defp assertion_mode(opts) do
    case Keyword.get(opts, :assertion_mode, default_assertion_mode()) do
      :asserted -> :asserted
      :unasserted -> :unasserted
      v -> raise ArgumentError, "unexpected value for :assertion_mode opt: #{inspect(v)}"
    end
  end

  defp assertion_mode_with_all(opts) do
    case Keyword.get(opts, :assertion_mode, :all) do
      :all -> :all
      :asserted -> :asserted
      :unasserted -> :unasserted
      v -> raise ArgumentError, "unexpected value for :assertion_mode opt: #{inspect(v)}"
    end
  end

  defp new_annotation(compound_id, nil), do: RDF.description(compound_id)

  defp new_annotation(compound_id, description),
    do: RDF.description(compound_id, init: description)

  @doc """
  Returns the id of the given `compound`.
  """
  @spec id(t) :: id()
  def id(%__MODULE__{} = compound), do: compound.annotations.subject

  # for compatibility with RDF.Graph API
  defdelegate name(compound), to: __MODULE__, as: :id

  @doc """
  Sets a new id on the given `compound`.
  """
  @spec reset_id(t, coercible_id()) :: t()
  def reset_id(%__MODULE__{} = compound, id) when not is_nil(id) do
    %{
      compound
      | asserted: Graph.change_name(compound.asserted, graph_name(id)),
        annotations: Description.change_subject(compound.annotations, id)
    }
  end

  # for compatibility with the RDF.Graph API
  defdelegate change_name(compound, name), to: __MODULE__, as: :reset_id

  @doc """
  Sets a new graph name on the given `compound` that will be used in serializations to RDF.
  """
  @spec change_graph_name(t, coercible_id()) :: t()
  def change_graph_name(compound, name) do
    %{compound | asserted: Graph.change_name(compound.asserted, graph_name(name))}
  end

  @doc """
  Retrieves the compound with the given `compound_id` from a `RDF.Graph`.

  When no compound with the given `compound_id` can be found in the given
  `graph`, an empty compound is returned.
  """
  @spec from_rdf(Graph.t(), coercible_id()) :: t
  def from_rdf(%Graph{} = graph, compound_id) do
    do_from_rdf(graph, Statement.coerce_subject(compound_id), [])
  end

  defp do_from_rdf(graph, compound_id, super_compound_ids) do
    if compound_id in super_compound_ids do
      raise("circle in sub-compound #{compound_id}")
    end

    {elements, annotations} =
      graph
      |> Graph.description(compound_id)
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

    {asserted, unasserted} =
      Enum.split_with(List.wrap(elements) ++ element_ofs, &Graph.include?(graph, &1))

    asserted
    |> new(compound_id,
      sub_compounds: sub_compounds,
      super_compounds: super_compounds,
      annotations: annotations
    )
    |> add(unasserted, assertion_mode: :unasserted)
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

    This function requests the execution of a SPARQL `CONSTRUCT` query without any
    further options on the respective `SPARQL.Client` call. You can configure the
    options to be used on this request with the `:from_sparql_opts` configuration
    on your `config.exs` files, e.g.

        config :rtc, :from_sparql_opts,
            accept_header: "application/x-turtlestar",
            result_format: :turtle

    See `SPARQL.Client` for available options.
    """
    @spec from_sparql(String.t(), coercible_id()) :: {:ok, t()}
    defdelegate from_sparql(endpoint, compound_id), to: RTC.SPARQL, as: :from_endpoint

    @doc """
    Retrieves the compound with the given `compound_id` from a SPARQL endpoint.

    This function is only available when the `sparql_client` dependency is
    added in your `Mixfile`.

    The `opts` are used as the options for the `CONSTRUCT` query call by the
    `SPARQL.Client` against the endpoint. See `SPARQL.Client` for
    available options.

    When no compound with the given `compound_id` can be found in the given
    `graph`, an empty compound is returned.
    """
    @spec from_sparql(String.t(), coercible_id(), keyword) :: {:ok, t()} | {:error, any}
    defdelegate from_sparql(endpoint, compound_id, opts), to: RTC.SPARQL, as: :from_endpoint

    @doc """
    Retrieves the compound with the given `compound_id` from a SPARQL endpoint.

    As opposed to `from_sparql/3` this function returns the result directly and
    raises an error when the SPARQL client call fails.
    """
    @spec from_sparql!(String.t(), coercible_id(), keyword) :: t()
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

  When no `:element_style` is specified, the configurable result of the
  `default_element_style/0` function is used as the default.

  The following options can be used to customize the returned graph:

  - `:name`: the name of the graph to be created. By default, the compound id is
    used as the graph name, unless a blank node is used as the compound id,
    or the one specified with `:name` on `new/3`.
  - `:prefixes`: some prefix mappings which should be added the graph
    and will be used for example when serializing in a format with prefix support.
    Defaults to the ones specified during the creation of the compound with `new/3`
    or the `RDF.default_prefixes/0`. The `rtc` prefix for the RTC vocabulary is
    added in any case, so it is not required to be defined here.
  - `:base_iri`: a base IRI which should be stored alongside the graph
    and will be used for example when serializing in a format with base IRI support

  """
  @spec to_rdf(t, keyword) :: Graph.t()
  def to_rdf(%__MODULE__{} = compound, opts \\ []) do
    compound.asserted
    |> apply_graph_opts(
      if compound.asserted |> Graph.prefixes() |> PrefixMap.empty?() do
        Keyword.put_new(opts, :prefixes, RDF.default_prefixes())
      else
        opts
      end
    )
    |> Graph.add_prefixes(rtc: RTC.NS.RTC)
    |> do_to_rdf(
      compound,
      Keyword.get(opts, :element_style, default_element_style()),
      Keyword.get(opts, :include_super_compounds, false)
    )
  end

  defp do_to_rdf(graph, compound, element_style, include_super_compounds) do
    graph =
      graph
      |> annotate(compound, element_style)
      |> Graph.add({id(compound), RTC.subCompoundOf(), super_compounds(compound)})

    graph =
      if include_super_compounds do
        Enum.reduce(compound.super_compounds, graph, fn {_, description}, graph ->
          Graph.add(graph, description)
        end)
      else
        graph
      end

    Enum.reduce(compound.sub_compounds, graph, fn
      {sub_compound_id, sub_compound}, acc_graph ->
        acc_graph
        |> Graph.add(sub_compound.asserted)
        |> Graph.add({sub_compound_id, RTC.subCompoundOf(), id(compound)})
        |> do_to_rdf(sub_compound, element_style, include_super_compounds)
    end)
  end

  defp apply_graph_opts(graph, opts) do
    graph
    |> apply_option(opts, :name, &Graph.change_name(&1, &2))
    |> apply_option(opts, :base_iri, &Graph.set_base_iri(&1, &2))
    |> apply_option(opts, :prefixes, &Graph.add_prefixes(&1, &2))
  end

  defp apply_option(subject, opts, opt, fun) do
    if Keyword.has_key?(opts, opt) do
      fun.(subject, Keyword.get(opts, opt))
    else
      subject
    end
  end

  defp annotate(graph, compound, :element_of) do
    graph
    |> Graph.add(compound.annotations)
    |> Graph.add_annotations(compound.asserted, {RTC.elementOf(), id(compound)})
    |> Graph.add_annotations(compound.unasserted, {RTC.elementOf(), id(compound)})
  end

  defp annotate(graph, compound, :elements) do
    Graph.add(
      graph,
      compound.annotations
      |> RTC.elements(Graph.triples(compound.asserted))
      |> RTC.elements(Graph.triples(compound.unasserted))
    )
  end

  @doc """
  Returns an RDF graph of the asserted triples in the compound (incl. its sub-compounds) without the annotations.

  The following options can be used to customize the returned graph:

  - `:name`: the name of the graph to be created
    (by default, the compound id is used as the graph name, unless a blank node is used
    as the compound id, or the one specified with `:name` on `new/3`)
  - `:prefixes`: some prefix mappings which should be added the graph
    and will be used for example when serializing in a format with prefix support.
    Defaults to the ones specified during the creation of the compound with `new/3`.
  - `:base_iri`: a base IRI which should be stored alongside the graph
    and will be used for example when serializing in a format with base IRI support

  """
  @spec asserted_graph(t, keyword) :: Graph.t()
  def asserted_graph(%__MODULE__{} = compound, opts \\ []) do
    compound.asserted
    |> apply_graph_opts(opts)
    |> Graph.add(do_asserted_graph(compound.sub_compounds))
  end

  defp do_asserted_graph(sub_compounds) do
    Enum.flat_map(sub_compounds, fn {_, compound} ->
      [compound.asserted | do_asserted_graph(compound.sub_compounds)]
    end)
  end

  @doc """
  Returns an RDF graph of the unasserted triples in the compound (incl. its sub-compounds) without the annotations.

  The following options can be used to customize the returned graph:

  - `:name`: the name of the graph to be created
    (by default, the compound id is used as the graph name, unless a blank node is used
    as the compound id, or the one specified with `:name` on `new/3`)
  - `:prefixes`: some prefix mappings which should be added the graph
    and will be used for example when serializing in a format with prefix support.
    Defaults to the ones specified during the creation of the compound with `new/3`.
  - `:base_iri`: a base IRI which should be stored alongside the graph
    and will be used for example when serializing in a format with base IRI support

  """
  @spec unasserted_graph(t, keyword) :: Graph.t()
  def unasserted_graph(%__MODULE__{} = compound, opts \\ []) do
    compound
    |> unasserted_graph_with_proper_metadata()
    |> apply_graph_opts(opts)
    |> Graph.add(do_unasserted_graph(compound.sub_compounds))
  end

  defp do_unasserted_graph(sub_compounds) do
    Enum.flat_map(sub_compounds, fn {_, compound} ->
      [compound.unasserted | do_unasserted_graph(compound.sub_compounds)]
    end)
  end

  defp unasserted_graph_with_proper_metadata(compound) do
    %Graph{compound.asserted | descriptions: compound.unasserted.descriptions}
  end

  @doc """
  Returns an RDF graph of all the asserted and unasserted triples in the compound (incl. its sub-compounds) without the annotations.

  The following options can be used to customize the returned graph:

  - `:name`: the name of the graph to be created
    (by default, the compound id is used as the graph name, unless a blank node is used
    as the compound id, or the one specified with `:name` on `new/3`)
  - `:prefixes`: some prefix mappings which should be added the graph
    and will be used for example when serializing in a format with prefix support.
    Defaults to the ones specified during the creation of the compound with `new/3`.
  - `:base_iri`: a base IRI which should be stored alongside the graph
    and will be used for example when serializing in a format with base IRI support
  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  """
  @spec graph(t, keyword) :: Graph.t()
  def graph(%__MODULE__{} = compound, opts \\ []) do
    case assertion_mode_with_all(opts) do
      :all ->
        compound
        |> asserted_graph(opts)
        |> Graph.add(unasserted_graph(compound, opts))

      :asserted ->
        asserted_graph(compound, opts)

      :unasserted ->
        unasserted_graph(compound, opts)
    end
  end

  @doc """
  Returns a list of the triples in the given `compound`.

  Supported options:

  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  """
  @spec triples(t, keyword) :: [Triple.t()]
  def triples(%__MODULE__{} = compound, opts \\ []) do
    compound
    |> graph(opts)
    |> Graph.triples()
  end

  defdelegate statements(compound), to: __MODULE__, as: :triples

  @doc """
  Gets the description of the given `subject`.

  When the subject can not be found the optionally given default value or
  `nil` is returned.

  Supported options:

  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  ## Examples

      iex> RTC.Compound.new([{EX.S1, EX.P1, EX.O1}, {EX.S2, EX.P2, EX.O2}], EX.Compound)
      ...> |> RTC.Compound.get(EX.S1)
      RDF.Description.new(EX.S1, init: {EX.P1, EX.O1})

      iex> RTC.Compound.get(RTC.Compound.new(), EX.Foo)
      nil

      iex> RTC.Compound.get(RTC.Compound.new(), EX.Foo, :bar)
      :bar

  """
  @spec get(t, Statement.coercible_subject(), any, keyword) :: Description.t() | any
  def get(compound, subject, default \\ nil, opts \\ []) do
    assertion_mode = assertion_mode_with_all(opts)

    case do_get(assertion_mode, compound, subject) ++
           do_get_sub(assertion_mode, compound.sub_compounds, subject) do
      [] -> default
      [description] -> description
      [description | rest] -> Enum.into(rest, description)
    end
  end

  defp do_get(:asserted, compound, subject),
    do: Graph.get(compound.asserted, subject) |> List.wrap()

  defp do_get(:unasserted, compound, subject),
    do: Graph.get(compound.unasserted, subject) |> List.wrap()

  defp do_get(:all, compound, subject) do
    do_get(:asserted, compound, subject) ++ do_get(:unasserted, compound, subject)
  end

  defp do_get_sub(assertion_mode, sub_compounds, subject) do
    Enum.flat_map(sub_compounds, fn {_, compound} ->
      do_get(assertion_mode, compound, subject) ++
        do_get_sub(assertion_mode, compound.sub_compounds, subject)
    end)
  end

  @doc """
  Returns the description of the given subject.

  When the subject can not be found an empty description is returned.

  Supported options:

  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  ## Examples

      iex> RTC.Compound.new([{EX.S1, EX.P1, EX.O1}, {EX.S2, EX.P2, EX.O2}])
      ...> |> RTC.Compound.description(EX.S1)
      RDF.Description.new(EX.S1, init: {EX.P1, EX.O1})

      iex> RTC.Compound.new([{EX.S, EX.P1, EX.O1}], sub_compounds: RTC.Compound.new([{EX.S, EX.P2, EX.O2}]))
      ...> |> RTC.Compound.description(EX.S)
      RDF.Description.new(EX.S, init: [{EX.P1, EX.O1}, {EX.P2, EX.O2}])

      iex> RTC.Compound.new([])
      ...> |> RTC.Compound.description(EX.S1)
      RDF.Description.new(EX.S1)

  """
  @spec description(t, Statement.coercible_subject(), keyword) :: Description.t() | nil
  def description(%__MODULE__{} = compound, subject, opts \\ []) do
    get(compound, subject, Description.new(subject), opts)
  end

  @doc """
  Fetches the description of the given subject.

  When the subject can not be found `:error` is returned.

  Supported options:

  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  ## Examples

      iex> RTC.Compound.new([{EX.S1, EX.P1, EX.O1}, {EX.S2, EX.P2, EX.O2}], EX.Compound)
      ...> |> RTC.Compound.fetch(EX.S1)
      {:ok, RDF.Description.new(EX.S1, init: {EX.P1, EX.O1})}

      iex> RTC.Compound.new() |> RTC.Compound.fetch(EX.foo)
      :error

  """
  @spec fetch(t, Statement.coercible_subject(), keyword) :: {:ok, Description.t()} | :error
  def fetch(%__MODULE__{} = compound, subject, opts \\ []) do
    case get(compound, subject, nil, opts) do
      nil -> :error
      description -> {:ok, description}
    end
  end

  @doc """
  Returns if the given `compound` (and of its sub-compounds) does not contain any triples.

  Supported options:

  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  """
  @spec empty?(t, keyword) :: boolean
  def empty?(%__MODULE__{} = compound, opts \\ []) do
    assertion_mode = assertion_mode_with_all(opts)

    (assertion_mode == :unasserted or Graph.empty?(compound.asserted)) and
      (assertion_mode == :asserted or Graph.empty?(compound.unasserted)) and
      Enum.all?(compound.sub_compounds, fn {_, sub_compound} -> empty?(sub_compound, opts) end)
  end

  @doc """
  Returns the number of triples in the given `compound`.

  Supported options:

  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  """
  @spec triple_count(t, keyword) :: non_neg_integer
  def triple_count(%__MODULE__{} = compound, opts \\ []) do
    compound
    |> graph(opts)
    |> Graph.triple_count()
  end

  defdelegate statement_count(compound), to: __MODULE__, as: :statement_count

  @doc """
  The set of all subjects used in the triples within a `RTC.Compound` or any of its sub-compounds.

  Supported options:

  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  ## Examples

      iex> RTC.Compound.new([
      ...>   {EX.S1, EX.p1, EX.O1},
      ...>   {EX.S2, EX.p2, EX.O2},
      ...>   {EX.S1, EX.p2, EX.O3}],
      ...>   EX.Compound)
      ...> |> RTC.Compound.subjects()
      MapSet.new([RDF.iri(EX.S1), RDF.iri(EX.S2)])
  """
  @spec subjects(t, keyword) :: MapSet.t()
  def subjects(%__MODULE__{} = compound, opts \\ []) do
    subjects =
      case assertion_mode_with_all(opts) do
        :asserted ->
          Graph.subjects(compound.asserted)

        :unasserted ->
          Graph.subjects(compound.unasserted)

        :all ->
          Graph.subjects(compound.asserted)
          |> MapSet.union(Graph.subjects(compound.unasserted))
      end

    Enum.reduce(compound.sub_compounds, subjects, fn
      {_, sub_compound}, subjects -> MapSet.union(subjects, subjects(sub_compound, opts))
    end)
  end

  @doc """
  The set of all properties used in the triples within a `RTC.Compound` or any of its sub-compounds.

  Supported options:

  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  ## Examples

      iex> RTC.Compound.new([
      ...>   {EX.S1, EX.p1, EX.O1},
      ...>   {EX.S2, EX.p2, EX.O2},
      ...>   {EX.S1, EX.p2, EX.O3}],
      ...>   EX.Compound)
      ...> |> RTC.Compound.predicates()
      MapSet.new([EX.p1, EX.p2])
  """
  @spec predicates(t, keyword) :: MapSet.t()
  def predicates(%__MODULE__{} = compound, opts \\ []) do
    predicates =
      case assertion_mode_with_all(opts) do
        :asserted ->
          Graph.predicates(compound.asserted)

        :unasserted ->
          Graph.predicates(compound.unasserted)

        :all ->
          Graph.predicates(compound.asserted)
          |> MapSet.union(Graph.predicates(compound.unasserted))
      end

    Enum.reduce(compound.sub_compounds, predicates, fn
      {_, sub_compound}, predicates -> MapSet.union(predicates, predicates(sub_compound, opts))
    end)
  end

  @doc """
  The set of all resources used in the objects of the triples within a `RTC.Compound` or any of its sub-compounds.

  Note: This function does collect only IRIs and BlankNodes, not Literals.

  Supported options:

  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  ## Examples

      iex> RTC.Compound.new([
      ...>   {EX.S1, EX.p1, EX.O1},
      ...>   {EX.S2, EX.p2, EX.O2},
      ...>   {EX.S3, EX.p1, EX.O2},
      ...>   {EX.S4, EX.p2, RDF.bnode(:bnode)},
      ...>   {EX.S5, EX.p3, "foo"}],
      ...>   EX.Compound)
      ...> |> RTC.Compound.objects()
      MapSet.new([RDF.iri(EX.O1), RDF.iri(EX.O2), RDF.bnode(:bnode)])
  """
  @spec objects(t, keyword) :: MapSet.t()
  def objects(%__MODULE__{} = compound, opts \\ []) do
    objects =
      case assertion_mode_with_all(opts) do
        :asserted ->
          Graph.objects(compound.asserted)

        :unasserted ->
          Graph.objects(compound.unasserted)

        :all ->
          Graph.objects(compound.asserted)
          |> MapSet.union(Graph.objects(compound.unasserted))
      end

    Enum.reduce(compound.sub_compounds, objects, fn
      {_, sub_compound}, objects -> MapSet.union(objects, objects(sub_compound, opts))
    end)
  end

  @doc """
  The set of all resources used in the triples within a `RTC.Compound` or any of its sub-compounds.

  Supported options:

  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  ## Examples

      iex> RTC.Compound.new([
      ...>   {EX.S1, EX.p1, EX.O1},
      ...>   {EX.S2, EX.p1, EX.O2},
      ...>   {EX.S2, EX.p2, RDF.bnode(:bnode)},
      ...>   {EX.S3, EX.p1, "foo"}],
      ...>   EX.Compound)
      ...> |> RTC.Compound.resources()
      MapSet.new([RDF.iri(EX.S1), RDF.iri(EX.S2), RDF.iri(EX.S3),
        RDF.iri(EX.O1), RDF.iri(EX.O2), RDF.bnode(:bnode), EX.p1, EX.p2])
  """
  @spec resources(t, keyword) :: MapSet.t()
  def resources(%__MODULE__{} = compound, opts \\ []) do
    resources =
      case assertion_mode_with_all(opts) do
        :asserted ->
          Graph.resources(compound.asserted)

        :unasserted ->
          Graph.resources(compound.unasserted)

        :all ->
          Graph.resources(compound.asserted)
          |> MapSet.union(Graph.resources(compound.unasserted))
      end

    Enum.reduce(compound.sub_compounds, resources, fn
      {_, sub_compound}, resources -> MapSet.union(resources, resources(sub_compound, opts))
    end)
  end

  @doc """
  Returns a list of the `RDF.Description`s in the given `compound`.

  Supported options:

  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  """
  @spec descriptions(t, keyword) :: [Description.t()]
  def descriptions(%__MODULE__{} = compound, opts \\ []) do
    compound
    |> graph(opts)
    |> Graph.descriptions()
  end

  @doc """
  Returns whether the given triples are an element of the given `compound` or any of its sub-compounds.

  Supported options:

  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  """
  @spec include?(t, Graph.input(), keyword) :: boolean
  def include?(%__MODULE__{} = compound, input, opts \\ []) do
    compound
    |> graph(opts)
    |> Graph.include?(input)
  end

  @doc """
  Returns whether the given `compound` or any of its sub-compounds contains triples about the given subject.

  Supported options:

  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  ## Examples

      iex> RTC.Compound.new([{EX.S1, EX.p1, EX.O1}]) |> RTC.Compound.describes?(EX.S1)
      true

      iex> RTC.Compound.new([{EX.S1, EX.p1, EX.O1}]) |> RTC.Compound.describes?(EX.S2)
      false

  """
  @spec describes?(t, Statement.coercible_subject(), keyword) :: boolean
  def describes?(%__MODULE__{} = compound, subject, opts \\ []) do
    assertion_mode = assertion_mode_with_all(opts)

    (assertion_mode != :unasserted and Graph.describes?(compound.asserted, subject)) or
      (assertion_mode != :asserted and Graph.describes?(compound.unasserted, subject)) or
      Enum.any?(compound.sub_compounds, fn {_, sub_compound} ->
        describes?(sub_compound, subject, opts)
      end)
  end

  @doc """
  Adds `triples` to the given `compound`.

  Triples can be provided in any form accepted by `RDF.Graph.add/2`.

  Available options:

  - `:assertion_mode`: the assertion mode to be used for the `triples` to be added
    (see module documentation section on "Asserted and unasserted triples")

  """
  @spec add(t, Graph.input(), keyword) :: t
  def add(compound, triples, opts \\ []) do
    case assertion_mode(opts) do
      :asserted ->
        compound
        |> update_unasserted(&Graph.delete(&1, triples, opts))
        |> update_asserted(&Graph.add(&1, triples, opts))

      :unasserted ->
        compound
        |> update_asserted(&Graph.delete(&1, triples, opts))
        |> update_unasserted(&Graph.add(&1, triples, opts))
    end
  end

  defp update_asserted(compound, fun) do
    %__MODULE__{compound | asserted: fun.(compound.asserted)}
  end

  defp update_unasserted(compound, fun) do
    %__MODULE__{compound | unasserted: fun.(compound.unasserted)}
  end

  @doc """
  Deletes `triples` from the given `compound`.

  Triples can be provided in any form accepted by `RDF.Graph.delete/2`.

  If a triple occurs in one or more sub-compounds, it gets deleted from all of them.

  Supported options:

  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  """
  @spec delete(t, Graph.input(), keyword) :: t
  def delete(%__MODULE__{} = compound, triples, opts \\ []) do
    assertion_mode = assertion_mode_with_all(opts)

    compound =
      if assertion_mode in [:all, :asserted] do
        update_asserted(compound, &Graph.delete(&1, triples, opts))
      else
        compound
      end

    compound =
      if assertion_mode in [:all, :unasserted] do
        update_unasserted(compound, &Graph.delete(&1, triples, opts))
      else
        compound
      end

    %__MODULE__{
      compound
      | sub_compounds:
          Map.new(compound.sub_compounds, fn {id, sub_compound} ->
            {id, delete(sub_compound, triples, opts)}
          end)
    }
  end

  @doc """
  Deletes all triples with the given `subjects` from the given `compound`.

  If a triple occurs in one or more sub-compounds, it gets deleted from all of them.

  Supported options:

  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  """
  @spec delete_descriptions(
          t,
          Statement.coercible_subject() | [Statement.coercible_subject()],
          keyword
        ) ::
          t
  def delete_descriptions(%__MODULE__{} = compound, subjects, opts \\ []) do
    assertion_mode = assertion_mode_with_all(opts)

    compound =
      if assertion_mode in [:all, :asserted] do
        update_asserted(compound, &Graph.delete_descriptions(&1, subjects, opts))
      else
        compound
      end

    compound =
      if assertion_mode in [:all, :unasserted] do
        update_unasserted(compound, &Graph.delete_descriptions(&1, subjects, opts))
      else
        compound
      end

    %__MODULE__{
      compound
      | sub_compounds:
          Map.new(compound.sub_compounds, fn {id, sub_compound} ->
            {id, delete_descriptions(sub_compound, subjects, opts)}
          end)
    }
  end

  @doc """
  Pops the description of the given subject.

  Removes the description of the given `subject` from `compound` and
  all of its sub-compounds.

  Returns a tuple containing the description of the given subject
  and the updated compound without this description.
  `nil` is returned instead of the description if `compound` does
  not contain a description of the given `subject`.

  Supported options:

  - `:assertion_mode`: see module documentation section on "Asserted and unasserted triples"

  ## Examples

      iex> RTC.Compound.new([{EX.S1, EX.P1, EX.O1}, {EX.S2, EX.P2, EX.O2}], EX.Compound)
      ...> |> RTC.Compound.pop(EX.S1)
      {
        RDF.Description.new(EX.S1, init: {EX.P1, EX.O1}),
        RTC.Compound.new({EX.S2, EX.P2, EX.O2}, EX.Compound)
      }

      iex> RTC.Compound.new({EX.S, EX.P, EX.O}, EX.Compound)
      ...> |> RTC.Compound.pop(EX.Missing)
      {nil, RTC.Compound.new({EX.S, EX.P, EX.O}, EX.Compound)}

  """
  @spec pop(t, Statement.coercible_subject(), keyword) :: {Description.t() | nil, t}
  def pop(%__MODULE__{} = compound, subject, opts \\ []) do
    subject = RDF.coerce_subject(subject)
    assertion_mode = assertion_mode_with_all(opts)

    {description, compound} =
      if assertion_mode in [:all, :asserted] do
        case Graph.pop(compound.asserted, subject) do
          {nil, _} ->
            {nil, compound}

          {asserted_description, new_asserted} ->
            {asserted_description, %__MODULE__{compound | asserted: new_asserted}}
        end
      else
        {nil, compound}
      end

    {description, compound} =
      if assertion_mode in [:all, :unasserted] do
        case Graph.pop(compound.unasserted, subject) do
          {nil, _} ->
            {description, compound}

          {unasserted_description, new_unasserted} ->
            {
              if(description,
                do: Description.add(description, unasserted_description),
                else: unasserted_description
              ),
              %__MODULE__{compound | unasserted: new_unasserted}
            }
        end
      else
        {description, compound}
      end

    {description, new_sub_compounds} =
      Enum.reduce(compound.sub_compounds, {description, compound.sub_compounds}, fn
        {id, sub_compound}, {description, new_sub_compounds} ->
          case pop(sub_compound, subject, opts) do
            {nil, _} ->
              {description, new_sub_compounds}

            {sub_description, new_sub_compound} ->
              {
                if(description,
                  do: Description.add(description, sub_description),
                  else: sub_description
                ),
                Map.put(new_sub_compounds, id, new_sub_compound)
              }
          end
      end)

    {description, %__MODULE__{compound | sub_compounds: new_sub_compounds}}
  end

  @doc """
  Pops an arbitrary triple from the given `compound` or any of its sub-compounds.
  """
  @spec pop(t) :: {Triple.t() | nil, t}
  def pop(%__MODULE__{} = compound) do
    cond do
      not Graph.empty?(compound.asserted) ->
        {triple, graph} = Graph.pop(compound.asserted)
        {triple, %{compound | asserted: graph}}

      not Graph.empty?(compound.unasserted) ->
        {triple, graph} = Graph.pop(compound.unasserted)
        {triple, %{compound | unasserted: graph}}

      true ->
        compound.sub_compounds
        |> Enum.find_value(fn {_id, sub_compound} ->
          case pop(sub_compound) do
            {nil, _} -> nil
            {triple, sub_compound} -> {triple, sub_compound}
          end
        end)
        |> case do
          nil -> {nil, compound}
          {triple, sub_compound} -> {triple, put_sub_compound(compound, sub_compound)}
        end
    end
  end

  @doc """
  Returns a list of the sub-compounds of the given `compound`.
  """
  @spec sub_compounds(t) :: [t]
  def sub_compounds(%__MODULE__{} = compound) do
    Enum.map(compound.sub_compounds, fn {_, sub_compound} ->
      put_super_compound(sub_compound, compound)
    end)
  end

  @doc """
  Returns the sub-compound of the given `compound` with the given `sub_compound_id`.

  It will search the whole sub-compound tree for the requested `sub_compound_id`

  If no compound with the `sub_compound_id` can be found, `nil` is returned.
  """
  @spec sub_compound(t, coercible_id) :: t | nil
  def sub_compound(%__MODULE__{} = compound, sub_compound_id) do
    sub_compound_id = Statement.coerce_subject(sub_compound_id)

    Enum.find_value(compound.sub_compounds, fn
      {^sub_compound_id, sub_compound} -> put_super_compound(sub_compound, compound)
      {_, sub_compound} -> sub_compound(sub_compound, sub_compound_id)
    end)
  end

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
      | sub_compounds:
          Map.put(
            compound.sub_compounds,
            id(sub_compound),
            delete_super_compound(sub_compound, compound)
          )
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
  @spec delete_sub_compound(t, t | coercible_id) :: t
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
  @spec put_super_compound(
          t,
          coercible_id | t | Description.t() | [coercible_id | t | Description.t()]
        ) :: t
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
  @spec delete_super_compound(t, t | coercible_id) :: t
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

  Supported options:

  - `:inherited` - controls if inherited annotations should be included or
    only the direct annotations of the given compound should be returned.
    Default: `true`
  """
  @spec annotations(t) :: Description.t()
  def annotations(%__MODULE__{} = compound, opts \\ []) do
    if Keyword.get(opts, :inherited, true) do
      for {_, inherited} <- compound.super_compounds, into: compound.annotations do
        inherited
      end
    else
      compound.annotations
    end
  end

  @doc """
  Returns the merged annotations of all super-compounds of the given `compound`.
  """
  @spec inherited_annotations(t) :: Description.t()
  def inherited_annotations(%__MODULE__{} = compound) do
    for {_, inherited} <- compound.super_compounds, into: RDF.description(id(compound)) do
      inherited
    end
  end

  @doc """
  Returns the annotations of the given super-compound of `compound`.
  """
  @spec inherited_annotations(t, coercible_id) :: Description.t()
  def inherited_annotations(compound, super_compound_id)

  def inherited_annotations(%__MODULE__{} = compound, super_compound_id),
    do: compound.super_compounds[Statement.coerce_subject(super_compound_id)]

  @doc """
  Adds statements to the annotations of the given `compound`.
  """
  @spec add_annotations(t, Description.input()) :: t
  def add_annotations(%__MODULE__{} = compound, annotations) do
    %__MODULE__{compound | annotations: Description.add(compound.annotations, annotations)}
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

    def reduce(%Compound{} = compound, acc, fun),
      do: compound |> Compound.graph() |> Enumerable.reduce(acc, fun)

    def member?(%Compound{} = compound, triple), do: {:ok, Compound.include?(compound, triple)}

    def count(_), do: {:error, __MODULE__}

    def slice(_), do: {:error, __MODULE__}
  end

  defimpl RDF.Data do
    alias RTC.Compound
    alias RDF.{Description, Graph, Dataset, Statement}

    def merge(compound, data, opts \\ []) do
      graph_name =
        case data do
          {_, _, _, graph_name} -> graph_name
          %Graph{name: graph_name} -> graph_name
          %Dataset{} -> compound.asserted.name
          _ -> nil
        end

      compound
      |> Compound.graph(name: graph_name)
      |> RDF.Data.merge(data, opts)
    end

    def delete(compound, input, _opts \\ []), do: Compound.delete(compound, input)
    def pop(compound), do: Compound.pop(compound)
    def empty?(compound), do: Compound.empty?(compound)
    def include?(compound, input, _opts \\ []), do: Compound.include?(compound, input)
    def describes?(compound, subject), do: Compound.describes?(compound, subject)
    def description(compound, subject), do: Compound.description(compound, subject)
    def descriptions(compound), do: Compound.descriptions(compound)
    def statements(compound), do: Compound.statements(compound)
    def subjects(compound), do: Compound.subjects(compound)
    def predicates(compound), do: Compound.predicates(compound)
    def objects(compound), do: Compound.objects(compound)
    def resources(compound), do: Compound.resources(compound)
    def subject_count(compound), do: compound |> subjects() |> MapSet.size()
    def statement_count(compound), do: Compound.triple_count(compound)
    def values(compound, opts \\ []), do: compound |> Compound.graph() |> Graph.values(opts)
    def map(compound, fun), do: compound |> Compound.graph() |> Graph.values(fun)

    def equal?(compound, %Compound{} = other),
      do: compound |> Compound.graph() |> RDF.Data.equal?(Compound.graph(other))

    def equal?(compound, data), do: compound |> Compound.graph() |> RDF.Data.equal?(data)
  end

  defimpl Inspect do
    alias RTC.Compound

    def inspect(compound, opts) do
      if opts.structs do
        try do
          graph =
            Compound.to_rdf(compound,
              element_style: :elements,
              include_super_compounds: true
            )

          id = Compound.id(compound)

          graph_name =
            if graph.name != id do
              ", graph_name: #{inspect(graph.name)}"
            end

          header = "#RTC.Compound<id: #{inspect(id)}#{graph_name}"

          body = Kernel.inspect(graph, custom_options: [content_only: true])

          "#{header}\n#{body}\n>"
        rescue
          caught_exception ->
            message =
              "got #{inspect(caught_exception.__struct__)} with message " <>
                "#{inspect(Exception.message(caught_exception))} while inspecting RTC.Compound #{Compound.id(compound)}"

            exception = Inspect.Error.exception(message: message)

            if opts.safe do
              Inspect.inspect(exception, opts)
            else
              reraise(exception, __STACKTRACE__)
            end
        end
      else
        Inspect.Map.inspect(compound, opts)
      end
    end
  end
end
