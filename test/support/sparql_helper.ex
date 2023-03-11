defmodule RTC.Test.SparqlHelper do
  alias RTC.Compound
  alias RDF.Graph

  @sparql_service_type_var "RTC_TEST_SPARQL_SERVICE_TYPE"
  @default_sparql_service_type "oxigraph"

  @repository_id "__RTC_TEST_SUITE__"
  @graph_db_host "http://localhost:7200"
  @oxigraph_host "http://localhost:7879"

  @sparql_service_settings %{
    oxigraph: %{
      endpoint: "#{@oxigraph_host}/query",
      update_endpoint: "#{@oxigraph_host}/update"
    },
    graph_db: %{
      endpoint: "#{@graph_db_host}/repositories/#{@repository_id}",
      update_endpoint: "#{@graph_db_host}/repositories/#{@repository_id}/statements"
    }
  }

  def sparql_service_type do
    case System.get_env(@sparql_service_type_var, @default_sparql_service_type) do
      "oxigraph" -> :oxigraph
      "graph_db" -> :graph_db
      unknown -> raise "unknown sparql_service_type: #{inspect(unknown)}"
    end
  end

  def create_repository(sparql_service_type \\ sparql_service_type())
  def create_repository(:oxigraph), do: :ok

  def create_repository(:graph_db) do
    HTTPoison.put(
      @sparql_service_settings[:graph_db][:endpoint],
      """
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
      @prefix rep: <http://www.openrdf.org/config/repository#>.
      @prefix sr: <http://www.openrdf.org/config/repository/sail#>.
      @prefix sail: <http://www.openrdf.org/config/sail#>.
      @prefix ms: <http://www.openrdf.org/config/sail/memory#>.

      [] a rep:Repository ;
         rep:repositoryID "#{@repository_id}" ;
         rdfs:label "#{@repository_id}" ;
         rep:repositoryImpl [
            rep:repositoryType "openrdf:SailRepository" ;
            sr:sailImpl [
               sail:sailType "openrdf:MemoryStore" ;
               ms:persist false ;
               ms:syncDelay 120
            ]
         ].
      """,
      [{"Content-Type", "text/turtle"}],
      recv_timeout: 2000
    )
  end

  def delete_repository!(sparql_service_type \\ sparql_service_type())
  def delete_repository!(:oxigraph), do: :ok

  def delete_repository!(:graph_db) do
    :ok = HTTPoison.delete(@sparql_service_settings[:graph_db][:endpoint])
  end

  def clean_repository!(sparql_service_type \\ sparql_service_type()) do
    :ok =
      SPARQL.Client.delete(
        """
        DELETE { ?s ?p ?o }
        WHERE  { ?s ?p ?o . }
        """,
        @sparql_service_settings[sparql_service_type][:update_endpoint],
        raw_mode: true
      )
  end

  def insert(data, sparql_service_type \\ sparql_service_type())

  def insert(%Compound{} = compound, sparql_service_type) do
    compound
    # We need the elements-style since GraphDB doesn't support the annotation syntax used in the elementOf-style
    |> Compound.to_rdf(element_style: :elements)
    |> insert(sparql_service_type)
  end

  def insert(%Graph{} = graph, :graph_db), do: insert_segregated(graph)
  def insert(data, sparql_service_type), do: do_insert(data, sparql_service_type)

  def do_insert(data, sparql_service_type) do
    :ok =
      SPARQL.Client.insert_data(
        data,
        @sparql_service_settings[sparql_service_type][:update_endpoint]
      )
  end

  # This function works around GraphDBs inability to handle the annotation syntax
  defp insert_segregated(graph) do
    {graph, annotations} =
      graph
      |> Graph.descriptions()
      |> Enum.reduce({graph, Graph.new()}, fn
        %{subject: triple} = description, {graph, annotations} when is_tuple(triple) ->
          {
            Graph.delete_descriptions(graph, description.subject),
            Graph.add(annotations, description)
          }

        _, {graph, annotations} ->
          {graph, annotations}
      end)

    do_insert(graph, :graph_db)
    do_insert(annotations, :graph_db)
  end

  def full_graph(sparql_service_type \\ sparql_service_type()) do
    with {:ok, result} <-
           """
           CONSTRUCT { ?s ?p ?o . }
           WHERE     {	?s ?p ?o . }
           """
           |> SPARQL.Client.construct(@sparql_service_settings[sparql_service_type][:endpoint]) do
      result
    end
  end

  def from_sparql!(id, sparql_service_type \\ sparql_service_type())

  def from_sparql!(id, :oxigraph) do
    opts = [result_format: :turtle]

    @sparql_service_settings[:oxigraph][:endpoint]
    |> Compound.from_sparql!(id, opts)
  end

  def from_sparql!(id, :graph_db) do
    opts = [accept_header: "application/x-turtlestar", result_format: :turtle]

    @sparql_service_settings[:graph_db][:endpoint]
    |> Compound.from_sparql!(id, opts)
  end
end
