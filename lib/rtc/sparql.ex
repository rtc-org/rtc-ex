# Since functions of optional dependencies can't be found by dialyzer, it can't
# find the SPARQL.Client functions. We're ignoring these warnings (via .dialyzer_ignore).
# See https://elixirforum.com/t/confusing-behavior-of-optional-deps-in-mix-exs/17719/4

if Code.ensure_loaded?(SPARQL.Client) do
  defmodule RTC.SPARQL do
    @moduledoc false

    alias RTC.Compound

    @prefixes """
    PREFIX rtc: <#{RTC.NS.RTC.__base_iri__()}>
    """

    def from_endpoint(endpoint, compound_id, opts \\ nil)

    def from_endpoint(endpoint, compound_id, nil),
      do: from_endpoint(endpoint, compound_id, default_opts())

    def from_endpoint(endpoint, compound_id, opts) do
      opts = Keyword.put(opts, :raw_mode, true)

      with {:ok, results} <-
             compound_id
             |> graph_query()
             |> SPARQL.Client.construct(endpoint, opts) do
        from_results(results, compound_id)
      end
    end

    defp graph_query(compound_id) do
      compound_id = RDF.Statement.coerce_subject(compound_id)

      # TODO: use BIND( TRIPLE(?s, ?p, ?o) AS ?t ) when the new SPARQL 1.2 TRIPLE function is more widely supported
      """
      #{@prefixes}
      CONSTRUCT {
        ?s1 ?p1 ?o1 .
        ?s2 ?p2 ?o2 .
        ?s3 ?p3 ?o3 .
        ?s4 ?p4 ?o4 .

        <#{compound_id}> ?compound_p ?compound_o ;
          rtc:elements ?triple .

        ?sub_compound ?sub_compound_p ?sub_compound_o ;
          rtc:elements ?sub_triple .

        ?super_compound ?super_compound_p ?super_compound_o .
      }
      WHERE {
        {
          ?triple rtc:elementOf <#{compound_id}> .
          OPTIONAL {
            << ?s1 ?p1 ?o1 >> rtc:elementOf <#{compound_id}> .
            ?s1 ?p1 ?o1 .
          }
        }
        UNION
        {
          <#{compound_id}> ?compound_p ?compound_o .
          OPTIONAL {
            <#{compound_id}> rtc:elements << ?s2 ?p2 ?o2 >> .
            ?s2 ?p2 ?o2 .
          }
        }
        UNION
        {
          ?sub_compound rtc:subCompoundOf+ <#{compound_id}> ;
            ?sub_compound_p ?sub_compound_o .
          OPTIONAL { ?sub_triple rtc:elementOf ?sub_compound . }
          OPTIONAL { << ?s3 ?p3 ?o3 >> rtc:elementOf ?sub_compound . ?s3 ?p3 ?o3 }
          OPTIONAL { ?sub_compound rtc:elements << ?s4 ?p4 ?o4 >> . ?s4 ?p4 ?o4 }
          OPTIONAL {
            ?sub_compound rtc:subCompoundOf+ ?super_compound .
            OPTIONAL { ?super_compound ?super_compound_p ?super_compound_o . }
          }
        }
        UNION
        {
          <#{compound_id}> rtc:subCompoundOf+ ?super_compound .
          OPTIONAL { ?super_compound ?super_compound_p ?super_compound_o . }
        }
      }
      """
    end

    defp default_opts, do: Application.get_env(:rtc, :from_sparql_opts, [])

    defp from_results(%RDF.Graph{} = graph, compound_id) do
      {:ok, Compound.from_rdf(graph, compound_id)}
    end
  end
end
