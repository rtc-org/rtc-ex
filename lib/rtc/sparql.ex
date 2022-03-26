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

    def from_endpoint(endpoint, compound_id, opts \\ []) do
      with {:ok, results} <-
             compound_id
             |> graph_query()
             |> SPARQL.Client.query(endpoint, opts) do
        from_results(results, compound_id)
      end
    end

    defp graph_query(compound_id) do
      compound_id = RDF.iri(compound_id)

      """
      #{@prefixes}
      CONSTRUCT {
        <#{compound_id}>
            ?compound_p ?compound_o ;
            rtc:elements ?triple .
        ?sub_compound
          rtc:subCompoundOf ?super_compound ;
          rtc:elements ?sub_triple ;
          ?sub_compound_p ?sub_compound_o .
      }
      WHERE {
        { ?triple rtc:elementOf <#{compound_id}> . }
        UNION
        { <#{compound_id}> ?compound_p ?compound_o . }
        UNION
        {
          ?sub_compound
            rtc:subCompoundOf+ <#{compound_id}> ;
            rtc:subCompoundOf ?super_compound ;
            ?sub_compound_p ?sub_compound_o .
          OPTIONAL { ?sub_triple rtc:elementOf ?sub_compound . }
        }
      }
      """
    end

    defp from_results(%RDF.Graph{} = graph, compound_id) do
      {:ok, Compound.from_rdf(graph, compound_id)}
    end
  end
end
