defmodule RTC.MixProject do
  use Mix.Project

  @scm_url "https://github.com/rtc-org/rtc-ex"

  @version File.read!("VERSION") |> String.trim()

  def project do
    [
      app: :rtc,
      version: @version,
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      deps: deps(),
      aliases: aliases(),
      preferred_cli_env: ["test.sparql_client": :test],

      # Dialyzer
      dialyzer: dialyzer(),

      # Hex
      package: package(),
      description: description(),

      # Docs
      name: "RTC.ex",
      docs: [
        main: "RTC",
        source_url: @scm_url,
        source_ref: "v#{@version}",
        extras: ["CHANGELOG.md"]
      ],

      # ExCoveralls
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        check: :test,
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test,
        earl_reports: :test
      ]
    ]
  end

  defp description do
    """
    An implementation of RDF Triple Compounds (RTC) in Elixir.
    """
  end

  defp package do
    [
      maintainers: ["Marcel Otto"],
      licenses: ["MIT"],
      links: %{
        "GitHub" => @scm_url,
        "Spec" => "https://rtc-org.github.io/spec",
        "Guide" => "https://rdf-elixir.dev/rtc-ex",
        "Changelog" => @scm_url <> "/blob/main/CHANGELOG.md"
      },
      files: ~w[lib priv mix.exs .formatter.exs VERSION *.md]
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      rdf_ex_dep(:rdf, "~> 1.1.1"),
      rdf_ex_dep(:sparql_client, "~> 0.4", optional: true),
      {:elixir_uuid, "~> 1.2"},
      {:dialyxir, "~> 1.1", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.26", only: :dev, runtime: false},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:excoveralls, "~> 0.16", only: :test},
      {:benchee, "~> 1.0", only: :dev}
    ]
  end

  defp rdf_ex_dep(dep, version, opts \\ nil) do
    case System.get_env("RDF_EX_PACKAGES_SRC") do
      "LOCAL" -> {dep, path: "../../../RDF.ex/src/#{dep}"}
      _ -> if opts, do: {dep, version, opts}, else: {dep, version}
    end
  end

  defp dialyzer do
    [
      plt_add_apps: [:mix],
      plt_file: {:no_warn, "priv/plts/dialyzer.plt"},
      ignore_warnings: ".dialyzer_ignore"
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp aliases do
    [
      "test.sparql_client": "test --only sparql_client"
    ]
  end
end
