defmodule Pluscode.MixProject do
  use Mix.Project

  def project do
    [
      app: :pluscode,
      version: "0.1.0",
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      description: description(),
      package: package(),
      deps: deps(),
      name: "Pluscode",
      docs: [
        main: "README",
        extras: ["README.md"],
        source_url: "https://github.com/dkuku/pluscode_elixir"
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:styler, "~> 1.0", only: :dev},
      {:ex_doc, "~> 0.37", only: :dev, runtime: false}
    ]
  end

  defp description do
    """
    An Elixir implementation of Open Location Code (Plus Codes).

    Plus Codes are short, easy to share location codes that can be used instead of street addresses.
    This library provides functions to encode/decode coordinates, validate codes, and handle shortened codes.
    """
  end

  defp package do
    [
      name: "pluscode",
      files: ["lib", "mix.exs", "README.md", "LICENSE"],
      maintainers: ["Daniel Kukula"],
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" => "https://github.com/dkuku/pluscode_elixir",
        "Plus Codes" => "https://plus.codes"
      }
    ]
  end
end
