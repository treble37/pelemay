# Pelemay
**Pelemay = The Penta (Five) “Elemental Way”: Freedom, Insight, Beauty, Efficiency and Robustness**

For example, the following code of the function `map_square` will be compiled to native code using SIMD instructions by Pelemay.

```elixir
defmodule M do
  require Pelemay
  import Pelemay

  defpelemay do
    def map_square (list) do
      list
      |> Enum.map(& &1 * &1)
    end
  end
end
```

## Supported Platforms

Potentially, Pelemay may support any architectures that both Erlang and Clang or GCC are supported.

We've tested it well on the following processor architectures:

* x86_64
* ARM

We've tested it well on the following OS:

* macOS (64bit)
* Linux (64bit)
* Windows (64bit)

We've tested it on the following Elixir versions:

* 1.9

We've tested it on the following OTP versions:

* 22
* 21
* 20

We've tested it on Clang 6 or later and GCC 7 or later.
Potentially, Clang and GCC that supports auto-vectorization can generate native code with SIMD instructions by Pelemay.

Pelemay also supports Nerves.

## Pre-installation

Pelemay requires Clang or GCC.

Environment Variable `CC` is recommended being set the path of the C compiler you want to use.

## Installation

Add `pelemay` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:pelemay, "~> 0.0.5"},
  ]
end
```

Documentation is generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). The docs will
be found at [https://hexdocs.pm/pelemay](https://hexdocs.pm/pelemay).
