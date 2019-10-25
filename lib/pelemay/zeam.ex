defmodule Pelemay.Zeam do
  import NimbleParsec

  defcombinatorp(
    :integer,
    integer(min: 1)
    |> post_traverse(:match_and_emit_integer)
  )

  defp match_and_emit_integer(_rest, [value], context, _line, _offset) do
    {[value], context}
  end

  defcombinatorp(
    :macro,
    ascii_string([?A..?Z, ?0..?9, ?_], min: 1)
    |> post_traverse(:match_and_emit_macro)
  )

  defp match_and_emit_macro(_rest, [variable], context, _line, _offset) do
    {[{String.to_atom(variable), [], :macro}], context}
  end

  defcombinatorp(
    :variable,
    ascii_string([?a..?z, ?A..?Z, ?0..?9, ?_], min: 1)
    |> post_traverse(:match_and_emit_variable)
  )

  defp match_and_emit_variable(_rest, [variable], context, _line, _offset) do
    {[{String.to_atom(variable), [], :c_var}], context}
  end

  defcombinatorp(
    :factor,
    choice([
      ignore(ascii_char([?(]))
      |> ignore(repeat(ascii_char([?\s])))
      |> concat(parsec(:expression))
      |> ignore(repeat(ascii_char([?\s])))
      |> ignore(ascii_char([?)])),
      parsec(:integer),
      parsec(:macro),
      parsec(:variable)
    ])
  )

  defcombinatorp(
    :term,
    choice([
      parsec(:factor)
      |> ignore(repeat(ascii_char([?\s])))
      |> ignore(ascii_char([?*]))
      |> ignore(repeat(ascii_char([?\s])))
      |> concat(parsec(:term))
      |> tag(:*)
      |> post_traverse(:match_and_emit_mul),
      parsec(:factor)
    ])
  )

  defp match_and_emit_mul(_rest, [*: children], context, _line, _offset) do
    {[
       {:*, [], children}
     ], context}
  end

  defcombinatorp(
    :enif_make_badarg,
    string("enif_make_badarg")
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> ignore(string("("))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> parsec(:variable)
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> ignore(string(")"))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> post_traverse(:match_and_emit_enif_make_badarg)
  )

  defp match_and_emit_enif_make_badarg(_rest, [val, "enif_make_badarg"], context, _line, _offset) do
    {[
       {:enif_make_badarg, [], val}
     ], context}
  end

  defcombinatorp(
    :enif_make_int64,
    string("enif_make_int64")
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> ignore(string("("))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> parsec(:variable)
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> ignore(string(","))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> parsec(:expression)
    |> ignore(string(")"))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> post_traverse(:match_and_emit_enif_make_int64)
  )

  defp match_and_emit_enif_make_int64(
         _rest,
         [value, env, "enif_make_int64"],
         context,
         _line,
         _offset
       ) do
    {[
       {:enif_make_int64, [], [env, value]}
     ], context}
  end

  defcombinatorp(
    :enif_get_int64,
    string("enif_get_int64")
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> ignore(string("("))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> parsec(:variable)
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> ignore(string(","))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> parsec(:expression)
    |> ignore(string(","))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> ignore(string("&"))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> parsec(:expression)
    |> ignore(string(")"))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> post_traverse(:match_and_emit_enif_get_int64)
  )

  defp match_and_emit_enif_get_int64(
         _rest,
         [var2, var1, env, "enif_get_int64"],
         context,
         _line,
         _offset
       ) do
    {[
       {:enif_get_int64, [], [env, var1, var2]}
     ], context}
  end

  defcombinatorp(
    :expression,
    choice([
      parsec(:enif_make_badarg),
      parsec(:enif_make_int64),
      parsec(:enif_get_int64),
      parsec(:term)
      |> ignore(repeat(ascii_char([?\s])))
      |> ignore(ascii_char([?+]))
      |> ignore(repeat(ascii_char([?\s])))
      |> concat(parsec(:expression))
      |> tag(:+)
      |> post_traverse(:match_and_emit_plus),
      parsec(:term)
      |> ignore(repeat(ascii_char([?\s])))
      |> ignore(ascii_char([?-]))
      |> ignore(repeat(ascii_char([?\s])))
      |> concat(parsec(:expression))
      |> tag(:-)
      |> post_traverse(:match_and_emit_minus),
      parsec(:term)
    ])
  )

  defp match_and_emit_plus(_rest, [+: children], context, _line, _offset) do
    {[
       {:+, [], children}
     ], context}
  end

  defp match_and_emit_minus(_rest, [-: children], context, _line, _offset) do
    {[
       {:-, [], children}
     ], context}
  end

  defcombinatorp(
    :return,
    choice([
      string("return")
      |> ignore(repeat(ascii_char([?\s, ?\r])))
      |> parsec(:expression)
      |> ignore(repeat(ascii_char([?\s, ?\r])))
      |> ignore(ascii_char([?;]))
      |> ignore(repeat(ascii_char([?\s, ?\r]))),
      string("return")
      |> ignore(repeat(ascii_char([?\s, ?\r])))
    ])
    |> post_traverse(:match_and_emit_return)
  )

  defp match_and_emit_return(_rest, ["return"], context, _line, _offset) do
    {[
       {:return, [], []}
     ], context}
  end

  defp match_and_emit_return(_rest, [expression, "return"], context, _line, _offset) do
    {[
       {:return_with_value, [], [expression]}
     ], context}
  end

  defcombinatorp(
    :statement,
    choice([
      parsec(:return),
      parsec(:expression)
      |> ignore(repeat(ascii_char([?\s, ?\r])))
      |> ignore(ascii_char([?;]))
      |> ignore(repeat(ascii_char([?\s, ?\r])))
    ])
  )

  defcombinatorp(
    :block,
    ignore(ascii_char([?{]))
    |> ignore(repeat(ascii_char([?\s, ?\r])))
    |> repeat(parsec(:statement))
    |> ignore(repeat(ascii_char([?\s, ?\r])))
    |> ignore(ascii_char([?}]))
    |> post_traverse(:match_and_emit_block)
  )

  defp match_and_emit_block(_rest, statements, context, _line, _offset) do
    {[statements], context}
  end

  defcombinatorp(
    :include,
    ignore(string("#"))
    |> concat(string("include"))
    |> ignore(repeat(ascii_char([?\s])))
    |> concat(
      choice([
        string("<")
        |> ascii_string([?a..?z, ?A..?Z, ?0..?9, ?_, ?.], min: 1)
        |> string(">"),
        string("\"")
        |> ascii_string([?a..?z, ?A..?Z, ?0..?9, ?_, ?.], min: 1)
        |> string("\"")
      ])
    )
    |> post_traverse(:match_and_emit_include)
  )

  defp match_and_emit_include(_rest, [">", header, "<", "include"], context, _line, _offset) do
    {[{:include, [], header}], context}
  end

  defp match_and_emit_include(_rest, ["\"", header, "\"", "include"], context, _line, _offset) do
    {[{:include_cd, [], header}], context}
  end

  defcombinatorp(
    :define,
    ignore(string("#"))
    |> concat(string("define"))
    |> ignore(repeat(ascii_char([?\s])))
    |> concat(ascii_string([?a..?z, ?A..?Z, ?0..?9, ?_, ?.], min: 1))
    |> ignore(repeat(ascii_char([?\s])))
    |> parsec(:expression)
    |> post_traverse(:match_and_emit_define)
  )

  defp match_and_emit_define(_rest, [rval, lval, "define"], context, _line, _offset) do
    {[
       {:define, [], [{:=, [], [{String.to_atom(lval), [], :macro}, rval]}]}
     ], context}
  end

  defcombinatorp(
    :defunc,
    ignore(string("static"))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> concat(string("ERL_NIF_TERM"))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> concat(ascii_string([?a..?z, ?A..?Z, ?0..?9, ?_, ?.], min: 1))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> ignore(string("("))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> ignore(string("ErlNifEnv"))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> ignore(string("*"))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> concat(ascii_string([?a..?z, ?A..?Z, ?0..?9, ?_, ?.], min: 1))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> ignore(string(","))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> ignore(string("int"))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> concat(ascii_string([?a..?z, ?A..?Z, ?0..?9, ?_, ?.], min: 1))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> ignore(string(","))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> ignore(string("const"))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> ignore(string("ERL_NIF_TERM"))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> choice([
      ignore(string("*"))
      |> concat(ascii_string([?a..?z, ?A..?Z, ?0..?9, ?_, ?.], min: 1)),
      ascii_string([?a..?z, ?A..?Z, ?0..?9, ?_, ?.], min: 1)
      |> ignore(string("[]"))
    ])
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> ignore(string(")"))
    |> ignore(repeat(ascii_char([?\s, ?\n])))
    |> concat(parsec(:block))
    |> post_traverse(:match_and_emit_defunc)
  )

  defp match_and_emit_defunc(
         _rest,
         [block, argv, argc, env, function, "ERL_NIF_TERM"],
         context,
         _line,
         _offset
       ) do
    {[
       {:defunc, [],
        [
          {String.to_atom(function), [context: Nif],
           [
             {String.to_atom(env), [], :c_var},
             {String.to_atom(argc), [], :c_var},
             {String.to_atom(argv), [], :c_var}
           ]},
          [do: block]
        ]}
     ], context}
  end

  defparsec(
    :clang,
    choice([
      parsec(:include),
      parsec(:define),
      parsec(:defunc)
    ])
  )

  @doc """
    ## Examples

    iex> Pelemay.Zeam.map_to_zeam_ir(["#include <stdio.h>"]) |> Enum.to_list
    [{:include, [], "stdio.h"}]
  """
  def map_to_zeam_ir(c_src_stream) do
    c_src_stream
    |> Stream.map(&String.trim/1)
    |> Stream.map(&to_zeam_ir/1)
  end

  defp to_zeam_ir(str) do
    {:ok, [result], "", %{}, _, _} = clang(str)
    result
  end

  @doc """
  	## Examples
  	```
  	["#include <stdio.h>"]
  	|> Pelemay.Zeam.write("/path/to/file")
  	```
  """
  def write(c_src_stream, path) do
    c_src_stream
    |> Stream.into(File.stream!(path), &"#{&1}\n")
    |> Stream.run()
  end

  @doc """
    ## Examples

    iex> Pelemay.Zeam.map_to_clang([{:include, [], "stdio.h"}]) |> Enum.to_list
    ["#include <stdio.h>"]
  """
  def map_to_clang(zeam_ir_stream) do
    zeam_ir_stream
    |> Stream.map(&to_clang/1)
  end

  defp to_clang({:include, _env, header}) do
    "#include <#{header}>"
  end

  defp to_clang({:include_cd, _env, header}) do
    "#include \"#{header}\""
  end

  defp to_clang({:define, _env, [{:=, _, [{lval, [], :macro}, rval]}]}) do
    "#define #{Atom.to_string(lval)} #{rval}"
  end
end
