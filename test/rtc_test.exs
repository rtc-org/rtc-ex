defmodule RTCTest do
  use ExUnit.Case

  doctest RTC

  test "id/0" do
    assert %RDF.BlankNode{} = RTC.id()
  end
end
