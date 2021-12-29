defmodule RTCTest do
  use ExUnit.Case
  doctest RTC

  test "greets the world" do
    assert RTC.hello() == :world
  end
end
