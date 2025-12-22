#!/usr/bin/env elixir

# Benchmark script for Aoc25.Day2.process_input with different worker counts

# Load the module
Code.require_file("lib/day2.ex", __DIR__)

# Read the input file
input_file = Path.join([__DIR__, "..", "day2-input"])
input_text = File.read!(input_file)

# Configuration
worker_counts = [1, 2, 4, 8, 16, 32]
iterations = 10

# Benchmark function
defmodule Benchmark do
  def run(input_text, workers, iterations) do
    times =
      for _ <- 1..iterations do
        {time_us, _result} =
          :timer.tc(fn ->
            Aoc25.Day2.process_input(input_text, workers)
          end)

        time_us
      end

    avg_time = Enum.sum(times) / length(times)
    min_time = Enum.min(times)
    max_time = Enum.max(times)

    {avg_time, min_time, max_time}
  end

  def format_time(microseconds) do
    cond do
      microseconds < 1_000 ->
        "#{Float.round(microseconds, 2)} μs"

      microseconds < 1_000_000 ->
        "#{Float.round(microseconds / 1_000, 2)} ms"

      true ->
        "#{Float.round(microseconds / 1_000_000, 2)} s"
    end
  end
end

# Run benchmarks
IO.puts("Benchmarking Aoc25.Day2.process_input")
IO.puts("Input file: #{input_file}")
IO.puts("Iterations per worker count: #{iterations}")
IO.puts(String.duplicate("=", 80))
IO.puts("")

results =
  for workers <- worker_counts do
    IO.write("Running with #{workers} worker(s)... ")
    {avg, min, max} = Benchmark.run(input_text, workers, iterations)
    IO.puts("done")

    {workers, avg, min, max}
  end

# Display results
IO.puts("")
IO.puts(String.duplicate("=", 80))
IO.puts("Results:")
IO.puts(String.duplicate("-", 80))

IO.puts(
  String.pad_trailing("Workers", 10) <>
    String.pad_trailing("Avg Time", 15) <>
    String.pad_trailing("Min Time", 15) <>
    String.pad_trailing("Max Time", 15) <>
    "Speedup"
)

IO.puts(String.duplicate("-", 80))

baseline_time = elem(hd(results), 1)

for {workers, avg, min, max} <- results do
  speedup = baseline_time / avg

  IO.puts(
    String.pad_trailing("#{workers}", 10) <>
      String.pad_trailing(Benchmark.format_time(avg), 15) <>
      String.pad_trailing(Benchmark.format_time(min), 15) <>
      String.pad_trailing(Benchmark.format_time(max), 15) <>
      "#{Float.round(speedup, 2)}x"
  )
end

IO.puts(String.duplicate("=", 80))
