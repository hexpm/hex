[
  inputs:
    ["*.exs", "config/*.exs", "test/**/*.{ex,exs}"] ++
      (Path.wildcard("lib/**/*.ex") -- Path.wildcard("lib/hex/mint/**/*.ex"))
]
