locals_without_parens = []

[
  inputs: ["{mix,.formatter}.exs", "{bench,config,lib,test}/**/*.{ex,exs}"],
  import_deps: [:rdf],
  locals_without_parens: locals_without_parens,
  export: [
    locals_without_parens: locals_without_parens
  ]
]
