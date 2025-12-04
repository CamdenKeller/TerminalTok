opam install . --deps-only

In a separate terminal, run:
dune exec bin/server.exe

For each client, run in their own terminal:
dune exec bin/main.exe