# Installation Instructions

This guide assumes you have OCaml, `opam`, and `dune` installed on your system.

## 1. System Dependencies

TerminalTok requires a few system libraries and tools to function correctly, specifically for video playback and cryptography.

### macOS (Homebrew)
```bash
brew install mpv gmp zlib pkg-config
```

### Linux (Ubuntu/Debian)
```bash
sudo apt-get install mpv libgmp-dev zlib1g-dev pkg-config
```

## 2. OCaml Dependencies

Install the required OCaml packages using `opam`.

```bash
opam install . --deps-only
```

Alternatively, you can install them manually:
```bash
opam install yojson lwt zarith cryptokit batteries csv ounit2 lwt_ppx ppx_inline_test bisect_ppx
```

## 3. Build the Project

Run the following command to build the project:

```bash
dune build
```

## 4. Running TerminalTok

TerminalTok consists of a central server and multiple clients.

### Step 1: Start the Server
In a dedicated terminal window, run:
```bash
dune exec bin/server.exe
```
*Note: The server must be running for Online Mode (Chat, ASCII, and Videos) to work.*

### Step 2: Start the Client
In a separate terminal window (you can open multiple for different users), run:
```bash
dune exec bin/main.exe
```

## 5. Running Tests

To run the test suite:
```bash
dune test
```

To run tests with coverage reporting:
```bash
dune runtest --instrument-with bisect_ppx --force && bisect-ppx-report summary && bisect-ppx-report html
```