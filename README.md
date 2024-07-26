# HermiCrab
Takes place of the shell!

### Quick pitch
- A consitent, extensible shell

### Goals
- [ ] Rich TUI
- [ ] Seamless Language
- [ ] Good ecosystem integration
  - [ ] Package Manager
  - [ ] POSIX Detector

### Building

- Clone the repo and init the switch
```sh
git clone https://github.com/glyh/hmc
cd hmc
opam switch create . --deps-only --with-test -y
```
- For developing, you may want to have LSP and other stuffs available
```sh
opam install --switch=. -y ocamlformat ocaml-lsp-server
```
- Update the environment, for example if you're on bash: 
```bash
eval $(opam env)
```
- Build and run the package
```sh
dune exec hmc
```
