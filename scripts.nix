{s}: 
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:klare' --allow-eval --warnings";
  testScript = s "test" "cabal run test:klare-tests";
  hoogleScript = s "hgl" "hoogle serve";
}
