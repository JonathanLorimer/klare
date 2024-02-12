{s}: 
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:klare' --allow-eval --warnings";
  testScript = s "test" "cabal run test:klare-tests";
  hoogleScript = s "hgl" "hoogle serve";
  record = s "rec" ''
    cabal build exe:klare
    apitrace trace ./dist-newstyle/build/x86_64-linux/ghc-9.6.3/klare-0.0.0.0/x/klare/build/klare/klare
    apitrace dump-images -o - klare.trace | ffmpeg -r 30 -f image2pipe -c:v ppm -i pipe: -c:v mpeg4 -y wiener.mp4
    rm -rf klare.trace
  '';
}
