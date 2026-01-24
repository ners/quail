{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-compose = {
      url = "github:garnix-io/nixos-compose";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    servant-effectful = {
      url = "github:Kleidukos/servant-effectful";
      flake = false;
    };
    dashi = {
      url = "github:ners/dashi";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    miso-graphql = {
      url = "github:haskell-miso/miso-graphql";
      flake = false;
    };
    hs-opentelemetry = {
      url = "github:ners/hs-opentelemetry/log-exporter";
      flake = false;
    };
  };

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      sourceFilter = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter
          (file: any file.hasExt [ "cabal" "hs" "md" "gql" ])
          root;
      };
      projects =
        with lib;
        genAttrs'
          (fileset.toList (fileset.fileFilter (file: file.hasExt "cabal") ./.))
          (file: nameValuePair (removeSuffix ".cabal" (baseNameOf file)) (dirOf file));
      pnames = attrNames projects;
      isWasmPkgs = haskellPackages: haskellPackages.ghc.targetPrefix == "wasm32-wasi-";
      haskell-overlay = pkgs: with pkgs.haskell.lib.compose; lib.composeManyExtensions [
        (inputs.dashi.overlays.haskell pkgs)
        (hfinal: hprev: lib.mapAttrs (pname: dir: hfinal.callCabal2nix pname (sourceFilter dir) { }) projects)
        (hfinal: hprev: {
          haxl = doJailbreak hprev.haxl;
          hs-opentelemetry-api = hfinal.callCabal2nix "hs-opentelemetry-api" (inputs.hs-opentelemetry + "/api") { };
          hs-opentelemetry-exporter-otlp = hfinal.callCabal2nix "hs-opentelemetry-exporter-otlp" (inputs.hs-opentelemetry + "/exporters/otlp") { };
          hs-opentelemetry-sdk = hfinal.callCabal2nix "hs-opentelemetry-sdk" (inputs.hs-opentelemetry + "/sdk") { };
          morpheus-graphql-code-gen = doJailbreak (unmarkBroken hprev.morpheus-graphql-code-gen);
          proto-lens = doJailbreak hprev.proto-lens;
          proto-lens-runtime = doJailbreak hprev.proto-lens-runtime;
          miso-graphql = hfinal.callCabal2nix "miso-graphql" inputs.miso-graphql { };
          servant-effectful = hfinal.callCabal2nix "servant-effectful" inputs.servant-effectful { };
          quail-ui = (hprev.quail-ui.overrideAttrs (attrs: {
            postPatch = ''
              ${attrs.postPatch or ""}
              sed -i 's|-DGQL_SCHEMA_FILE=.*|-DGQL_SCHEMA_FILE="${hprev.quail-api.src}/assets/schema.gql"|' *.cabal
            '';
          })) // {
            staticAssets = pkgs.callPackage staticAssets { };
          };
        })
        (hfinal: hprev: lib.optionalAttrs (isWasmPkgs hprev) {
          zlib = addBuildDepend hprev.zlib-clib hprev.zlib;
          quail-ui = appendBuildFlag "--ghc-options=-DGHCJS_BROWSER" hprev.quail-ui // {
            inherit (hprev.${pname}) staticAssets;
            dist = pkgs.runCommand "quail-ui-dist"
              {
                nativeBuildInputs = with pkgs; [
                  binaryen
                  hfinal.ghc
                  nodejs
                  wasm-tools
                  webpack-cli
                ];
              }
              ''
                function compare() {
                  echo "$1: $(numfmt --to=si --suffix=B $2) -> $(numfmt --to=si --suffix=B $3) ($(( $3 * 100 / $2 - 100 ))%)"
                }
                function compress() {
                    f1="$1"
                    shift
                    f2="$1"
                    shift
                    size1="$(cat $f1 | wc -c)"
                    gzip1="$(gzip -c $f1 | wc -c)"
                    eval "$*"
                    size2="$(cat $f2 | wc -c)"
                    gzip2="$(gzip -c $f2 | wc -c)"
                    compare $f2 $size1 $size2
                    compare $f2.gz $gzip1 $gzip2
                }
                mkdir -p "$out"
                cd "$out"
                cp -r "${hprev.quail-ui.staticAssets}" static
                cd static
                chmod -R +w .
                cp "${hfinal.quail-ui}/bin"/*.wasm app.wasm
                "$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs --input app.wasm --output ghc_wasm_jsffi.js

                chmod +w app.wasm
                compress app.wasm{,} "wasm-opt -all -O2 -o app.wasm{,} ; wasm-tools strip -o app.wasm{,}"
                sed -i "s/\?v=0/\?v=$(md5sum app.wasm | cut -d' ' -f1)/" index.html main.js

                substituteInPlace ghc_wasm_jsffi.js --replace-fail "node:timers" timers
                entries="./main.js ./ghc_wasm_jsffi.js ./browser_wasi_shim/*.js"
                compress "$entries" main.js webpack --config "${pkgs.writeText "webpack.config.js" /*javascript*/ ''
                  module.exports = {
                    resolve: {
                      fallback: {
                        timers: false, // do not include a polyfill for node:timers
                      },
                    },
                  };
                ''}" --mode production --output-path . --entry $entries
                rm -fr ghc_wasm_jsffi.js browser_wasi_shim
                cd ..
                mv static/{index.html,favicon.ico,apple-touch-icon.png} .
              '';
          };
        })
      ];
      staticAssets =
        { stdenv
        , runCommand
        , imagemagick
        , librsvg
        }:
        let
          favicon = runCommand "favicon.ico"
            {
              nativeBuildInputs = [
                imagemagick
                librsvg
              ];
            } ''
            tmpPng="$(mktemp --suffix=.png)"
            rsvg-convert "${./quail-ui/static/icon.svg}" \
              --width 64 \
              --output "$tmpPng"
            convert "$tmpPng" -define icon:auto-resize=64,48,32,16 "$out"
            rm "$tmpPng"
          '';
          apple-touch-icon = runCommand "apple-touch-icon.png"
            {
              nativeBuildInputs = [
                librsvg
              ];
            } ''
            rsvg-convert "${./quail-ui/static/icon.svg}" \
              --background-color '#3457D5' \
              --width 180 \
              --output "$out"
          '';
        in
        runCommand "quail-static-assets" { } ''
          cp -r "${inputs.dashi.legacyPackages.${stdenv.hostPlatform.system}.dashi.staticAssets}" "$out"
          cd "$out"
          chmod -R +w .
          rm 404.html dashi.svg *.ftl
          cp -fr "${./quail-ui/static}"/* .

          cp "${favicon}" favicon.ico
          cp "${apple-touch-icon}" apple-touch-icon.png
        '';
      overlay = lib.composeManyExtensions [
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (haskell-overlay final)
            ];
          };
        })
      ];
    in
    {
      overlays = {
        default = overlay;
        haskell = haskell-overlay;
      };
    }
    //
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          wasmPkgs' = inputs.dashi.inputs.nix-wasm.legacyPackages.${system};
          wasmPkgs = wasmPkgs' // {
            haskellPackages = wasmPkgs'.haskellPackages.extend (haskell-overlay pkgs');
          };
          hps = with lib; foldlAttrs
            (acc: name: hp':
              let
                hp = tryEval hp';
                version = getVersion hp.value.ghc;
                majorMinor = versions.majorMinor version;
                ghcName = "ghc${replaceStrings ["."] [""] majorMinor}";
              in
              if hp.value ? ghc && ! acc ? ${ghcName} && versionAtLeast version "9.4" && versionOlder version "9.13"
              then acc // { ${ghcName} = hp.value; }
              else acc
            )
            { default = pkgs.haskellPackages; }
            pkgs.haskell.packages;
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs // {
            inherit wasmPkgs;
          };
          packages.${system} = rec {
            default = pkgs.haskellPackages.quail-server.overrideAttrs (attrs: {
              postPatch = ''
                ${attrs.postPatch or ""}
                substituteInPlace app/Quail/Server/Files.hs \
                  --replace-fail "#define ROOT_DIR \".\"" "#define ROOT_DIR \"${wasm}\""
              '';
            });
            nixosConfigurations.vm = import ./vm { inherit lib; inherit (pkgs) hostPlatform; };
            wasm = wasmPkgs.haskellPackages.quail-ui.dist;
          };
          devShells.${system} =
            foreach hps (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: map (pname: ps.${pname}) pnames;
                nativeBuildInputs = with pkgs'; with haskellPackages; [
                  cabal-gild
                  cabal-install
                  fourmolu
                  ghcid
                  hp.haskell-language-server
                  nixpkgs-fmt
                  nixos-shell
                ];
                env = {
                  OTEL_EXPORTER_OTLP_ENDPOINT = "http://localhost:4318";
                  OTEL_TRACES_EXPORTER = "otlp";
                };
                shellHook = ''
                  cd quail-ui
                  find static -type l -delete
                  ln -s "${hp.quail-ui.staticAssets}"/* static
                  ln -s static/index.html static/favicon.ico static/apple-touch-icon.png .
                '';
              };
            });
        }
      );
}
