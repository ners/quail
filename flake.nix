{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-compose = {
      url = "github:ners/nixos-compose/nixos-25-11";
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
      ghcsFor = pkgs: with lib; foldlAttrs
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
        { }
        pkgs.haskell.packages;
      hpsFor = pkgs: { default = pkgs.haskellPackages; } // ghcsFor pkgs;
      pnames = map (path: baseNameOf (dirOf path)) (lib.fileset.toList (lib.fileset.fileFilter (file: file.hasExt "cabal") ./.));
      isWasmPkgs = haskellPackages: haskellPackages.ghc.targetPrefix == "wasm32-wasi-";
      haskell-overlay = pkgs: lib.composeManyExtensions [
        (inputs.dashi.overlays.haskell pkgs)
        (hfinal: hprev: lib.genAttrs pnames (pname: hfinal.callCabal2nix pname (sourceFilter ./${pname}) { }))
        (hfinal: hprev: with pkgs.haskell.lib.compose; {
          haxl = doJailbreak hprev.haxl;
          co-log-effectful = doJailbreak (unmarkBroken hprev.co-log-effectful);
          morpheus-graphql-code-gen = doJailbreak (unmarkBroken hprev.morpheus-graphql-code-gen);
          proto-lens = doJailbreak hprev.proto-lens;
          proto-lens-runtime = doJailbreak hprev.proto-lens-runtime;
          servant-effectful = hfinal.callCabal2nix "servant-effectful" inputs.servant-effectful { };
          quail-ui = hprev.quail-ui.overrideAttrs (attrs: lib.optionalAttrs (isWasmPkgs hprev) {
            nativeBuildInputs = with pkgs; [
              binaryen
              nodejs
              wasm-tools
            ] ++ attrs.nativeBuildInputs or [ ];
            postInstall = ''
              ${attrs.postInstall or ""}
              cd "$out"
              cp -r "${staticAssets pkgs}" static
              chmod -R +w static
              mv bin/*.wasm static/app.wasm
              rmdir bin
              cd static
              "$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs --input app.wasm --output ghc_wasm_jsffi.js
              # hold @MagicRB accountable for this crime
              sed -i 's/var runBatch = /var initialSyncDepth = 0; &/' ghc_wasm_jsffi.js
              wasm-opt -all -O2 app.wasm -o app.wasm
              wasm-tools strip -o app.wasm app.wasm
              sed -i "s/\?v=0/\?v=$(md5sum app.wasm | cut -d' ' -f1)/" index.html index.js
              cd ..
              mv static/index.html static/favicon.ico static/apple-touch-icon.png .
            '';
            postFixup = ''
              ${attrs.postFixup or ""}
              rm -rf lib nix-support share
            '';
          });
        })
      ];
      favicon = pkgs: pkgs.runCommand "quail-ui/favicon.ico"
        {
          nativeBuildInputs = with pkgs; [
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
      apple-touch-icon = pkgs: pkgs.runCommand "quail-ui/apple-touch-icon.png"
        {
          nativeBuildInputs = with pkgs; [
            librsvg
          ];
        } ''
        rsvg-convert "${./quail-ui/static/icon.svg}" \
          --background-color '#3457D5' \
          --width 180 \
          --output "$out"
      '';
      staticAssets = pkgs: pkgs.runCommand "quail-ui/static" { } ''
        cp -r "${./quail-ui/static}" "$out"
        cd "$out"
        chmod -R +w .

        cp "${inputs.dashi.inputs.mdi-webfont}"/*.woff2 .
        cp "${favicon pkgs}" favicon.ico
        cp "${apple-touch-icon pkgs}" apple-touch-icon.png

        mkdir browser_wasi_shim
        cp -r "${inputs.dashi.legacyPackages.${pkgs.stdenv.hostPlatform.system}.browser_wasi_shim}"/lib/node_modules/*/browser_wasi_shim/dist/*.js browser_wasi_shim
      '';
      overlay = lib.composeManyExtensions [
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (haskell-overlay prev)
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
          hps = hpsFor pkgs;
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system} = {
            default = pkgs.haskellPackages.quail-server.overrideAttrs (attrs: {
              postPatch = ''
                ${attrs.postPatch or ""}
                substituteInPlace app/Quail/Server/Files.hs \
                  --replace-fail "#define ROOT_DIR \".\"" "#define ROOT_DIR \"${wasmPkgs.haskellPackages.quail-ui}\""
              '';
            });
            nixosConfigurations.vm = import ./vm.nix { inherit lib; inherit (pkgs) hostPlatform; };
            wasm = wasmPkgs.haskellPackages.quail-ui;
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
                  inputs.nixos-compose.packages.${system}.default
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
                  ln -s "${staticAssets pkgs}"/* static
                  ln -fs static/index.html static/favicon.ico static/apple-touch-icon.png .
                '';
              };
            });
        }
      );
}
