{ lib, hostPlatform }:

lib.makeOverridable lib.nixosSystem {
  modules = [
    ./alloy.nix
    ./grafana.nix
    ./httpd.nix
    ./loki.nix
    ./prometheus.nix
    ./tempo.nix

    ({ config, pkgs, ... }: {
      nixpkgs.hostPlatform = hostPlatform.system;
      users.users.root.shell = lib.mkForce pkgs.bash;
      networking.domain = "localhost";

      environment.systemPackages = with pkgs; [
        tcpdump
      ];

      virtualisation = {
        forwardPorts = [
          {
            from = "guest";
            guest = {
              address = "10.0.2.10";
              port = 8081;
            };
            host = {
              address = "127.0.0.1";
              port = 8081;
            };
          }
          {
            from = "host";
            guest.port = config.services.nginx.defaultHTTPListenPort;
            host.port = 3000;
          }
          {
            # OTLP gRPC ingestion
            from = "host";
            guest.port = 4317;
            host.port = 4317;
          }
          {
            # OTLP HTTP ingestion
            from = "host";
            guest.port = 4318;
            host.port = 4318;
          }
        ];
      };
    })
  ];
}
