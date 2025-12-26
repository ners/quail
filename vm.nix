{ lib, hostPlatform }:

lib.makeOverridable lib.nixosSystem {
  modules = [
    ({ config, pkgs, ... }: {
      nixpkgs.hostPlatform = hostPlatform.system;
      users.users.root.shell = lib.mkForce pkgs.bash;

      environment.systemPackages = with pkgs; [
        tcpdump
      ];

      services.nginx = {
        enable = true;
        recommendedProxySettings = true;
        virtualHosts =
          with lib;
          foldr recursiveUpdate { }
            (mapAttrsToList
              (service: port: {
                "${service}.localhost".locations."/".proxyPass = "http://localhost:${toString port}";
              })
              {
                grafana = config.services.grafana.settings.server.http_port;
                prometheus = config.services.prometheus.port;
                loki = config.services.loki.configuration.server.http_listen_port;
                tempo = config.services.tempo.settings.server.http_listen_port;
              });
      };

      services.grafana = {
        enable = true;
        settings.analytics.reporting_enabled = false;
      };

      services.prometheus = {
        enable = true;
        globalConfig.scrape_interval = "1s";
        scrapeConfigs = [
          #{
          #  job_name = "prometheus";
          #  static_configs = [
          #    {
          #      targets = [ "localhost:${toString config.services.prometheus.port}" ];
          #    }
          #  ];
          #}
          {
            job_name = "quail";
            static_configs = [
              {
                targets = [ "10.0.2.10:8081" ];
              }
            ];
          }
        ];
      };

      services.loki = {
        enable = true;
        configuration = {
          auth_enabled = false;
          server = {
            http_listen_port = 3100;
            grpc_listen_port = 3101;
          };
          common = {
            path_prefix = config.services.loki.dataDir;
            storage.filesystem = {
              chunks_directory = "${config.services.loki.dataDir}/chunks";
              rules_directory = "${config.services.loki.dataDir}/rules";
            };
            replication_factor = 1;
            ring = {
              instance_addr = "127.0.0.1";
              kvstore = {
                store = "inmemory";
              };
            };
          };
          schema_config.configs = [
            {
              from = "2025-01-01";
              store = "tsdb";
              object_store = "filesystem";
              schema = "v13";
              index = {
                prefix = "index_";
                period = "24h";
              };
            }
          ];
        };
      };

      services.tempo = {
        enable = true;
        settings = {
          server = {
            http_listen_port = 3200;
            grpc_listen_port = 3201;
          };
          distributor = {
            receivers = {
              otlp = {
                protocols = {
                  http = {
                    endpoint = "0.0.0.0:4318";
                  };
                  grpc = {
                    endpoint = "0.0.0.0:4317";
                  };
                };
              };
            };
          };
          storage = {
            trace = {
              backend = "local";
              local = {
                path = "/var/lib/tempo";
              };
              wal = {
                path = "/var/lib/tempo/wal";
              };
            };
          };
        };
      };

      systemd.services.tempo = {
        serviceConfig = {
          StateDirectory = "tempo";
          StateDirectoryMode = "0755";
        };
      };

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
