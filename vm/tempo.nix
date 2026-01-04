{ config, ... }:

{
  httpd.subdomains.tempo.proxy.port = config.services.tempo.settings.server.http_listen_port;

  services.tempo = {
    enable = true;
    settings = {
      server = {
        http_listen_address = "0.0.0.0";
        http_listen_port = 3200;
        grpc_listen_port = 3201;
      };
      distributor.receivers.otlp.protocols = {
        http.endpoint = "localhost:14318";
        grpc.endpoint = "localhost:14317";
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

  services.grafana.provision.datasources.settings.datasources = [
    {
      name = "Tempo";
      type = "tempo";
      uid = "tempo";
      access = "proxy";
      url = "http://tempo.localhost";
      basicAuth = false;
      jsonData = {
        nodeGraph.enabled = true;
        search.hide = false;
        traceQuery = {
          timeShiftEnabled = true;
          spanStartTimeShift = "1h";
          spanEndTimeShift = "1h";
        };
        spanBar = {
          type = "Tag";
          tag = "http.path";
        };
        tracesToLogsV2 = {
          datasourceUid = "loki";
          tags = [
            "job"
            "instance"
            "pod"
            "namespace"
          ];
          filterByTraceID = true;
          filterBySpanID = true;
          customQuery = false;
        };
      };
    }
  ];

  systemd.services.tempo = {
    serviceConfig = {
      StateDirectory = "tempo";
      StateDirectoryMode = "0755";
    };
  };
}
