{ config, ... }:

{
  httpd.subdomains.loki.proxy.port = config.services.loki.configuration.server.http_listen_port;

  services.loki = {
    enable = true;
    configuration = {
      auth_enabled = false;
      server = {
        http_listen_address = "0.0.0.0";
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

  services.grafana.provision.datasources.settings.datasources = [
    {
      name = "Loki";
      type = "loki";
      uid = "loki";
      access = "proxy";
      url = "http://loki.localhost";
      basicAuth = false;
      jsonData = {
        timeout = 60;
        maxLines = 1000;
        derivedFields = let valueRaw = "$\${__value.raw}"; in [
          {
            name = "Trace";
            matcherType = "label";
            matcherRegex = "trace_id";
            url = valueRaw;
            datasourceUid = "tempo";
            urlDisplayLabel = "Trace: ${valueRaw}";
          }
          {
            name = "Span";
            matcherType = "label";
            matcherRegex = "span_id";
            url = valueRaw;
            datasourceUid = "tempo";
            urlDisplayLabel = "Span: ${valueRaw}";
          }
        ];
      };
    }
  ];
}
