{ config, ... }:

{
  services.alloy = {
    enable = true;
    extraFlags = [
      "--disable-reporting"
      "--server.http.listen-addr=0.0.0.0:${toString config.httpd.subdomains.alloy.proxy.port}"
    ];
  };
  httpd.subdomains.alloy.proxy.port = 9902;
  environment.etc."alloy/config.alloy".text =
    let
      tempoProtocols = config.services.tempo.settings.distributor.receivers.otlp.protocols;
      lokiConfig = config.services.loki.configuration;
      # mimirServer = config.services.mimir.configuration.server;
    in
    # Note: This may look like hocon, but looks can be deceiving! Alloy uses its own custom format.
      # hocon
    ''
      livedebugging {
        enabled = true
      }

      otelcol.receiver.otlp "localhost" {
        grpc {
          endpoint = "0.0.0.0:4317"
        }
        http {
          endpoint = "0.0.0.0:4318"
        }
        output {
            // metrics = [otelcol.processor.batch.default.input]
            logs    = [otelcol.processor.batch.default.input]
            traces  = [otelcol.processor.batch.default.input]
        }
      }

      otelcol.processor.batch "default" {
        output {
            // metrics = [otelcol.exporter.prometheus.mimir.input]
            logs    = [otelcol.exporter.loki.default.input]
            traces  = [otelcol.exporter.otlp.tempo.input]
        }
      }

      otelcol.exporter.loki "default" {
        forward_to = [loki.write.default.receiver]
      }

      loki.write "default" {
        endpoint {
          url = "http://localhost:${toString lokiConfig.server.http_listen_port}/loki/api/v1/push"
        }
      }

      otelcol.exporter.otlp "tempo" {
        client {
          endpoint = "${tempoProtocols.grpc.endpoint}"
          tls {
            insecure = true
            insecure_skip_verify = true
          }
        }
      }
    '';
}
