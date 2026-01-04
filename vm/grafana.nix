{ config, ... }:

{
  httpd.subdomains.grafana.proxy.port = config.services.grafana.settings.server.http_port;

  services.grafana = {
    enable = true;
    provision.enable = true;
    settings.analytics.reporting_enabled = false;
  };
}
