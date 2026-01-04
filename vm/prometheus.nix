{ config, ... }:

{
  httpd.subdomains.prometheus.proxy.port = config.services.prometheus.port;

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
      # {
      #   job_name = "quail";
      #   static_configs = [
      #     {
      #       targets = [ "10.0.2.10:8081" ];
      #     }
      #   ];
      # }
    ];
  };
}
