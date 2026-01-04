{ config, lib, pkgs, ... }:

with builtins;
with lib;
let
  isEmpty = x: x == null || x == "" || x == [ ] || x == { };
  isNotEmpty = x: ! isEmpty x;
  cfg = config.httpd;
  proxyOptions = {
    options = {
      schema = mkOption {
        type = types.enum [ "http" "https" ];
        default = "http";
      };
      host = mkOption {
        type = types.str;
        default = "localhost";
      };
      port = mkOption {
        type = types.port;
      };
    };
  };
  hostOptions = {
    options = {
      root = mkOption {
        type = types.nullOr types.path;
        default = null;
      };
      proxy = mkOption {
        type = types.nullOr (types.submodule proxyOptions);
        default = null;
      };
      proxyPass = mkOption {
        type = types.nullOr types.str;
        default = null;
      };
    };
  };
in
{
  options.httpd = {
    subdomains = mkOption {
      type = types.attrsOf (types.submodule hostOptions);
      default = { };
    };
    hosts = mkOption {
      type = types.attrsOf (types.submodule hostOptions);
      default = { };
    };
    sslCertificate = mkOption {
      type = types.path;
    };
    sslCertificateKey = mkOption {
      type = types.path;
    };
  };

  config = {
    services.nginx = {
      enable = true;
      package = pkgs.nginxMainline;
      recommendedBrotliSettings = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      virtualHosts =
        let
          subdomains = mapAttrs' (name: nameValuePair "${name}.${config.networking.domain}") cfg.subdomains;
          hosts = subdomains // cfg.hosts;
          mkHost = hostname: host:
            let
              proxyPass =
                if isNotEmpty host.proxyPass
                then host.proxyPass
                else if isNotEmpty host.proxy
                then "${host.proxy.schema}://${host.proxy.host}:${toString host.proxy.port}/"
                else null;
            in
            {
              http2 = true;
              http3 = true;
              quic = true;
              inherit (host) root;
            }
            // lib.optionalAttrs (isNotEmpty proxyPass) {
              locations."/" = {
                inherit proxyPass;
                proxyWebsockets = true;
              };
            };
        in
        mapAttrs mkHost hosts;
    };

    networking.firewall.allowedTCPPorts = [ 80 443 ];
  };
}
