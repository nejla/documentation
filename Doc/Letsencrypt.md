# Using Letsencrypt to secure your (nginx) web server

This document describes how to create and renew [Let's Encrypt](https://letsencrypt.org/) certificates with
certbot.  The idea is to configure the web server to serve ACME http challenges
so we don't need to stop it to renew certificates.

## Steps:

* Create and mount a directory or volume into your webserver and serve it under `/.well-known/acme-challenge/` for http (port 80)
* Create and mount a directory or volume into your webserver for the certificates

Example:

* `mkdir -p letsencrypt-challenge`
* `mkdir -p certs`


in docker-compose.yaml
```yaml
services:
  proxy:
    image: nginx
    volumes:
      - ./certs:/etc/nginx/certs:ro
      - ./letsencrypt-challenge:/letsencrypt-challenge:ro
    <...>

```

* Configure nginx to serve the challenge directory

in nginx.conf

```conf
http {
  <...>
  server {
    server_name <my server name>;
    listen 80;
    location /.well-known/acme-challenge/ {
      root /letsencrypt-challenge/;
    }
    <...>
  }
}
```

* Start the webserver

* Run certbot to create certificate for `$domain` (don't forget to set `$email`)


```bash
docker run --rm \
       -v "$PWD/letsencrypt/certs":/etc/letsencrypt \
       -v "$PWD/letsencrypt/logs":/var/log/letsencrypt \
       -v "$PWD/letsencrypt-challenge":/webroot \
       certbot/certbot certonly \
       --noninteractive \
       --agree-tos --email "$email" \
       --webroot -w /webroot\
       -d "$domain"
```

* Copy the certificate files from `letsencrypt/certs/live` to `certs`. Don't
  volume `letsencrypt/certs/live` directly since it only contains symlinks that
  would be broken.

* Configure nginx to use the newly created certificates and restart

``` conf
http {
  <...>
  server {
    server_name <my server name>;
    listen 443 ssl;
    server_name avancera.app;
    ssl_certificate certs/<domain>.crt;
    ssl_certificate_key certs/<domain>.key;
  }
}
```

### To update the certificates (don't forget to set $email):

``` bash
docker run --rm \
       -v "$PWD/letsencrypt/certs":/etc/letsencrypt \
       -v "$PWD/letsencrypt/logs":/var/log/letsencrypt \
       -v "$PWD/letsencrypt-challenge":/webroot \
       certbot/certbot renew \
       --noninteractive \
       --agree-tos --email "$email"
```

* Replace the certificates under `certs` with the new ones in `letsencrypt/certs/live`
* Reload nginx

```bash
docker exec <container> nginx -s reload
```
