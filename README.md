Quick start:

```
stack run -- config.ini
```

Configuration:

 | Option | Description |
 |--------|-------------|
 | `server.host`, `server.port` | Listener address (optional) and port |
 | `server.tls_certificate`, `server.tls_certificate_key` | TLS certificate and key used by server (currently unused but required) |

Development certificates (those in [tls](tls) directory) can be downloaded from https://invoices-form.randomforest.ee/tls.

Password of [admin.encrypted.pfx](tls/admin.encrypted.pfx) is `Passw0rd`.
